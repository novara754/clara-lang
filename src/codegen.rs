use std::{collections::HashMap, ffi::CString, mem::MaybeUninit};

use llvm::{
    prelude::{LLVMBasicBlockRef, LLVMBuilderRef, LLVMValueRef},
    target_machine::{
        LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMRelocMode, LLVMTargetRef,
    },
    LLVMCallConv, LLVMContext, LLVMIntPredicate, LLVMLinkage, LLVMModule, LLVMType, LLVMValue,
};
use llvm_sys as llvm;

use crate::{
    parser::{CompareOperation, MathOperation},
    typechecker::{
        CheckedBlock, CheckedExpression, CheckedLiteral, CheckedProgram, CheckedStatement, Type,
    },
};

macro_rules! c_str {
    ($s:literal) => {
        concat_bytes!($s, b'\0').as_ptr() as *const _
    };
    (mut $s:literal) => {{
        let mut byte_str = *concat_bytes!($s, b'\0');
        byte_str.as_mut_ptr() as *mut _
    }};
}

struct ScopeStack {
    scopes: Vec<HashMap<String, LLVMValueRef>>,
}

impl ScopeStack {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn pop_scope(&mut self) {
        assert!(self.scopes.pop().is_some());
    }

    fn add_variable(&mut self, name: String, r#ref: LLVMValueRef) {
        assert!(self
            .scopes
            .last_mut()
            .unwrap()
            .insert(name, r#ref)
            .is_none());
    }

    fn get_variable(&self, name: &str) -> LLVMValueRef {
        for scope in self.scopes.iter().rev() {
            if let Some(&r#ref) = scope.get(name) {
                return r#ref;
            }
        }

        panic!("existence of variable was previously established in typechecker");
    }
}

struct EmitContext {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: LLVMBuilderRef,
    current_function: Option<*mut LLVMValue>,
    known_functions: HashMap<String, (*mut LLVMValue, *mut LLVMType)>,
    known_structs: HashMap<String, *mut LLVMType>,
    scope_stack: ScopeStack,
}

#[derive(Debug)]
enum ExprEmitAs {
    LValue,
    RValue,
}

pub fn generate_executable<P>(
    _o_filepath: P,
    program: &CheckedProgram,
    print_llir: bool,
) -> eyre::Result<()> {
    unsafe {
        llvm::target::LLVM_InitializeAllTargetInfos();
        llvm::target::LLVM_InitializeAllTargets();
        llvm::target::LLVM_InitializeAllTargetMCs();
        llvm::target::LLVM_InitializeAllAsmParsers();
        llvm::target::LLVM_InitializeAllAsmPrinters();

        let target_triple = llvm::target_machine::LLVMGetDefaultTargetTriple();
        let target = {
            let mut target: MaybeUninit<LLVMTargetRef> = MaybeUninit::uninit();
            let _ = llvm::target_machine::LLVMGetTargetFromTriple(
                target_triple,
                target.as_mut_ptr(),
                std::ptr::null_mut(),
            );
            target.assume_init()
        };
        let target_machine = llvm::target_machine::LLVMCreateTargetMachine(
            target,
            target_triple,
            c_str!(b"generic"),
            c_str!(b""),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        let target_data_layout = llvm::target_machine::LLVMCreateTargetDataLayout(target_machine);

        let context = llvm::core::LLVMContextCreate();

        let module = llvm::core::LLVMModuleCreateWithName(c_str!(b"program"));
        llvm::target::LLVMSetModuleDataLayout(module, target_data_layout);
        llvm::core::LLVMSetTarget(module, target_triple);

        let builder = llvm::core::LLVMCreateBuilderInContext(context);

        emit_program(
            &mut EmitContext {
                context,
                module,
                builder,
                current_function: None,
                known_functions: HashMap::new(),
                known_structs: HashMap::new(),
                scope_stack: ScopeStack { scopes: vec![] },
            },
            program,
        )?;

        {
            let mut error_message: MaybeUninit<*mut i8> = MaybeUninit::uninit();
            if llvm::target_machine::LLVMTargetMachineEmitToFile(
                target_machine,
                module,
                c_str!(mut b"./build/out.o"),
                LLVMCodeGenFileType::LLVMObjectFile,
                error_message.as_mut_ptr(),
            ) != 0
            {
                llvm::core::LLVMDisposeMessage(error_message.assume_init());
            }
        }

        if print_llir {
            llvm::core::LLVMDumpModule(module);
        }
        llvm::core::LLVMDisposeBuilder(builder);
        llvm::core::LLVMDisposeModule(module);
        llvm::core::LLVMContextDispose(context);
    }

    Ok(())
}

unsafe fn emit_program(ctx: &mut EmitContext, program: &CheckedProgram) -> eyre::Result<()> {
    for struc in &program.structs {
        let mut fields: Vec<_> = struc
            .fields
            .iter()
            .map(|field| type_to_llvm(ctx, &field.1))
            .collect::<eyre::Result<_>>()?;

        let struct_type = llvm::core::LLVMStructCreateNamed(
            ctx.context,
            CString::new(struc.name.as_str())?.as_ptr(),
        );

        if !struc.is_opaque {
            llvm::core::LLVMStructSetBody(
                struct_type,
                fields.as_mut_ptr() as *mut _,
                fields.len().try_into()?,
                0,
            );
        }

        assert!(ctx
            .known_structs
            .insert(struc.name.clone(), struct_type)
            .is_none());
    }

    for func in &program.extern_functions {
        let mut params: Vec<_> = func
            .parameters
            .iter()
            .map(|param| type_to_llvm(ctx, &param.ttype))
            .collect::<eyre::Result<_>>()?;
        let return_type = type_to_llvm(ctx, &func.return_type)?;
        let function_type = llvm::core::LLVMFunctionType(
            return_type,
            params.as_mut_ptr(),
            params.len().try_into()?,
            0,
        );
        let function = llvm::core::LLVMAddFunction(
            ctx.module,
            CString::new(func.name.as_str())?.as_ptr(),
            function_type,
        );
        llvm::core::LLVMSetFunctionCallConv(function, LLVMCallConv::LLVMCCallConv as u32);

        ctx.known_functions
            .insert(func.name.clone(), (function, function_type));
    }

    for func in &program.functions {
        let mut params: Vec<_> = func
            .parameters
            .iter()
            .map(|param| type_to_llvm(ctx, &param.ttype))
            .collect::<eyre::Result<_>>()?;
        let return_type = type_to_llvm(ctx, &func.return_type)?;
        let function_type = llvm::core::LLVMFunctionType(
            return_type,
            params.as_mut_ptr(),
            params.len().try_into()?,
            0,
        );
        let function = llvm::core::LLVMAddFunction(
            ctx.module,
            CString::new(func.name.as_str())?.as_ptr(),
            function_type,
        );
        llvm::core::LLVMSetFunctionCallConv(function, LLVMCallConv::LLVMCCallConv as u32);

        ctx.known_functions
            .insert(func.name.clone(), (function, function_type));

        ctx.scope_stack.push_scope();
        ctx.current_function = Some(function);

        let bb = llvm::core::LLVMAppendBasicBlockInContext(
            ctx.context,
            ctx.current_function.unwrap(),
            c_str!(b""),
        );
        llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

        for (param_idx, param) in func.parameters.iter().enumerate() {
            let param_type_ref = type_to_llvm(ctx, &param.ttype)?;
            let param_storage =
                llvm::core::LLVMBuildAlloca(ctx.builder, param_type_ref, c_str!(b""));
            llvm::core::LLVMBuildStore(
                ctx.builder,
                llvm::core::LLVMGetParam(function, param_idx.try_into()?),
                param_storage,
            );
            ctx.scope_stack
                .add_variable(param.name.clone(), param_storage);
        }

        emit_block(ctx, &func.body, bb)?;

        llvm::core::LLVMBuildRetVoid(ctx.builder);

        ctx.current_function.take();
        ctx.scope_stack.pop_scope();
    }

    Ok(())
}

unsafe fn emit_block(
    ctx: &mut EmitContext,
    block: &CheckedBlock,
    bb: LLVMBasicBlockRef,
) -> eyre::Result<()> {
    llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.scope_stack.push_scope();
    for stmt in &block.statements {
        emit_statement(ctx, stmt)?;
    }
    ctx.scope_stack.pop_scope();

    Ok(())
}

unsafe fn emit_statement(ctx: &mut EmitContext, statement: &CheckedStatement) -> eyre::Result<()> {
    match statement {
        CheckedStatement::Expression(expr) => {
            emit_expression(ctx, expr, ExprEmitAs::RValue)?;
        }
        CheckedStatement::LetAssign(variable_name, value_expr) => {
            let var_type = type_to_llvm(ctx, &value_expr.ttype())?;
            let var = llvm::core::LLVMBuildAlloca(ctx.builder, var_type, c_str!(b""));
            let value = emit_expression(ctx, value_expr, ExprEmitAs::RValue)?;
            llvm::core::LLVMBuildStore(ctx.builder, value, var);
            ctx.scope_stack.add_variable(variable_name.clone(), var);
        }
        CheckedStatement::IfElse(if_else) => {
            let if_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"if_block"),
            );
            let else_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"else_block"),
            );
            let after_if_else = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"after_if_else"),
            );

            let condition = emit_expression(ctx, &if_else.condition, ExprEmitAs::RValue)?;
            llvm::core::LLVMBuildCondBr(ctx.builder, condition, if_block, else_block);

            emit_block(ctx, &if_else.if_body, if_block)?;
            llvm::core::LLVMBuildBr(ctx.builder, after_if_else);

            emit_block(ctx, &if_else.else_body, else_block)?;
            llvm::core::LLVMBuildBr(ctx.builder, after_if_else);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, after_if_else);
        }
        CheckedStatement::WhileLoop(while_loop) => {
            let condition_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"condition_block"),
            );
            let loop_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"loop_block"),
            );
            let after_loop_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"after_loop_block"),
            );

            llvm::core::LLVMBuildBr(ctx.builder, condition_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, condition_block);
            let condition = emit_expression(ctx, &while_loop.condition, ExprEmitAs::RValue)?;
            llvm::core::LLVMBuildCondBr(ctx.builder, condition, loop_block, after_loop_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, loop_block);
            emit_block(ctx, &while_loop.body, loop_block)?;
            llvm::core::LLVMBuildBr(ctx.builder, condition_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, after_loop_block);
        }
        CheckedStatement::ForInLoop(for_in) => {
            ctx.scope_stack.push_scope();

            let elem_var_storage = llvm::core::LLVMBuildAlloca(
                ctx.builder,
                type_to_llvm(ctx, &for_in.elem_var_type)?,
                c_str!(b"elem"),
            );
            ctx.scope_stack
                .add_variable(for_in.elem_var_name.clone(), elem_var_storage);

            let index_var_storage = llvm::core::LLVMBuildAlloca(
                ctx.builder,
                type_to_llvm(ctx, &Type::Int)?,
                c_str!(b"idx"),
            );
            if let Some(index_var_name) = &for_in.index_var {
                ctx.scope_stack
                    .add_variable(index_var_name.clone(), index_var_storage);
            }
            llvm::core::LLVMBuildStore(
                ctx.builder,
                llvm::core::LLVMConstInt(type_to_llvm(ctx, &Type::Int)?, 0, 0),
                index_var_storage,
            );

            let iterable = emit_expression(ctx, &for_in.iterable, ExprEmitAs::LValue)?;
            let iterable_elem_ptr = llvm::core::LLVMBuildBitCast(
                ctx.builder,
                iterable,
                type_to_llvm(
                    ctx,
                    &Type::Pointer(Box::new(for_in.elem_var_type.clone()), false),
                )?,
                c_str!(b"iterable_ptr"),
            );

            let condition_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"condition_block"),
            );
            let loop_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"loop_block"),
            );
            let after_loop_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"after_loop_block"),
            );

            llvm::core::LLVMBuildBr(ctx.builder, condition_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, condition_block);
            let current_idx =
                llvm::core::LLVMBuildLoad(ctx.builder, index_var_storage, c_str!(b"current_idx"));

            let array_len = if let Type::Array(_, array_len) = for_in.iterable.ttype() {
                llvm::core::LLVMConstInt(type_to_llvm(ctx, &Type::Int)?, array_len.try_into()?, 0)
            } else {
                panic!("non-array in for-in loop iterable");
            };

            let condition = llvm::core::LLVMBuildICmp(
                ctx.builder,
                LLVMIntPredicate::LLVMIntSLT,
                current_idx,
                array_len,
                c_str!(b""),
            );
            llvm::core::LLVMBuildCondBr(ctx.builder, condition, loop_block, after_loop_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, loop_block);
            {
                let elem_ptr = llvm::core::LLVMBuildGEP(
                    ctx.builder,
                    iterable_elem_ptr,
                    [current_idx].as_mut_ptr(),
                    1,
                    c_str!(b""),
                );
                let elem = llvm::core::LLVMBuildLoad(ctx.builder, elem_ptr, c_str!(b""));
                llvm::core::LLVMBuildStore(ctx.builder, elem, elem_var_storage);
            }
            emit_block(ctx, &for_in.body, loop_block)?;
            {
                let const_1 = llvm::core::LLVMConstInt(type_to_llvm(ctx, &Type::Int)?, 1, 0);
                let new_idx =
                    llvm::core::LLVMBuildAdd(ctx.builder, current_idx, const_1, c_str!(b""));
                llvm::core::LLVMBuildStore(ctx.builder, new_idx, index_var_storage);
            }
            llvm::core::LLVMBuildBr(ctx.builder, condition_block);

            ctx.scope_stack.pop_scope();

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, after_loop_block);
        }
        CheckedStatement::Return(return_value) => {
            let return_value = emit_expression(ctx, return_value, ExprEmitAs::RValue)?;
            llvm::core::LLVMBuildRet(ctx.builder, return_value);

            let unreachable_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                c_str!(b"unreachable_block"),
            );
            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, unreachable_block);
        }
    }

    Ok(())
}

unsafe fn emit_expression(
    ctx: &mut EmitContext,
    expression: &CheckedExpression,
    emit_as: ExprEmitAs,
) -> eyre::Result<LLVMValueRef> {
    let value_ref = match expression {
        CheckedExpression::Literal(literal) => match literal {
            CheckedLiteral::Int(value, ttype) => {
                let int_type = type_to_llvm(ctx, ttype)?;
                llvm::core::LLVMConstInt(int_type, *value as u64, 1)
            }
            CheckedLiteral::Bool(value, _type) => {
                let value = if *value { 1 } else { 0 };
                llvm::core::LLVMConstInt(llvm::core::LLVMInt1TypeInContext(ctx.context), value, 0)
            }
            CheckedLiteral::String(value, _type) => {
                let bytes = value.as_bytes();
                let i8 = llvm::core::LLVMInt8TypeInContext(ctx.context);
                let str_type = llvm::core::LLVMArrayType(i8, (bytes.len() + 1).try_into()?);
                let str =
                    llvm::core::LLVMAddGlobal(ctx.module, str_type, c_str!(b"string_literal"));
                llvm::core::LLVMSetLinkage(str, LLVMLinkage::LLVMInternalLinkage);

                let mut str_bytes: Vec<_> = bytes
                    .iter()
                    .map(|&b| llvm::core::LLVMConstInt(i8, b as u64, 0))
                    .collect();

                str_bytes.push(llvm::core::LLVMConstInt(i8, 0, 0));

                llvm::core::LLVMSetInitializer(
                    str,
                    llvm::core::LLVMConstArray(
                        i8,
                        str_bytes.as_mut_ptr(),
                        str_bytes.len().try_into()?,
                    ),
                );

                str
            }
            CheckedLiteral::Struct(struct_literal, r#struct, struct_type) => {
                let struct_type_ref = type_to_llvm(ctx, struct_type)?;

                let mut field_values: Vec<LLVMValueRef> = r#struct
                    .fields
                    .iter()
                    .map(|(declared_field_name, _)| {
                        let field_value = struct_literal
                            .fields
                            .get(declared_field_name.as_str())
                            .expect("existence of field value was established by typechecker");
                        emit_expression(ctx, field_value, ExprEmitAs::RValue)
                    })
                    .collect::<eyre::Result<_>>()?;

                llvm::core::LLVMConstNamedStruct(
                    struct_type_ref,
                    field_values.as_mut_ptr(),
                    field_values.len().try_into()?,
                )
            }
            CheckedLiteral::Array(array_literal, _array_type) => {
                let element_type = array_literal
                    .element_type
                    .as_ref()
                    .expect("emitting generic arrays is not supported");

                let mut elements: Vec<_> = array_literal
                    .elements
                    .iter()
                    .map(|elem| emit_expression(ctx, elem, ExprEmitAs::RValue))
                    .collect::<eyre::Result<_>>()?;

                llvm::core::LLVMConstArray(
                    type_to_llvm(ctx, element_type)?,
                    elements.as_mut_ptr(),
                    elements.len().try_into()?,
                )
            }
        },
        CheckedExpression::FunctionCall(func_call) => {
            let &(callee, callee_type) = ctx.known_functions.get(&func_call.name).unwrap();
            let mut args: Vec<LLVMValueRef> = func_call
                .args
                .iter()
                .map(|expr| emit_expression(ctx, expr, ExprEmitAs::RValue))
                .collect::<eyre::Result<_>>()?;
            llvm::core::LLVMBuildCall2(
                ctx.builder,
                callee_type,
                callee,
                args.as_mut_ptr(),
                args.len().try_into()?,
                c_str!(b"function_call"),
            )
        }
        CheckedExpression::CompareOp(lhs, rhs, op, _type) => {
            let lhs = emit_expression(ctx, lhs, ExprEmitAs::RValue)?;
            let rhs = emit_expression(ctx, rhs, ExprEmitAs::RValue)?;
            let predicate = match op {
                CompareOperation::GreaterThan => LLVMIntPredicate::LLVMIntSGT,
                CompareOperation::Equality => LLVMIntPredicate::LLVMIntEQ,
                CompareOperation::GreaterThanEqual => LLVMIntPredicate::LLVMIntSGE,
                CompareOperation::LessThan => LLVMIntPredicate::LLVMIntSLT,
                CompareOperation::LessThanEqual => LLVMIntPredicate::LLVMIntSLE,
            };
            llvm::core::LLVMBuildICmp(ctx.builder, predicate, lhs, rhs, c_str!(b"bin_op"))
        }
        CheckedExpression::MathOp(lhs, rhs, op, ttype) => {
            assert!(
                ttype.is_integer_type(),
                "Codegen for MathOp for non-ints not implemented"
            );

            let lhs = emit_expression(ctx, lhs, ExprEmitAs::RValue)?;
            let rhs = emit_expression(ctx, rhs, ExprEmitAs::RValue)?;
            match op {
                MathOperation::Addition => {
                    llvm::core::LLVMBuildAdd(ctx.builder, lhs, rhs, c_str!(b""))
                }
                MathOperation::Subtraction => {
                    llvm::core::LLVMBuildSub(ctx.builder, lhs, rhs, c_str!(b""))
                }
                MathOperation::Multiplication => {
                    llvm::core::LLVMBuildMul(ctx.builder, lhs, rhs, c_str!(b""))
                }
                MathOperation::Division => {
                    llvm::core::LLVMBuildSDiv(ctx.builder, lhs, rhs, c_str!(b""))
                }
            }
        }
        CheckedExpression::Variable(variable_name, _type, _is_mut) => {
            let var_ref = ctx.scope_stack.get_variable(variable_name);
            match emit_as {
                ExprEmitAs::RValue => llvm::core::LLVMBuildLoad(ctx.builder, var_ref, c_str!(b"")),
                ExprEmitAs::LValue => return Ok(var_ref),
            }
        }
        CheckedExpression::FieldAccess(field_access, r#struct, _struct_type) => {
            // let struct_type_ref = type_to_llvm(ctx, struct_type);
            let field_index = r#struct
                .fields
                .iter()
                .position(|(declared_field_name, _)| {
                    declared_field_name == &field_access.field_name
                })
                .expect("existence of field in field access was established by typechecker");

            // let object_type = type_to_llvm(ctx, &field_access.object.ttype())?;
            // let object_storage = llvm::core::LLVMBuildAlloca(ctx.builder, object_type, c_str!(b""));
            let mut object = emit_expression(ctx, &field_access.object, ExprEmitAs::LValue)?;

            if field_access.object_is_ptr {
                object = llvm::core::LLVMBuildLoad(ctx.builder, object, c_str!(b""));
            }

            let field_ptr = llvm::core::LLVMBuildStructGEP(
                ctx.builder,
                // struct_type_ref,
                object,
                field_index.try_into()?,
                c_str!(b""),
            );

            match emit_as {
                ExprEmitAs::RValue => {
                    llvm::core::LLVMBuildLoad(ctx.builder, field_ptr, c_str!(b""))
                }
                ExprEmitAs::LValue => return Ok(field_ptr),
            }
        }
        CheckedExpression::Assignment(lhs, rhs) => {
            let destination = emit_expression(ctx, lhs, ExprEmitAs::LValue)?;
            let value = emit_expression(ctx, rhs, ExprEmitAs::RValue)?;
            llvm::core::LLVMBuildStore(ctx.builder, value, destination);
            value
        }
        CheckedExpression::PointerTo(pointer_to) => {
            emit_expression(ctx, &pointer_to.inner, ExprEmitAs::LValue)?
        }
        CheckedExpression::Deref(deref) => {
            let pointer = emit_expression(ctx, &deref.inner, ExprEmitAs::RValue)?;
            match emit_as {
                ExprEmitAs::LValue => pointer,
                ExprEmitAs::RValue => llvm::core::LLVMBuildLoad(ctx.builder, pointer, c_str!(b"")),
            }
        }
    };

    Ok(value_ref)
}

unsafe fn type_to_llvm(ctx: &mut EmitContext, ttype: &Type) -> eyre::Result<*mut LLVMType> {
    Ok(match ttype {
        Type::Pointer(subtype, _is_mut) => {
            let subtype = type_to_llvm(ctx, subtype)?;
            llvm::core::LLVMPointerType(subtype, 0)
        }
        Type::CChar => llvm::core::LLVMInt8TypeInContext(ctx.context),
        Type::GenericInt | Type::Int | Type::CInt => {
            llvm::core::LLVMInt32TypeInContext(ctx.context)
        }
        Type::Unit => llvm::core::LLVMVoidTypeInContext(ctx.context),
        Type::UserDefined(name) => *ctx
            .known_structs
            .get(name)
            .expect("user defined type should exist as determined by typechecker"),
        Type::String => todo!(),
        Type::Bool => llvm::core::LLVMInt1TypeInContext(ctx.context),
        Type::GenericEmptyArray => todo!(),
        Type::Array(element_type, size) => {
            llvm::core::LLVMArrayType(type_to_llvm(ctx, element_type)?, (*size).try_into()?)
        }
        Type::Incomplete => panic!("attempted to use incomplete type in llvm codegen"),
    })
}
