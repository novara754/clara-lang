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

pub fn generate_executable<P>(_o_filepath: P, program: &CheckedProgram) -> eyre::Result<()> {
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
            b"generic\0".as_ptr() as *const _,
            b"\0".as_ptr() as *const _,
            LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        let target_data_layout = llvm::target_machine::LLVMCreateTargetDataLayout(target_machine);

        let context = llvm::core::LLVMContextCreate();

        let module = llvm::core::LLVMModuleCreateWithName(b"program\0".as_ptr() as *const _);
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
                b"./build/out.o\0".as_ptr() as *mut _,
                LLVMCodeGenFileType::LLVMObjectFile,
                error_message.as_mut_ptr(),
            ) != 0
            {
                llvm::core::LLVMDisposeMessage(error_message.assume_init());
            }
        }

        llvm::core::LLVMDumpModule(module);
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
            .collect();
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
            .collect();
        let return_type = type_to_llvm(ctx, &func.return_type);
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
            .collect();
        let return_type = type_to_llvm(ctx, &func.return_type);
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
            b"\0".as_ptr() as *const _,
        );
        llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

        for (param_idx, param) in func.parameters.iter().enumerate() {
            let param_type_ref = type_to_llvm(ctx, &param.ttype);
            let param_storage = llvm::core::LLVMBuildAlloca(
                ctx.builder,
                param_type_ref,
                b"\0".as_ptr() as *const _,
            );
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
            emit_expression(ctx, expr)?;
        }
        CheckedStatement::LetAssign(variable_name, value_expr) => {
            let value = emit_expression(ctx, value_expr)?;
            let var_type = type_to_llvm(ctx, &value_expr.ttype());
            let var =
                llvm::core::LLVMBuildAlloca(ctx.builder, var_type, b"\0".as_ptr() as *const _);
            llvm::core::LLVMBuildStore(ctx.builder, value, var);
            ctx.scope_stack.add_variable(variable_name.clone(), var);
        }
        CheckedStatement::IfElse(if_else) => {
            let if_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                b"if_block\0".as_ptr() as *const _,
            );
            let else_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                b"else_block\0".as_ptr() as *const _,
            );
            let after_if_else = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                b"after_if_else\0".as_ptr() as *const _,
            );

            let condition = emit_expression(ctx, &if_else.condition)?;
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
                b"condition_block\0".as_ptr() as *const _,
            );
            let loop_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                b"loop_block\0".as_ptr() as *const _,
            );
            let after_loop_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                b"after_loop_block\0".as_ptr() as *const _,
            );

            llvm::core::LLVMBuildBr(ctx.builder, condition_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, condition_block);
            let condition = emit_expression(ctx, &while_loop.condition)?;
            llvm::core::LLVMBuildCondBr(ctx.builder, condition, loop_block, after_loop_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, loop_block);
            emit_block(ctx, &while_loop.body, loop_block)?;
            llvm::core::LLVMBuildBr(ctx.builder, condition_block);

            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, after_loop_block);
        }
        CheckedStatement::Return(return_value) => {
            let return_value = emit_expression(ctx, return_value)?;
            llvm::core::LLVMBuildRet(ctx.builder, return_value);

            let unreachable_block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.context,
                ctx.current_function.unwrap(),
                b"unreachable_block\0".as_ptr() as *const _,
            );
            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, unreachable_block);
        }
    }

    Ok(())
}

unsafe fn emit_expression(
    ctx: &mut EmitContext,
    expression: &CheckedExpression,
) -> eyre::Result<LLVMValueRef> {
    Ok(match expression {
        CheckedExpression::Literal(literal) => match literal {
            CheckedLiteral::Int(value, ttype) => {
                let int_type = type_to_llvm(ctx, ttype);
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
                let str = llvm::core::LLVMAddGlobal(
                    ctx.module,
                    str_type,
                    b"string_literal\0".as_ptr() as *const _,
                );
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
                let struct_type_ref = type_to_llvm(ctx, struct_type);

                let mut field_values: Vec<LLVMValueRef> = r#struct
                    .fields
                    .iter()
                    .map(|(declared_field_name, _)| {
                        let field_value = struct_literal
                            .fields
                            .get(declared_field_name.as_str())
                            .expect("existence of field value was established by typechecker");
                        emit_expression(ctx, field_value)
                    })
                    .collect::<eyre::Result<_>>()?;

                llvm::core::LLVMConstNamedStruct(
                    struct_type_ref,
                    field_values.as_mut_ptr(),
                    field_values.len().try_into()?,
                )
            }
        },
        CheckedExpression::FunctionCall(func_call) => {
            let &(callee, callee_type) = ctx.known_functions.get(&func_call.name).unwrap();
            let mut args: Vec<LLVMValueRef> = func_call
                .args
                .iter()
                .map(|expr| emit_expression(ctx, expr))
                .collect::<eyre::Result<_>>()?;
            llvm::core::LLVMBuildCall2(
                ctx.builder,
                callee_type,
                callee,
                args.as_mut_ptr(),
                args.len().try_into()?,
                b"function_call\0".as_ptr() as *const _,
            )
        }
        CheckedExpression::CompareOp(lhs, rhs, op, _type) => {
            let lhs = emit_expression(ctx, lhs)?;
            let rhs = emit_expression(ctx, rhs)?;
            let predicate = match op {
                CompareOperation::GreaterThan => LLVMIntPredicate::LLVMIntSGT,
                CompareOperation::Equality => LLVMIntPredicate::LLVMIntEQ,
                CompareOperation::GreaterThanEqual => LLVMIntPredicate::LLVMIntSGE,
                CompareOperation::LessThan => LLVMIntPredicate::LLVMIntSLT,
                CompareOperation::LessThanEqual => LLVMIntPredicate::LLVMIntSLE,
            };
            llvm::core::LLVMBuildICmp(
                ctx.builder,
                predicate,
                lhs,
                rhs,
                b"bin_op\0".as_ptr() as *const _,
            )
        }
        CheckedExpression::MathOp(lhs, rhs, op, ttype) => {
            assert!(
                ttype.is_integer_type(),
                "Codegen for MathOp for non-ints not implemented"
            );

            let lhs = emit_expression(ctx, lhs)?;
            let rhs = emit_expression(ctx, rhs)?;
            match op {
                MathOperation::Addition => {
                    llvm::core::LLVMBuildAdd(ctx.builder, lhs, rhs, b"\0".as_ptr() as *const _)
                }
            }
        }
        CheckedExpression::Variable(variable_name, _type) => llvm::core::LLVMBuildLoad(
            ctx.builder,
            ctx.scope_stack.get_variable(variable_name),
            b"\0".as_ptr() as *const _,
        ),
        CheckedExpression::FieldAccess(field_access, r#struct, _struct_type) => {
            // let struct_type_ref = type_to_llvm(ctx, struct_type);
            let field_index = r#struct
                .fields
                .iter()
                .position(|(declared_field_name, _)| {
                    declared_field_name == &field_access.field_name
                })
                .expect("existence of field in field access was established by typechecker");
            let object = emit_expression(ctx, &field_access.object)?;

            let object_type = type_to_llvm(ctx, &field_access.object.ttype());
            let object_storage =
                llvm::core::LLVMBuildAlloca(ctx.builder, object_type, b"\0".as_ptr() as *const _);

            llvm::core::LLVMBuildStore(ctx.builder, object, object_storage);

            let field_ptr = llvm::core::LLVMBuildStructGEP(
                ctx.builder,
                // struct_type_ref,
                object_storage,
                field_index.try_into()?,
                b"\0".as_ptr() as *const _,
            );

            llvm::core::LLVMBuildLoad(ctx.builder, field_ptr, b"\0".as_ptr() as *const _)
        }
    })
}

unsafe fn type_to_llvm(ctx: &mut EmitContext, ttype: &Type) -> *mut LLVMType {
    match ttype {
        Type::Pointer(subtype) => {
            let subtype = type_to_llvm(ctx, subtype);
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
        Type::Incomplete => panic!("attempted to use incomplete type in llvm codegen"),
    }
}
