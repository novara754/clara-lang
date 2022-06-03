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
    parser::BinaryOperation,
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
    current_block: Option<*mut LLVMBasicBlockRef>,
    known_functions: HashMap<String, (*mut LLVMValue, *mut LLVMType)>,
    scope_stack: ScopeStack,
}

pub fn generate_executable(program: &CheckedProgram) -> eyre::Result<()> {
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
                current_block: None,
                known_functions: HashMap::new(),
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
    let void = llvm::core::LLVMVoidTypeInContext(ctx.context);

    for func in &program.extern_functions {
        let mut params: Vec<_> = func
            .parameters
            .iter()
            .map(|param| type_to_llvm(ctx, &param.ttype))
            .collect();
        let function_type =
            llvm::core::LLVMFunctionType(void, params.as_mut_ptr(), params.len().try_into()?, 0);
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
        let function_type = llvm::core::LLVMFunctionType(void, std::ptr::null_mut(), 0, 0);
        let function = llvm::core::LLVMAddFunction(
            ctx.module,
            CString::new(func.name.as_str())?.as_ptr(),
            function_type,
        );
        llvm::core::LLVMSetFunctionCallConv(function, LLVMCallConv::LLVMCCallConv as u32);

        ctx.known_functions
            .insert(func.name.clone(), (function, function_type));

        ctx.current_function = Some(function);
        emit_block(ctx, &func.body, b"entry\0")?;
        ctx.current_function.take();
        llvm::core::LLVMBuildRetVoid(ctx.builder);
    }

    Ok(())
}

unsafe fn emit_block(
    ctx: &mut EmitContext,
    block: &CheckedBlock,
    name: &'static [u8],
) -> eyre::Result<LLVMBasicBlockRef> {
    let bb = llvm::core::LLVMAppendBasicBlockInContext(
        ctx.context,
        ctx.current_function.unwrap(),
        name.as_ptr() as *const _,
    );
    llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.current_block = Some(bb);
    ctx.scope_stack.push_scope();
    for stmt in &block.statements {
        emit_statement(ctx, stmt)?;
    }
    ctx.scope_stack.pop_scope();
    ctx.current_block = None;

    Ok(bb)
}

unsafe fn emit_statement(
    ctx: &mut EmitContext,
    statement: &CheckedStatement,
    bb: LLVMBasicBlockRef,
) -> eyre::Result<()> {
    match statement {
        CheckedStatement::Expression(expr) => {
            emit_expression(ctx, expr, bb)?;
        }
        CheckedStatement::LetAssign(variable_name, value_expr) => {
            let value = emit_expression(ctx, value_expr, bb)?;
            let var_type = type_to_llvm(ctx, &value_expr.ttype());
            let var = llvm::core::LLVMBuildAlloca(
                ctx.builder,
                var_type,
                CString::new(variable_name.as_str())?.as_ptr(),
            );
            llvm::core::LLVMBuildStore(ctx.builder, value, var);
            ctx.scope_stack.add_variable(variable_name.clone(), var);
        }
        CheckedStatement::IfElse(if_else) => {
            let condition = emit_expression(ctx, &if_else.condition, bb)?;
            let if_block = emit_block(ctx, &if_else.if_body, b"then\0")?;
            // let else_block = emit_block(ctx, &if_else.else_body)?;
            // llvm::core::LLVMBuildCondBr(ctx.builder, condition, if_block, else_block);
        }
        _ => todo!(),
    }

    Ok(())
}

unsafe fn emit_expression(
    ctx: &mut EmitContext,
    expression: &CheckedExpression,
    bb: LLVMBasicBlockRef,
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
            _ => todo!(),
        },
        CheckedExpression::FunctionCall(func_call) => {
            let &(callee, callee_type) = ctx.known_functions.get(&func_call.name).unwrap();
            let mut args: Vec<LLVMValueRef> = func_call
                .args
                .iter()
                .map(|expr| emit_expression(ctx, expr, bb))
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
        CheckedExpression::BinaryOp(lhs, rhs, op, _type) => {
            let lhs = emit_expression(ctx, lhs, bb)?;
            let rhs = emit_expression(ctx, rhs, bb)?;
            let predicate = match op {
                BinaryOperation::GreaterThan => LLVMIntPredicate::LLVMIntSGT,
                _ => todo!(),
            };
            llvm::core::LLVMBuildICmp(
                ctx.builder,
                predicate,
                lhs,
                rhs,
                b"bin_op".as_ptr() as *const _,
            )
        }
        CheckedExpression::Variable(variable_name, _type) => {
            ctx.scope_stack.get_variable(variable_name)
        }
        _ => todo!(),
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
        _ => todo!(),
    }
}
