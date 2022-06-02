use std::collections::HashMap;

use ariadne::{Color, Label, Report, ReportKind};

use crate::{
    error::ReportError,
    parser::{
        BinaryOperation, FunctionParameter, Literal, ParsedBlock, ParsedExpression, ParsedProgram,
        ParsedStatement, ParsedStruct,
    },
    span::{Span, Spanned},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UserDefined(String),
    Pointer(Box<Type>),
    GenericInt,
    String,
    Int,
    Bool,
    Unit,
    CChar,
    CInt,
    Incomplete,
}

impl Type {
    pub fn is_integer_type(&self) -> bool {
        matches!(self, Self::GenericInt | Self::CInt | Self::Int)
    }

    pub fn matches(&self, other: &Type) -> bool {
        if self == other {
            true
        } else {
            match (self, other) {
                (Self::GenericInt, _) => other.is_integer_type(),
                (_, Self::GenericInt) => self.is_integer_type(),
                _ => false,
            }
        }
    }

    pub fn from_str(typename: &str) -> Self {
        match typename {
            "string" => Type::String,
            "int" => Type::Int,
            "bool" => Type::Bool,
            "c_char" => Type::CChar,
            "c_int" => Type::CInt,
            "()" => Type::Unit,
            _ => Type::UserDefined(typename.to_string()),
        }
    }

    pub fn to_str(&self) -> String {
        match self {
            Self::GenericInt => "{integer}".to_string(),
            Self::Pointer(ty) => format!("->{}", ty.to_str()),
            Self::String => "string".to_string(),
            Self::Int => "int".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Unit => "unit".to_string(),
            Self::CChar => "c_char".to_string(),
            Self::CInt => "c_int".to_string(),
            Self::Incomplete => "incomplete type".to_string(),
            Self::UserDefined(name) => name.clone(),
        }
    }

    pub fn to_c(&self) -> String {
        match self {
            // TODO: Maybe handle this case better?
            Self::GenericInt => {
                panic!("attempted to instantiate generic integer type in c codegen")
            }
            Self::Pointer(ty) => format!("{}*", ty.to_str()),
            Self::String => "string".to_string(),
            Self::Int => "int".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Unit => "unit".to_string(),
            Self::CChar => "c_char".to_string(),
            Self::CInt => "c_int".to_string(),
            Self::Incomplete => "incomplete type".to_string(),
            Self::UserDefined(name) => name.clone(),
        }
    }
}

pub enum TypeCheckError {
    WrongNumArgs(Span, usize, usize),
    WrongArgType(Span, Type, Type),
    WrongConditionType(Span, Type),
    UnknownFunction(String, Span),
    UnknownVariable(String, Option<String>, Span),
    BinaryOpMismatch(Type, Type, Span, Span),
}

impl ReportError for TypeCheckError {
    fn report(&self) -> Report<Span> {
        match *self {
            Self::WrongNumArgs(span, actual, expected) => {
                Report::build(ReportKind::Error, (), span.start)
                    .with_message("incorrect number of arguments to function call")
                    .with_label(Label::new(span).with_color(Color::Red))
                    .with_note(format!(
                        "function expects {} arguments but {} were provided",
                        expected, actual
                    ))
            }
            Self::WrongArgType(span, ref actual, ref expected) => {
                Report::build(ReportKind::Error, (), span.start)
                    .with_message("incorrect argument type in function call")
                    .with_label(
                        Label::new(span)
                            .with_color(Color::Red)
                            .with_message(format!(
                                "argument has type {} but function expects {}",
                                actual.to_str(),
                                expected.to_str()
                            )),
                    )
            }
            Self::UnknownFunction(ref function_name, span) => {
                Report::build(ReportKind::Error, (), span.start)
                    .with_message(format!("reference to unknown function `{}`", function_name))
                    .with_label(Label::new(span).with_color(Color::Red))
            }
            Self::UnknownVariable(ref variable_name, ref function_name, span) => {
                let report = Report::build(ReportKind::Error, (), span.start);
                if let Some(function_name) = function_name {
                    report.with_message(format!(
                        "reference to unknown variable `{}` in function `{}`",
                        variable_name, function_name
                    ))
                } else {
                    report
                        .with_message(format!("reference to unknown variable `{}`", variable_name))
                }
                .with_label(Label::new(span).with_color(Color::Red))
            }
            Self::WrongConditionType(span, ref actual) => {
                Report::build(ReportKind::Error, (), span.start)
                    .with_message("incorrect type in condition")
                    .with_label(
                        Label::new(span)
                            .with_message(format!("expression has type {}", actual.to_str()))
                            .with_color(Color::Red),
                    )
                    .with_note(format!(
                        "expression in condition has to be of type {}",
                        Type::Bool.to_str()
                    ))
            }
            Self::BinaryOpMismatch(ref lhs_type, ref rhs_type, lhs_span, rhs_span) => {
                Report::build(ReportKind::Error, (), lhs_span.start)
                    .with_message("type mismatch in binary operator")
                    .with_label(
                        Label::new(lhs_span)
                            .with_message(format!("left operand has type {}", lhs_type.to_str()))
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new(rhs_span)
                            .with_message(format!("right operand has type {}", rhs_type.to_str()))
                            .with_color(Color::Red),
                    )
                    .with_note("Both sides of the operator need to have the same type")
            }
        }
        .finish()
    }
}

#[derive(Debug)]
pub struct CheckedFunctionCall {
    pub name: String,
    pub args: Vec<CheckedExpression>,
    pub ttype: Type,
}

#[derive(Debug)]
pub enum CheckedExpression {
    Literal(Literal, Type),
    FunctionCall(CheckedFunctionCall),
    Variable(String, Type),
    BinaryOp(
        Box<CheckedExpression>,
        Box<CheckedExpression>,
        BinaryOperation,
        Type,
    ),
}

impl CheckedExpression {
    pub fn ttype(&self) -> Type {
        match self {
            Self::Literal(_literal, ttype) => ttype.clone(),
            Self::FunctionCall(func_call) => func_call.ttype.clone(),
            Self::Variable(_name, ttype) => ttype.clone(),
            Self::BinaryOp(_lhs, _rhs, _op, ttype) => ttype.clone(),
        }
    }
}

#[derive(Debug)]
pub struct CheckedWhileLoop {
    pub condition: CheckedExpression,
    pub body: CheckedBlock,
}

#[derive(Debug)]
pub enum CheckedStatement {
    Expression(CheckedExpression),
    LetAssign(String, CheckedExpression),
    WhileLoop(CheckedWhileLoop),
}

#[derive(Debug)]
pub struct CheckedBlock {
    pub statements: Vec<CheckedStatement>,
}

#[derive(Debug)]
pub struct CheckedFunction {
    pub name: String,
    pub body: CheckedBlock,
}

#[derive(Debug)]
pub struct CheckedProgram {
    pub functions: Vec<CheckedFunction>,
}

#[derive(Debug)]
struct Function {
    parameters: Vec<FunctionParameter>,
    return_type: Type,
}

#[derive(Debug, Default)]
struct ScopeStack {
    stack: Vec<(String, HashMap<String, Type>)>,
}

impl ScopeStack {
    fn push_scope(&mut self, function_name: String) {
        self.stack.push((function_name, HashMap::default()));
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn add_variable(&mut self, variable_name: &str, ttype: Type) {
        self.stack
            .last_mut()
            .unwrap()
            .1
            .insert(variable_name.to_string(), ttype);
    }

    fn get_variable_type(&self, variable_name: &str) -> Option<&Type> {
        for scope in self.stack.iter().rev() {
            if let Some(ttype) = scope.1.get(variable_name) {
                return Some(ttype);
            }
        }
        None
    }

    fn get_current_function(&self) -> Option<&str> {
        self.stack.last().map(|scope| scope.0.as_str())
    }
}

#[derive(Debug)]
struct Context {
    known_structs: HashMap<String, ()>,
    known_functions: HashMap<String, Function>,
    scope_stack: ScopeStack,
}

pub fn typecheck_program(program: &ParsedProgram) -> (CheckedProgram, Vec<TypeCheckError>) {
    let mut errors = vec![];

    let mut context = Context {
        known_structs: HashMap::new(),
        known_functions: HashMap::new(),
        scope_stack: ScopeStack::default(),
    };

    for func in &program.extern_functions {
        let name = func.name.clone();
        context.known_functions.insert(
            name,
            Function {
                parameters: func.parameters.clone(),
                return_type: func.return_type.clone(),
            },
        );
    }
    for func in &program.functions {
        let name = func.name.clone();
        context.known_functions.insert(
            name,
            Function {
                parameters: func.parameters.clone(),
                return_type: Type::Unit,
            },
        );
    }
    for r#struct in &program.structs {
        match r#struct {
            ParsedStruct::Opaque(name) => context.known_structs.insert(name.clone(), ()),
        };
    }

    let functions = program
        .functions
        .iter()
        .map(|func| {
            context.scope_stack.push_scope(func.name.clone());

            let (body, mut errs) = typecheck_block(&mut context, &func.body);
            errors.append(&mut errs);

            context.scope_stack.pop_scope();

            CheckedFunction {
                name: func.name.clone(),
                body,
            }
        })
        .collect();

    (CheckedProgram { functions }, errors)
}

fn typecheck_block(
    context: &mut Context,
    block: &ParsedBlock,
) -> (CheckedBlock, Vec<TypeCheckError>) {
    let mut errors = vec![];

    let statements = block
        .statements
        .iter()
        .map(|stmt| {
            let (checked_stmt, mut errs) = typecheck_statement(context, stmt);
            errors.append(&mut errs);
            checked_stmt
        })
        .collect();

    (CheckedBlock { statements }, errors)
}

fn typecheck_statement(
    context: &mut Context,
    statement: &ParsedStatement,
) -> (CheckedStatement, Vec<TypeCheckError>) {
    match statement {
        ParsedStatement::Expression(expr) => {
            let (checked_expr, errors) = typecheck_expression(context, expr);
            (CheckedStatement::Expression(checked_expr), errors)
        }
        ParsedStatement::LetAssign(name, expr, _span) => {
            let (checked_expr, errors) = typecheck_expression(context, expr);
            context.scope_stack.add_variable(name, checked_expr.ttype());
            (
                CheckedStatement::LetAssign(name.clone(), checked_expr),
                errors,
            )
        }
        ParsedStatement::WhileLoop(while_loop) => {
            let mut errors = vec![];

            let (checked_condition, mut errs) =
                typecheck_expression(context, &while_loop.condition);
            errors.append(&mut errs);

            if checked_condition.ttype() != Type::Bool {
                errors.push(TypeCheckError::WrongConditionType(
                    while_loop.condition.span(),
                    checked_condition.ttype(),
                ));
            }

            let (checked_body, mut errs) = typecheck_block(context, &while_loop.body);
            errors.append(&mut errs);

            (
                CheckedStatement::WhileLoop(CheckedWhileLoop {
                    condition: checked_condition,
                    body: checked_body,
                }),
                errors,
            )
        }
    }
}

/// Typecheck an expression and return the type of the expression.
fn typecheck_expression(
    context: &mut Context,
    expression: &ParsedExpression,
) -> (CheckedExpression, Vec<TypeCheckError>) {
    match expression {
        ParsedExpression::Literal(literal) => match literal {
            Literal::String(_, _) => (
                CheckedExpression::Literal(literal.clone(), Type::String),
                vec![],
            ),
            Literal::Int(_, _) => (
                CheckedExpression::Literal(literal.clone(), Type::GenericInt),
                vec![],
            ),
            Literal::Bool(_, _) => (
                CheckedExpression::Literal(literal.clone(), Type::Bool),
                vec![],
            ),
        },
        ParsedExpression::FunctionCall(func_call) => {
            let mut errors = vec![];

            let checked_args: Vec<CheckedExpression> = func_call
                .args
                .iter()
                .map(|arg| {
                    let (checked_arg, mut errs) = typecheck_expression(context, arg);
                    errors.append(&mut errs);
                    checked_arg
                })
                .collect();

            let return_type = if let Some(func) = context.known_functions.get(&func_call.name) {
                if checked_args.len() != func.parameters.len() {
                    errors.push(TypeCheckError::WrongNumArgs(
                        func_call.span,
                        checked_args.len(),
                        func.parameters.len(),
                    ));
                }

                for ((checked_arg, arg), param) in checked_args
                    .iter()
                    .zip(func_call.args.iter())
                    .zip(func.parameters.iter())
                {
                    if checked_arg.ttype() != param.ttype {
                        errors.push(TypeCheckError::WrongArgType(
                            arg.span(),
                            checked_arg.ttype(),
                            param.ttype.clone(),
                        ));
                    }
                }

                func.return_type.clone()
            } else {
                errors.push(TypeCheckError::UnknownFunction(
                    func_call.name.clone(),
                    func_call.name_span,
                ));
                Type::Incomplete
            };

            (
                CheckedExpression::FunctionCall(CheckedFunctionCall {
                    name: func_call.name.clone(),
                    args: checked_args,
                    ttype: return_type,
                }),
                errors,
            )
        }
        ParsedExpression::Variable(variable_name, span) => {
            if let Some(ttype) = context.scope_stack.get_variable_type(variable_name) {
                (
                    CheckedExpression::Variable(variable_name.clone(), ttype.clone()),
                    vec![],
                )
            } else {
                (
                    CheckedExpression::Variable(variable_name.clone(), Type::Incomplete),
                    vec![TypeCheckError::UnknownVariable(
                        variable_name.clone(),
                        context
                            .scope_stack
                            .get_current_function()
                            .map(|name| name.to_string()),
                        *span,
                    )],
                )
            }
        }
        ParsedExpression::BinaryOp(lhs, rhs, op) => {
            let (checked_lhs, mut errors) = typecheck_expression(context, lhs);
            let (checked_rhs, mut errs) = typecheck_expression(context, rhs);
            errors.append(&mut errs);

            if !checked_lhs.ttype().matches(&checked_rhs.ttype()) {
                errors.push(TypeCheckError::BinaryOpMismatch(
                    checked_lhs.ttype(),
                    checked_rhs.ttype(),
                    lhs.span(),
                    rhs.span(),
                ))
            }

            let ttype = match op {
                BinaryOperation::Equality => Type::Bool,
            };

            (
                CheckedExpression::BinaryOp(
                    Box::new(checked_lhs),
                    Box::new(checked_rhs),
                    *op,
                    ttype,
                ),
                errors,
            )
        }
    }
}
