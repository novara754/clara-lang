use std::collections::HashMap;

use ariadne::{Color, Label, Report, ReportKind};

use crate::{
    error::ReportError,
    parser::{FunctionParameter, Literal, ParsedExpression, ParsedProgram, ParsedStatement},
    span::{Span, Spanned},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    String,
    Int,
    Unit,
}

impl Type {
    pub fn from_str(typename: &str) -> Self {
        match typename {
            "string" => Type::String,
            "int" => Type::Int,
            _ => panic!("Type::from_str: invalid typename"),
        }
    }

    pub fn to_str(self) -> &'static str {
        match self {
            Self::String => "string",
            Self::Int => "int",
            Self::Unit => "unit",
        }
    }
}

pub enum TypeCheckError {
    WrongNumArgs(Span, usize, usize),
    WrongArgType(Span, Type, Type),
    UnknownFunction(String, Span),
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
            Self::WrongArgType(span, actual, expected) => {
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
        }
        .finish()
    }
}

struct Function {
    parameters: Vec<FunctionParameter>,
    return_type: Type,
}

struct Context {
    known_functions: HashMap<String, Function>,
}

pub fn typecheck_program(program: &ParsedProgram) -> Vec<TypeCheckError> {
    let mut errors = vec![];
    let mut context = Context {
        known_functions: HashMap::new(),
    };

    for func in &program.extern_functions {
        let name = func.name.clone();
        context.known_functions.insert(
            name,
            Function {
                parameters: func.parameters.clone(),
                return_type: Type::Unit,
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

    for func in &program.functions {
        for stmt in &func.body.statements {
            match stmt {
                ParsedStatement::Expression(expr) => {
                    let (_, mut errs) = typecheck_expression(&context, expr);
                    errors.append(&mut errs);
                }
            }
        }
    }

    errors
}

/// Typecheck an expression and return the type of the expression.
fn typecheck_expression(
    context: &Context,
    expression: &ParsedExpression,
) -> (Type, Vec<TypeCheckError>) {
    match expression {
        ParsedExpression::Literal(literal) => match literal {
            Literal::String(_, _) => (Type::String, vec![]),
            Literal::Int(_, _) => (Type::Int, vec![]),
        },
        ParsedExpression::FunctionCall(func_call) => {
            if let Some(func) = context.known_functions.get(&func_call.name) {
                let mut errors = vec![];

                if func_call.args.len() != func.parameters.len() {
                    errors.push(TypeCheckError::WrongNumArgs(
                        func_call.span,
                        func_call.args.len(),
                        func.parameters.len(),
                    ));
                }

                for (arg, param) in func_call.args.iter().zip(func.parameters.iter()) {
                    let (arg_type, mut errs) = typecheck_expression(context, arg);
                    errors.append(&mut errs);
                    if arg_type != param.ttype {
                        errors.push(TypeCheckError::WrongArgType(
                            arg.span(),
                            arg_type,
                            param.ttype,
                        ));
                    }
                }

                (func.return_type, errors)
            } else {
                (
                    Type::Unit,
                    vec![TypeCheckError::UnknownFunction(
                        func_call.name.clone(),
                        func_call.name_span,
                    )],
                )
            }
        }
        ParsedExpression::Invalid => (Type::Unit, vec![]),
    }
}
