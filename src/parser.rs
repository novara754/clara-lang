use std::collections::HashMap;

use ariadne::{Color, Label, Report, ReportKind};
use serde_json::json;

use crate::{
    error::{JsonError, ReportError},
    lexer::{Token, TokenKind},
    span::{Span, Spanned},
    typechecker::Type,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Span),
    ExpectedIdentifier(Span),
    ExpectedToken(TokenKind, Span),
    UnexpectedEndOfInput(Span),
}

impl ReportError for ParseError {
    fn report(&self) -> ariadne::Report<Span> {
        use ParseError::*;
        match *self {
            UnexpectedToken(span) => Report::build(ReportKind::Error, (), span.start)
                .with_message("unexpected token encountered")
                .with_label(Label::new(span).with_color(Color::Red)),
            ExpectedIdentifier(span) => Report::build(ReportKind::Error, (), span.start)
                .with_message("expected identifier")
                .with_label(Label::new(span).with_color(Color::Red)),
            ExpectedToken(ref kind, span) => Report::build(ReportKind::Error, (), span.start)
                .with_message(format!("expected token {}", kind.human_name()))
                .with_label(Label::new(span).with_color(Color::Red)),
            UnexpectedEndOfInput(span) => Report::build(ReportKind::Error, (), span.start)
                .with_message("unexpected end of input")
                .with_label(Label::new(span).with_color(Color::Red)),
        }
        .finish()
    }
}

impl JsonError for ParseError {
    fn json(&self) -> serde_json::Value {
        use ParseError::*;
        match *self {
            UnexpectedToken(span) => json!({
                "message": "unexpected token encountered",
                "span": span.json(),
            }),
            ExpectedIdentifier(span) => json!({
                "message": "expected identifier",
                "span": span.json(),
            }),
            ExpectedToken(ref kind, span) => json!({
                "message": format!("expected token {}", kind.human_name()),
                "span": span.json(),
            }),
            UnexpectedEndOfInput(span) => json!({
                "message": "reached unexpected end of input",
                "span": span.json(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedFunctionCall {
    pub name: String,
    pub name_span: Span,
    pub args: Vec<ParsedExpression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedStructLiteral {
    pub name: String,
    pub name_span: Span,
    pub fields: Vec<(String, Span, ParsedExpression)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String, Span),
    Int(i32, Span),
    Bool(bool, Span),
    Struct(ParsedStructLiteral, Span),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperation {
    Equality,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

impl BinaryOperation {
    pub fn to_c(self) -> &'static str {
        match self {
            Self::Equality => "==",
            Self::GreaterThan => ">",
            Self::GreaterThanEqual => ">=",
            Self::LessThan => "<",
            Self::LessThanEqual => "<=",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedFieldAccess {
    pub object: Box<ParsedExpression>,
    pub object_span: Span,
    pub field_name: String,
    pub field_name_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedExpression {
    Literal(Literal),
    FunctionCall(ParsedFunctionCall),
    Variable(String, Span),
    BinaryOp(
        Box<ParsedExpression>,
        Box<ParsedExpression>,
        BinaryOperation,
    ),
    FieldAccess(ParsedFieldAccess),
}

impl Spanned for ParsedExpression {
    fn span(&self) -> Span {
        match self {
            Self::Literal(l) => match l {
                Literal::String(_, span) => *span,
                Literal::Int(_, span) => *span,
                Literal::Bool(_, span) => *span,
                Literal::Struct(_, span) => *span,
            },
            Self::FunctionCall(f) => f.span,
            Self::Variable(_, span) => *span,
            Self::BinaryOp(lhs, rhs, _) => lhs.span().to(rhs.span()),
            ParsedExpression::FieldAccess(field_access) => field_access.span,
        }
    }
}

#[derive(Debug)]
pub struct ParsedWhileLoop {
    pub condition: ParsedExpression,
    pub body: ParsedBlock,
}

#[derive(Debug)]
pub struct ParsedIfElse {
    pub condition: ParsedExpression,
    pub if_body: ParsedBlock,
    pub else_body: Option<ParsedBlock>,
}

#[derive(Debug)]
pub enum ParsedStatement {
    Expression(ParsedExpression),
    LetAssign(String, ParsedExpression, Span),
    WhileLoop(ParsedWhileLoop),
    IfElse(ParsedIfElse),
}

#[derive(Debug)]
pub struct ParsedBlock {
    pub statements: Vec<ParsedStatement>,
}

#[derive(Debug)]
pub struct ParsedFunction {
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
    pub body: ParsedBlock,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub ttype: Type,
    pub type_span: Span,
}

#[derive(Debug)]
pub struct ParsedExternFunction {
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
    pub return_type_span: Span,
}

#[derive(Debug)]
pub enum ParsedStruct {
    Opaque(String),
    Transparent(String, HashMap<String, Type>),
}

#[derive(Debug)]
pub struct ParsedProgram {
    pub structs: Vec<ParsedStruct>,
    pub extern_functions: Vec<ParsedExternFunction>,
    pub functions: Vec<ParsedFunction>,
}

macro_rules! expect {
    ($errors:expr, $tokens:expr, $idx:expr, $($kind:tt)+) => {{
        if matches!($tokens.get(*$idx)?, &Token { kind: $($kind)+, .. }) {
            *$idx += 1;
        } else {
            $errors.push(ParseError::ExpectedToken($($kind)+, $tokens.get(*$idx)?.span()));
        }
    }};
}

macro_rules! recover_at_token {
    ($errors:expr, $tokens:expr, $idx:expr, $($expected_kind:tt)+) => {{
        while *$idx < $tokens.len()
            && !matches!(
                $tokens.get(*$idx)?,
                &Token {
                    kind: $($expected_kind)+,
                    ..
                }
            )
        {
            $errors.push(ParseError::ExpectedToken(
                $($expected_kind)+,
                $tokens.get(*$idx)?.span(),
            ));
            *$idx += 1;
        }
        expect!($errors, $tokens, $idx, $($expected_kind)+);
    }};
}

pub fn parse_program(tokens: &[Token], idx: &mut usize) -> (ParsedProgram, Vec<ParseError>) {
    let mut errors = vec![];
    let mut program = ParsedProgram {
        structs: vec![],
        extern_functions: vec![],
        functions: vec![],
    };

    while *idx < tokens.len() {
        let reached_unexpected_eoi = (|| {
            let token = &tokens[*idx];
            match token {
                Token {
                    kind: TokenKind::Opaque,
                    ..
                } => {
                    let (r#struct, mut errs) = parse_opaque_struct(tokens, idx)?;
                    program.structs.push(r#struct);
                    errors.append(&mut errs);
                }
                Token {
                    kind: TokenKind::Struct,
                    ..
                } => {
                    let (r#struct, mut errs) = parse_struct(tokens, idx)?;
                    program.structs.push(r#struct);
                    errors.append(&mut errs);
                }
                Token {
                    kind: TokenKind::Fn,
                    ..
                } => {
                    let (fun, mut errs) = parse_function(tokens, idx)?;
                    program.functions.push(fun);
                    errors.append(&mut errs);
                }
                Token {
                    kind: TokenKind::Extern,
                    ..
                } => {
                    let (fun, mut errs) = parse_extern_function(tokens, idx)?;
                    program.extern_functions.push(fun);
                    errors.append(&mut errs);
                }
                _ => {
                    errors.push(ParseError::UnexpectedToken(token.span()));
                    *idx += 1;
                }
            }
            Some(())
        })()
        .is_none();

        if reached_unexpected_eoi {
            let last_span = tokens.last().unwrap().span();
            let span = Span::new(last_span.start + last_span.len - 1, 1);
            errors.push(ParseError::UnexpectedEndOfInput(span));
            break;
        };
    }

    (program, errors)
}

fn parse_struct(tokens: &[Token], idx: &mut usize) -> Option<(ParsedStruct, Vec<ParseError>)> {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::Struct);

    let (name, _name_span, mut errs) = parse_name(tokens, idx)?;
    errors.append(&mut errs);

    expect!(&mut errors, tokens, idx, TokenKind::OBrace);

    let mut fields = vec![];
    while *idx < tokens.len()
        && !matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::CBrace,
                ..
            }
        )
    {
        let (field, mut errs) = parse_parameter(tokens, idx)?;
        fields.push((field.name, field.ttype));
        errors.append(&mut errs);

        if matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::Comma,
                ..
            }
        ) {
            *idx += 1;
        } else {
            break;
        }
    }

    recover_at_token!(&mut errors, tokens, idx, TokenKind::CBrace);

    Some((
        ParsedStruct::Transparent(name, fields.into_iter().collect()),
        errors,
    ))
}

fn parse_opaque_struct(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedStruct, Vec<ParseError>)> {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::Opaque);
    expect!(&mut errors, tokens, idx, TokenKind::Struct);

    let (name, _name_span, mut errs) = parse_name(tokens, idx)?;
    errors.append(&mut errs);

    expect!(&mut errors, tokens, idx, TokenKind::SemiColon);

    Some((ParsedStruct::Opaque(name), errors))
}

fn parse_extern_function(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedExternFunction, Vec<ParseError>)> {
    let mut errors = vec![];

    *idx += 1; // Consume `extern` keyword

    expect!(&mut errors, tokens, idx, TokenKind::Fn);

    let name = if let &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = tokens.get(*idx)?
    {
        *idx += 1;
        name.clone()
    } else {
        errors.push(ParseError::ExpectedIdentifier(tokens[*idx].span()));
        String::new()
    };

    expect!(&mut errors, tokens, idx, TokenKind::OParen);

    let mut parameters = vec![];
    while *idx < tokens.len()
        && !matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::CParen,
                ..
            }
        )
    {
        let (param, mut errs) = parse_parameter(tokens, idx)?;
        parameters.push(param);
        errors.append(&mut errs);

        if matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::Comma,
                ..
            }
        ) {
            *idx += 1;
        } else {
            break;
        }
    }

    expect!(&mut errors, tokens, idx, TokenKind::CParen);

    let (return_type, return_type_span) = if let Some(Token {
        kind: TokenKind::Colon,
        ..
    }) = tokens.get(*idx)
    {
        expect!(&mut errors, tokens, idx, TokenKind::Colon);
        let (return_type, return_type_span, mut errs) = parse_type(tokens, idx)?;
        errors.append(&mut errs);

        (return_type, return_type_span)
    } else {
        (Type::Unit, Span::new(0, 0))
    };

    // Semicolon should be the very next token, but if there was a parse error before
    // that might not be the case.
    // Looking for the next semicolon allows for recovery from an invalid state
    recover_at_token!(&mut errors, tokens, idx, TokenKind::SemiColon);

    let fun = ParsedExternFunction {
        name,
        parameters,
        return_type,
        return_type_span,
    };

    Some((fun, errors))
}

fn parse_type(tokens: &[Token], idx: &mut usize) -> Option<(Type, Span, Vec<ParseError>)> {
    let mut errors = vec![];

    let is_pointer = matches!(
        tokens.get(*idx)?,
        Token {
            kind: TokenKind::RightArrow,
            ..
        }
    );
    if is_pointer {
        *idx += 1;
    }

    let (ttype, type_span) = if let tok @ &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = tokens.get(*idx)?
    {
        *idx += 1;
        (Type::from_string(name), tok.span())
    } else {
        let span = tokens.get(*idx)?.span();
        errors.push(ParseError::ExpectedIdentifier(span));
        (Type::Unit, span)
    };

    let ttype = if is_pointer {
        Type::Pointer(Box::new(ttype))
    } else {
        ttype
    };

    Some((ttype, type_span, errors))
}

fn parse_parameter(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(FunctionParameter, Vec<ParseError>)> {
    let mut errors = vec![];

    let name = if let &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = tokens.get(*idx)?
    {
        *idx += 1;
        name.clone()
    } else {
        errors.push(ParseError::ExpectedIdentifier(tokens.get(*idx)?.span()));
        String::new()
    };

    expect!(&mut errors, tokens, idx, TokenKind::Colon);

    let (ttype, type_span, mut errs) = parse_type(tokens, idx)?;
    errors.append(&mut errs);

    Some((
        FunctionParameter {
            name,
            ttype,
            type_span,
        },
        errors,
    ))
}

fn parse_function(tokens: &[Token], idx: &mut usize) -> Option<(ParsedFunction, Vec<ParseError>)> {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::Fn);

    let (name, _name_span, mut errs) = parse_name(tokens, idx)?;
    errors.append(&mut errs);

    expect!(&mut errors, tokens, idx, TokenKind::OParen);

    let mut parameters = vec![];
    while *idx < tokens.len()
        && !matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::CParen,
                ..
            }
        )
    {
        let (param, mut errs) = parse_parameter(tokens, idx)?;
        parameters.push(param);
        errors.append(&mut errs);

        if matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::Comma,
                ..
            }
        ) {
            *idx += 1;
        } else {
            break;
        }
    }

    expect!(&mut errors, tokens, idx, TokenKind::CParen);

    let (body, mut errs) = parse_block(tokens, idx)?;
    errors.append(&mut errs);

    let fun = ParsedFunction {
        name,
        body,
        parameters,
    };

    Some((fun, errors))
}

fn parse_block(tokens: &[Token], idx: &mut usize) -> Option<(ParsedBlock, Vec<ParseError>)> {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::OBrace);

    let mut statements = vec![];
    while *idx < tokens.len()
        && !matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::CBrace,
                ..
            }
        )
    {
        let (stmt, mut errs) = parse_statement(tokens, idx)?;
        statements.push(stmt);
        errors.append(&mut errs);
    }

    expect!(&mut errors, tokens, idx, TokenKind::CBrace);

    Some((ParsedBlock { statements }, errors))
}

fn parse_statement(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedStatement, Vec<ParseError>)> {
    let (statement, mut errors, needs_semi) = match tokens.get(*idx)? {
        Token {
            kind: TokenKind::Let,
            ..
        } => {
            let mut errors = vec![];

            *idx += 1; // Consume `let` token

            let (name, name_span) = if let tok @ &Token {
                kind: TokenKind::Ident(ref name),
                ..
            } = &tokens.get(*idx)?
            {
                *idx += 1;
                (name.clone(), tok.span())
            } else {
                errors.push(ParseError::ExpectedIdentifier(tokens.get(*idx)?.span()));
                (String::new(), tokens.get(*idx)?.span())
            };

            expect!(&mut errors, tokens, idx, TokenKind::Equal);

            let (value, mut errs) = parse_expression(tokens, idx)?;
            errors.append(&mut errs);

            let span = name_span.to(value.span());

            (ParsedStatement::LetAssign(name, value, span), errors, true)
        }
        Token {
            kind: TokenKind::While,
            ..
        } => {
            let (stmt, errors) = parse_while_loop(tokens, idx)?;
            (ParsedStatement::WhileLoop(stmt), errors, false)
        }
        Token {
            kind: TokenKind::If,
            ..
        } => {
            let (if_else, errors) = parse_if_else(tokens, idx)?;
            (ParsedStatement::IfElse(if_else), errors, false)
        }
        _ => {
            let (expr, errors) = parse_expression(tokens, idx)?;
            (ParsedStatement::Expression(expr), errors, true)
        }
    };

    if needs_semi {
        // Semicolon should be the very next token, but if there was a parse error before
        // that might not be the case.
        // Looking for the next semicolon allows for recovery from an invalid state
        recover_at_token!(&mut errors, tokens, idx, TokenKind::SemiColon);
    }

    Some((statement, errors))
}

fn parse_while_loop(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedWhileLoop, Vec<ParseError>)> {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::While);

    let (condition, mut errs) = parse_expression(tokens, idx)?;
    errors.append(&mut errs);

    let (body, mut errs) = parse_block(tokens, idx)?;
    errors.append(&mut errs);

    Some((ParsedWhileLoop { condition, body }, errors))
}

fn parse_if_else(tokens: &[Token], idx: &mut usize) -> Option<(ParsedIfElse, Vec<ParseError>)> {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::If);

    let (condition, mut errs) = parse_expression(tokens, idx)?;
    errors.append(&mut errs);

    let (if_body, mut errs) = parse_block(tokens, idx)?;
    errors.append(&mut errs);

    let else_body = if matches!(
        tokens.get(*idx),
        Some(Token {
            kind: TokenKind::Else,
            ..
        })
    ) {
        expect!(&mut errors, tokens, idx, TokenKind::Else);

        let (else_body, mut errs) = parse_block(tokens, idx)?;
        errors.append(&mut errs);

        Some(else_body)
    } else {
        None
    };

    Some((
        ParsedIfElse {
            condition,
            if_body,
            else_body,
        },
        errors,
    ))
}

fn parse_expression(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedExpression, Vec<ParseError>)> {
    let mut errors = vec![];
    let (expr, mut errors) = loop {
        break match tokens.get(*idx)? {
            tok @ Token {
                kind: TokenKind::Ident(name),
                ..
            } => match tokens.get(*idx + 1) {
                Some(Token {
                    kind: TokenKind::OParen,
                    ..
                }) => {
                    let (func_call, mut errs) = parse_function_call(tokens, idx)?;
                    errors.append(&mut errs);
                    (ParsedExpression::FunctionCall(func_call), errors)
                }
                Some(Token {
                    kind: TokenKind::OBrace,
                    ..
                }) => {
                    let (struct_literal, mut errs) = parse_struct_literal(tokens, idx)?;
                    errors.append(&mut errs);
                    let span = struct_literal.span;
                    (
                        ParsedExpression::Literal(Literal::Struct(struct_literal, span)),
                        errors,
                    )
                }
                _ => {
                    *idx += 1;
                    (ParsedExpression::Variable(name.clone(), tok.span()), errors)
                }
            },
            tok @ Token {
                kind: TokenKind::StringLiteral(string),
                ..
            } => {
                *idx += 1;
                (
                    ParsedExpression::Literal(Literal::String(string.clone(), tok.span())),
                    errors,
                )
            }
            tok @ Token {
                kind: TokenKind::IntLiteral(int),
                ..
            } => {
                *idx += 1;
                (
                    ParsedExpression::Literal(Literal::Int(*int, tok.span())),
                    errors,
                )
            }
            tok @ Token {
                kind: TokenKind::True | TokenKind::False,
                ..
            } => {
                *idx += 1;
                let bool_value = matches!(tok.kind, TokenKind::True);
                (
                    ParsedExpression::Literal(Literal::Bool(bool_value, tok.span())),
                    errors,
                )
            }
            tok => {
                errors.push(ParseError::UnexpectedToken(tok.span()));
                *idx += 1;
                continue;
            }
        };
    };

    let expr = if let Some(Token {
        kind: TokenKind::Dot,
        ..
    }) = tokens.get(*idx)
    {
        *idx += 1; // Consume dot token.

        let (field_name, field_name_span, mut errs) = parse_name(tokens, idx)?;
        errors.append(&mut errs);

        let object_span = expr.span();
        let span = object_span.to(field_name_span);

        ParsedExpression::FieldAccess(ParsedFieldAccess {
            object: Box::new(expr),
            object_span,
            field_name,
            field_name_span,
            span,
        })
    } else {
        expr
    };

    let expr = if let Some(
        tok @ Token {
            kind:
                TokenKind::EqualEqual
                | TokenKind::GreaterThan
                | TokenKind::GreaterThanEqual
                | TokenKind::LessThan
                | TokenKind::LessThanEqual,
            ..
        },
    ) = tokens.get(*idx)
    {
        *idx += 1; // Consume oeprator token

        let op = match tok.kind {
            TokenKind::EqualEqual => BinaryOperation::Equality,
            TokenKind::GreaterThan => BinaryOperation::GreaterThan,
            TokenKind::GreaterThanEqual => BinaryOperation::GreaterThanEqual,
            TokenKind::LessThan => BinaryOperation::LessThan,
            TokenKind::LessThanEqual => BinaryOperation::LessThanEqual,
            _ => unreachable!(),
        };

        let (rhs, mut errs) = parse_expression(tokens, idx)?;
        errors.append(&mut errs);

        ParsedExpression::BinaryOp(Box::new(expr), Box::new(rhs), op)
    } else {
        expr
    };

    Some((expr, errors))
}

fn parse_struct_literal(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedStructLiteral, Vec<ParseError>)> {
    let (name, name_span, mut errors) = parse_name(tokens, idx)?;

    expect!(&mut errors, tokens, idx, TokenKind::OBrace);

    let mut fields = vec![];
    while *idx < tokens.len()
        && !matches!(
            &tokens.get(*idx)?,
            &Token {
                kind: TokenKind::CBrace,
                ..
            }
        )
    {
        let (field_name, field_name_span, mut errs) = parse_name(tokens, idx)?;
        errors.append(&mut errs);

        expect!(&mut errors, tokens, idx, TokenKind::Colon);

        let (field_value, mut errs) = parse_expression(tokens, idx)?;
        errors.append(&mut errs);

        fields.push((field_name, field_name_span, field_value));

        if matches!(
            &tokens.get(*idx)?,
            &Token {
                kind: TokenKind::Comma,
                ..
            }
        ) {
            *idx += 1;
        } else {
            break;
        }
    }

    expect!(&mut errors, tokens, idx, TokenKind::CBrace);
    let c_brace_span = tokens[*idx - 1].span();

    Some((
        ParsedStructLiteral {
            name,
            name_span,
            fields,
            span: name_span.to(c_brace_span),
        },
        errors,
    ))
}

fn parse_function_call(
    tokens: &[Token],
    idx: &mut usize,
) -> Option<(ParsedFunctionCall, Vec<ParseError>)> {
    let (name, name_span, mut errors) = parse_name(tokens, idx)?;

    expect!(&mut errors, tokens, idx, TokenKind::OParen);

    let mut args = vec![];
    while *idx < tokens.len()
        && !matches!(
            tokens.get(*idx)?,
            &Token {
                kind: TokenKind::CParen,
                ..
            }
        )
    {
        let (arg, mut errs) = parse_expression(tokens, idx)?;
        args.push(arg);
        errors.append(&mut errs);

        if matches!(
            &tokens.get(*idx)?,
            &Token {
                kind: TokenKind::Comma,
                ..
            }
        ) {
            *idx += 1;
        } else {
            break;
        }
    }

    let cparen_span = tokens.get(*idx)?.span();
    expect!(&mut errors, tokens, idx, TokenKind::CParen);

    let func_call = ParsedFunctionCall {
        name,
        name_span,
        args,
        span: name_span.to(cparen_span),
    };

    Some((func_call, errors))
}

fn parse_name(tokens: &[Token], idx: &mut usize) -> Option<(String, Span, Vec<ParseError>)> {
    Some(
        if let tok @ &Token {
            kind: TokenKind::Ident(ref name),
            ..
        } = tokens.get(*idx)?
        {
            *idx += 1;
            (name.clone(), tok.span(), vec![])
        } else {
            (
                String::new(),
                tokens.get(*idx)?.span(),
                vec![ParseError::ExpectedIdentifier(tokens.get(*idx)?.span())],
            )
        },
    )
}
