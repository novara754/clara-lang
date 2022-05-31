use ariadne::{Color, Label, Report, ReportKind};

use crate::{
    error::ReportError,
    lexer::{Token, TokenKind},
    span::{Span, Spanned},
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Span),
    ExpectedIdentifier(Span),
    ExpectedToken(TokenKind, Span),
}

impl ReportError for ParseError {
    fn report(&self) -> ariadne::Report<Span> {
        use ParseError::*;
        let report = Report::build(ReportKind::Error, (), 0);
        match *self {
            UnexpectedToken(span) => report
                .with_message("unexpected token encountered")
                .with_label(Label::new(span).with_color(Color::Red)),
            ExpectedIdentifier(span) => report
                .with_message("expected identifier")
                .with_label(Label::new(span).with_color(Color::Red)),
            ExpectedToken(ref kind, span) => report
                .with_message(format!("expected token {}", kind.human_name()))
                .with_label(Label::new(span).with_color(Color::Red)),
        }
        .finish()
    }
}

#[derive(Debug)]
pub struct ParsedFunctionCall {
    pub name: String,
    pub args: Vec<ParsedExpression>,
}

#[derive(Debug)]
pub enum Literal {
    String(String),
}

#[derive(Debug)]
pub enum ParsedExpression {
    Literal(Literal),
    FunctionCall(ParsedFunctionCall),
    Invalid,
}

#[derive(Debug)]
pub enum ParsedStatement {
    Expression(ParsedExpression),
}

#[derive(Debug)]
pub struct ParsedBlock {
    pub statements: Vec<ParsedStatement>,
}

#[derive(Debug)]
pub struct ParsedFunction {
    pub name: String,
    pub body: ParsedBlock,
}

#[derive(Debug)]
pub struct ParsedProgram {
    pub functions: Vec<ParsedFunction>,
}

pub fn parse_program(tokens: &[Token], idx: &mut usize) -> (ParsedProgram, Vec<ParseError>) {
    let mut errors = vec![];
    let mut program = ParsedProgram { functions: vec![] };

    while *idx < tokens.len() {
        let token = &tokens[*idx];
        match token {
            Token {
                kind: TokenKind::Fn,
                ..
            } => {
                let (fun, mut errs) = parse_function(tokens, idx);
                program.functions.push(fun);
                errors.append(&mut errs);
            }
            _ => {
                errors.push(ParseError::UnexpectedToken(token.span()));
                *idx += 1;
            }
        }
    }

    (program, errors)
}

fn parse_function(tokens: &[Token], idx: &mut usize) -> (ParsedFunction, Vec<ParseError>) {
    let mut errors = vec![];

    *idx += 1; // Consume `fn` keyword

    let name = if let &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = &tokens[*idx]
    {
        *idx += 1;
        name.clone()
    } else {
        errors.push(ParseError::ExpectedIdentifier(tokens[*idx].span()));
        String::new()
    };

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::OParen,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::OParen,
            tokens[*idx].span(),
        ));
    }

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::CParen,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::CParen,
            tokens[*idx].span(),
        ));
    }

    let (body, mut errs) = parse_block(tokens, idx);
    errors.append(&mut errs);

    let fun = ParsedFunction { name, body };

    (fun, errors)
}

fn parse_block(tokens: &[Token], idx: &mut usize) -> (ParsedBlock, Vec<ParseError>) {
    let mut errors = vec![];

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::OBrace,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::OBrace,
            tokens[*idx].span(),
        ));
    }

    let mut statements = vec![];
    while *idx < tokens.len()
        && !matches!(
            &tokens[*idx],
            &Token {
                kind: TokenKind::CBrace,
                ..
            }
        )
    {
        let (stmt, mut errs) = parse_statement(tokens, idx);
        statements.push(stmt);
        errors.append(&mut errs);
    }

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::CBrace,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::CBrace,
            tokens[*idx].span(),
        ));
    }

    (ParsedBlock { statements }, errors)
}

fn parse_statement(tokens: &[Token], idx: &mut usize) -> (ParsedStatement, Vec<ParseError>) {
    let (expr, mut errors) = parse_expression(tokens, idx);

    // Semicolon should be the very next token, but if there was a parse error before
    // that might not be the case.
    // Looking for the next semicolon allows for recovery from an invalid state
    while *idx < tokens.len()
        && !matches!(
            &tokens[*idx],
            &Token {
                kind: TokenKind::SemiColon,
                ..
            }
        )
    {
        *idx += 1;
    }

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::SemiColon,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::SemiColon,
            tokens[*idx].span(),
        ));
    }

    (ParsedStatement::Expression(expr), errors)
}

fn parse_expression(tokens: &[Token], idx: &mut usize) -> (ParsedExpression, Vec<ParseError>) {
    match &tokens[*idx] {
        Token {
            kind: TokenKind::Ident(_),
            ..
        } => {
            let (func_call, errors) = parse_function_call(tokens, idx);
            (ParsedExpression::FunctionCall(func_call), errors)
        }
        Token {
            kind: TokenKind::StringLiteral(string),
            ..
        } => {
            *idx += 1;
            (
                ParsedExpression::Literal(Literal::String(string.clone())),
                vec![],
            )
        }
        _ => (
            ParsedExpression::Invalid,
            vec![ParseError::UnexpectedToken(tokens[*idx].span())],
        ),
    }
}

fn parse_function_call(tokens: &[Token], idx: &mut usize) -> (ParsedFunctionCall, Vec<ParseError>) {
    let mut errors = vec![];

    let name = if let &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = &tokens[*idx]
    {
        *idx += 1;
        name.clone()
    } else {
        errors.push(ParseError::ExpectedIdentifier(tokens[*idx].span()));
        String::new()
    };

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::OParen,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::OParen,
            tokens[*idx].span(),
        ));
    }

    let mut args = vec![];
    while *idx < tokens.len()
        && !matches!(
            &tokens[*idx],
            &Token {
                kind: TokenKind::CParen,
                ..
            }
        )
    {
        let (arg, mut errs) = parse_expression(tokens, idx);
        args.push(arg);
        errors.append(&mut errs);

        if matches!(
            &tokens[*idx],
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

    if matches!(
        &tokens[*idx],
        &Token {
            kind: TokenKind::CParen,
            ..
        }
    ) {
        *idx += 1;
    } else {
        errors.push(ParseError::ExpectedToken(
            TokenKind::CParen,
            tokens[*idx].span(),
        ));
    }

    let func_call = ParsedFunctionCall { name, args };

    (func_call, errors)
}
