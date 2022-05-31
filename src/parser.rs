use ariadne::{Color, Label, Report, ReportKind};

use crate::{
    error::ReportError,
    lexer::{Token, TokenKind},
    span::{Span, Spanned},
    typechecker::Type,
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
    pub name_span: Span,
    pub args: Vec<ParsedExpression>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Literal {
    String(String, Span),
    Int(i32, Span),
}

#[derive(Debug)]
pub enum ParsedExpression {
    Literal(Literal),
    FunctionCall(ParsedFunctionCall),
    Invalid,
}

impl Spanned for ParsedExpression {
    fn span(&self) -> Span {
        match self {
            Self::Literal(l) => match l {
                Literal::String(_, span) => *span,
                Literal::Int(_, span) => *span,
            },
            Self::FunctionCall(f) => f.span,
            Self::Invalid => panic!("span of invalid expression"),
        }
    }
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
    pub parameters: Vec<FunctionParameter>,
    pub body: ParsedBlock,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub ttype: Type,
}

#[derive(Debug)]
pub struct ParsedExternFunction {
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
}

#[derive(Debug)]
pub struct ParsedProgram {
    pub extern_functions: Vec<ParsedExternFunction>,
    pub functions: Vec<ParsedFunction>,
}

macro_rules! expect {
    ($errors:expr, $tokens:expr, $idx:expr, $($kind:tt)+) => {{
        if matches!(&$tokens[*$idx], &Token { kind: $($kind)+, .. }) {
            *$idx += 1;
        } else {
            $errors.push(ParseError::ExpectedToken($($kind)+, $tokens[*$idx].span()));
        }
    }};
}

pub fn parse_program(tokens: &[Token], idx: &mut usize) -> (ParsedProgram, Vec<ParseError>) {
    let mut errors = vec![];
    let mut program = ParsedProgram {
        extern_functions: vec![],
        functions: vec![],
    };

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
            Token {
                kind: TokenKind::Extern,
                ..
            } => {
                let (fun, mut errs) = parse_extern_function(tokens, idx);
                program.extern_functions.push(fun);
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

fn parse_extern_function(
    tokens: &[Token],
    idx: &mut usize,
) -> (ParsedExternFunction, Vec<ParseError>) {
    let mut errors = vec![];

    *idx += 1; // Consume `extern` keyword

    expect!(&mut errors, tokens, idx, TokenKind::Fn);

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

    expect!(&mut errors, tokens, idx, TokenKind::OParen);

    let mut parameters = vec![];
    while *idx < tokens.len()
        && !matches!(
            &tokens[*idx],
            &Token {
                kind: TokenKind::CParen,
                ..
            }
        )
    {
        let (param, mut errs) = parse_parameter(tokens, idx);
        parameters.push(param);
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

    expect!(&mut errors, tokens, idx, TokenKind::CParen);

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
    expect!(&mut errors, tokens, idx, TokenKind::SemiColon);

    let fun = ParsedExternFunction { name, parameters };

    (fun, errors)
}

fn parse_parameter(tokens: &[Token], idx: &mut usize) -> (FunctionParameter, Vec<ParseError>) {
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

    expect!(&mut errors, tokens, idx, TokenKind::Colon);

    let ttype = if let &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = &tokens[*idx]
    {
        *idx += 1;
        Type::from_str(name)
    } else {
        errors.push(ParseError::ExpectedIdentifier(tokens[*idx].span()));
        Type::Unit
    };

    (FunctionParameter { name, ttype }, errors)
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

    expect!(&mut errors, tokens, idx, TokenKind::OParen);

    let mut parameters = vec![];
    while *idx < tokens.len()
        && !matches!(
            &tokens[*idx],
            &Token {
                kind: TokenKind::CParen,
                ..
            }
        )
    {
        let (param, mut errs) = parse_parameter(tokens, idx);
        parameters.push(param);
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

    expect!(&mut errors, tokens, idx, TokenKind::CParen);

    let (body, mut errs) = parse_block(tokens, idx);
    errors.append(&mut errs);

    let fun = ParsedFunction {
        name,
        body,
        parameters,
    };

    (fun, errors)
}

fn parse_block(tokens: &[Token], idx: &mut usize) -> (ParsedBlock, Vec<ParseError>) {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::OBrace);

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

    expect!(&mut errors, tokens, idx, TokenKind::CBrace);

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

    expect!(&mut errors, tokens, idx, TokenKind::SemiColon);

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
        tok @ Token {
            kind: TokenKind::StringLiteral(string),
            ..
        } => {
            *idx += 1;
            (
                ParsedExpression::Literal(Literal::String(string.clone(), tok.span())),
                vec![],
            )
        }
        tok @ Token {
            kind: TokenKind::IntLiteral(int),
            ..
        } => {
            *idx += 1;
            (
                ParsedExpression::Literal(Literal::Int(*int, tok.span())),
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

    let (name, name_span) = if let tok @ &Token {
        kind: TokenKind::Ident(ref name),
        ..
    } = &tokens[*idx]
    {
        *idx += 1;
        (name.clone(), tok.span())
    } else {
        errors.push(ParseError::ExpectedIdentifier(tokens[*idx].span()));
        (String::new(), tokens[*idx].span())
    };

    expect!(&mut errors, tokens, idx, TokenKind::OParen);

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

    let cparen_span = tokens[*idx].span();
    expect!(&mut errors, tokens, idx, TokenKind::CParen);

    let func_call = ParsedFunctionCall {
        name,
        name_span,
        args,
        span: name_span.to(cparen_span),
    };

    (func_call, errors)
}
