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
        }
        .finish()
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
pub enum Literal {
    String(String, Span),
    Int(i32, Span),
    Bool(bool, Span),
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
pub enum ParsedExpression {
    Literal(Literal),
    FunctionCall(ParsedFunctionCall),
    Variable(String, Span),
    BinaryOp(
        Box<ParsedExpression>,
        Box<ParsedExpression>,
        BinaryOperation,
    ),
}

impl Spanned for ParsedExpression {
    fn span(&self) -> Span {
        match self {
            Self::Literal(l) => match l {
                Literal::String(_, span) => *span,
                Literal::Int(_, span) => *span,
                Literal::Bool(_, span) => *span,
            },
            Self::FunctionCall(f) => f.span,
            Self::Variable(_, span) => *span,
            Self::BinaryOp(lhs, rhs, _) => lhs.span().to(rhs.span()),
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
}

#[derive(Debug)]
pub struct ParsedExternFunction {
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
}

#[derive(Debug)]
pub enum ParsedStruct {
    Opaque(String),
}

#[derive(Debug)]
pub struct ParsedProgram {
    pub structs: Vec<ParsedStruct>,
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

macro_rules! recover_at_token {
    ($tokens:expr, $idx:expr, $($expected_kind:tt)+) => {{
        let mut errors = vec![];
        while *$idx < $tokens.len()
            && !matches!(
                &$tokens[*$idx],
                &Token {
                    kind: $($expected_kind)+,
                    ..
                }
            )
        {
            errors.push(ParseError::ExpectedToken(
                $($expected_kind)+,
                $tokens[*$idx].span(),
            ));
            *$idx += 1;
        }
        expect!(&mut errors, $tokens, $idx, $($expected_kind)+);
        errors
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
        let token = &tokens[*idx];
        match token {
            Token {
                kind: TokenKind::Opaque,
                ..
            } => {
                let (r#struct, mut errs) = parse_opaque_struct(tokens, idx);
                program.structs.push(r#struct);
                errors.append(&mut errs);
            }
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

fn parse_opaque_struct(tokens: &[Token], idx: &mut usize) -> (ParsedStruct, Vec<ParseError>) {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::Opaque);
    expect!(&mut errors, tokens, idx, TokenKind::Struct);

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

    expect!(&mut errors, tokens, idx, TokenKind::SemiColon);

    (ParsedStruct::Opaque(name), errors)
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

    let return_type = if let Some(Token {
        kind: TokenKind::Colon,
        ..
    }) = tokens.get(*idx)
    {
        expect!(&mut errors, tokens, idx, TokenKind::Colon);
        let (return_type, mut errs) = parse_type(tokens, idx);
        errors.append(&mut errs);

        return_type
    } else {
        Type::Unit
    };

    // Semicolon should be the very next token, but if there was a parse error before
    // that might not be the case.
    // Looking for the next semicolon allows for recovery from an invalid state
    let mut errs = recover_at_token!(tokens, idx, TokenKind::SemiColon);
    errors.append(&mut errs);

    let fun = ParsedExternFunction {
        name,
        parameters,
        return_type,
    };

    (fun, errors)
}

fn parse_type(tokens: &[Token], idx: &mut usize) -> (Type, Vec<ParseError>) {
    let mut errors = vec![];

    let is_pointer = matches!(
        tokens[*idx],
        Token {
            kind: TokenKind::RightArrow,
            ..
        }
    );
    if is_pointer {
        *idx += 1;
    }

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

    let ttype = if is_pointer {
        Type::Pointer(Box::new(ttype))
    } else {
        ttype
    };

    (ttype, errors)
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

    let (ttype, mut errs) = parse_type(tokens, idx);
    errors.append(&mut errs);

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
    let (statement, mut errors, needs_semi) = match &tokens[*idx] {
        Token {
            kind: TokenKind::Let,
            ..
        } => {
            let mut errors = vec![];

            *idx += 1; // Consume `let` token

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

            expect!(&mut errors, tokens, idx, TokenKind::Equal);

            let (value, mut errs) = parse_expression(tokens, idx);
            errors.append(&mut errs);

            let span = name_span.to(value.span());

            (ParsedStatement::LetAssign(name, value, span), errors, true)
        }
        Token {
            kind: TokenKind::While,
            ..
        } => {
            let (stmt, errors) = parse_while_loop(tokens, idx);
            (ParsedStatement::WhileLoop(stmt), errors, false)
        }
        Token {
            kind: TokenKind::If,
            ..
        } => {
            let (if_else, errors) = parse_if_else(tokens, idx);
            (ParsedStatement::IfElse(if_else), errors, false)
        }
        _ => {
            let (expr, errors) = parse_expression(tokens, idx);
            (ParsedStatement::Expression(expr), errors, true)
        }
    };

    if needs_semi {
        // Semicolon should be the very next token, but if there was a parse error before
        // that might not be the case.
        // Looking for the next semicolon allows for recovery from an invalid state
        let mut errs = recover_at_token!(tokens, idx, TokenKind::SemiColon);
        errors.append(&mut errs);
    }

    (statement, errors)
}

fn parse_while_loop(tokens: &[Token], idx: &mut usize) -> (ParsedWhileLoop, Vec<ParseError>) {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::While);

    let (condition, mut errs) = parse_expression(tokens, idx);
    errors.append(&mut errs);

    let (body, mut errs) = parse_block(tokens, idx);
    errors.append(&mut errs);

    (ParsedWhileLoop { condition, body }, errors)
}

fn parse_if_else(tokens: &[Token], idx: &mut usize) -> (ParsedIfElse, Vec<ParseError>) {
    let mut errors = vec![];

    expect!(&mut errors, tokens, idx, TokenKind::If);

    let (condition, mut errs) = parse_expression(tokens, idx);
    errors.append(&mut errs);

    let (if_body, mut errs) = parse_block(tokens, idx);
    errors.append(&mut errs);

    let else_body = if matches!(
        tokens.get(*idx),
        Some(Token {
            kind: TokenKind::Else,
            ..
        })
    ) {
        expect!(&mut errors, tokens, idx, TokenKind::Else);

        let (else_body, mut errs) = parse_block(tokens, idx);
        errors.append(&mut errs);

        Some(else_body)
    } else {
        None
    };

    (
        ParsedIfElse {
            condition,
            if_body,
            else_body,
        },
        errors,
    )
}

fn parse_expression(tokens: &[Token], idx: &mut usize) -> (ParsedExpression, Vec<ParseError>) {
    let (expr, mut errors) = loop {
        let mut errors = vec![];
        break match &tokens[*idx] {
            tok @ Token {
                kind: TokenKind::Ident(name),
        } => {
            if let Some(Token {
                kind: TokenKind::OParen,
                ..
            }) = tokens.get(*idx + 1)
            {
                let (func_call, errors) = parse_function_call(tokens, idx);
                (ParsedExpression::FunctionCall(func_call), errors)
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

        let (rhs, mut errs) = parse_expression(tokens, idx);
        errors.append(&mut errs);

        ParsedExpression::BinaryOp(Box::new(expr), Box::new(rhs), op)
    } else {
        expr
    };

    (expr, errors)
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
