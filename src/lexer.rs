use ariadne::{Color, Label, Report, ReportKind};
use serde_json::json;

use crate::{
    error::{JsonError, ReportError},
    span::{Span, Spanned},
};

#[derive(Debug)]
pub enum TokenKind {
    StringLiteral(String),
    IntLiteral(i32),
    Ident(String),
    True,
    False,
    Opaque,
    Struct,
    Extern,
    Fn,
    Let,
    While,
    If,
    Else,
    OParen,
    CParen,
    OBrace,
    CBrace,
    SemiColon,
    Comma,
    Colon,
    RightArrow,
    Equal,
    EqualEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Dot,
    Unknown,
}

impl TokenKind {
    pub fn human_name(&self) -> &'static str {
        use TokenKind::*;
        match *self {
            StringLiteral(_) => "string literal",
            IntLiteral(_) => "integer literal",
            True | False => "boolean literal",
            Ident(_) => "identifier",
            Fn => "`fn` keyword",
            Extern => "`extern` keyword",
            Opaque => "`opaque` keyword",
            Struct => "`struct` keyword",
            Let => "`let` keyword",
            While => "`while` keyword",
            If => "`if` keyword",
            Else => "`else` keyword",
            OParen => "`(`",
            CParen => "`)`",
            OBrace => "`{`",
            CBrace => "`}`",
            SemiColon => "`;`",
            Comma => "`,`",
            Colon => "`:`",
            RightArrow => "`->`",
            Equal => "`=`",
            EqualEqual => "`==`",
            GreaterThan => "`>`",
            GreaterThanEqual => "`>=`",
            LessThan => "`<`",
            LessThanEqual => "`<=`",
            Dot => "`.`",
            Unknown => "unknown token",
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    UnknownToken(char, Span),
    UnterminatedString(Span),
    InvalidInt(Span),
}

impl ReportError for LexError {
    fn report(&self) -> Report<Span> {
        use LexError::*;
        match *self {
            UnknownToken(c, span) => Report::build(ReportKind::Error, (), span.start)
                .with_message(format!("unknown token `{}`", c))
                .with_label(Label::new(span).with_color(Color::Red)),
            UnterminatedString(span) => Report::build(ReportKind::Error, (), span.start)
                .with_message("unterminated string")
                .with_label(
                    Label::new(span)
                        .with_color(Color::Red)
                        .with_message("Each string needs to be terminated with a matching `\"`."),
                ),
            InvalidInt(span) => Report::build(ReportKind::Error, (), span.start)
                .with_message("invalid integer")
                .with_label(
                    Label::new(span)
                        .with_message("value does not fit into signed 32-bit integer")
                        .with_color(Color::Red),
                ),
        }
        .finish()
    }
}

impl JsonError for LexError {
    fn json(&self) -> serde_json::Value {
        use LexError::*;
        match *self {
            UnknownToken(c, span) => json!({
                "message": format!("unknown character `{c}` encountered"),
                "span": span.json(),
            }),
            UnterminatedString(span) => json!({
                "message": "unterminated string",
                "span": span.json(),
            }),
            InvalidInt(span) => json!({
                "message": "invalid integer literal",
                "span": span.json(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub len: usize,
}

impl Token {
    fn new(kind: TokenKind, start: usize, len: usize) -> Self {
        Self { kind, start, len }
    }
}

impl Spanned for Token {
    fn span(&self) -> Span {
        Span {
            start: self.start,
            len: self.len,
        }
    }
}

pub fn lex(source: &str) -> (Vec<Token>, Vec<LexError>) {
    let source = source.as_bytes();
    let mut idx = 0;

    let mut tokens = vec![];
    let mut errors = vec![];

    loop {
        while idx < source.len() && source[idx].is_ascii_whitespace() {
            idx += 1;
        }

        if idx == source.len() {
            break;
        }

        // Identifiers & keywords
        if source[idx].is_ascii_alphabetic() {
            let start = idx;

            while idx < source.len() && (source[idx].is_ascii_alphanumeric() || source[idx] == b'_')
            {
                idx += 1;
            }

            let name = std::str::from_utf8(&source[start..idx]).unwrap();

            let len = idx - start;
            let kind = match name {
                "fn" => TokenKind::Fn,
                "extern" => TokenKind::Extern,
                "opaque" => TokenKind::Opaque,
                "struct" => TokenKind::Struct,
                "let" => TokenKind::Let,
                "while" => TokenKind::While,
                "if" => TokenKind::If,
                "else" => TokenKind::Else,
                "true" => TokenKind::True,
                "false" => TokenKind::False,
                _ => TokenKind::Ident(name.to_owned()),
            };
            tokens.push(Token::new(kind, start, len));

            continue;
        }

        // String literals
        if source[idx] == b'"' {
            let start = idx;
            idx += 1; // Consume opening quote

            while idx < source.len() && source[idx] != b'"' {
                idx += 1;
            }

            if idx == source.len() {
                errors.push(LexError::UnterminatedString(Span::new(start, idx - start)));
            } else {
                idx += 1; // Consume closing quote
            }

            if idx == start + 1 {
                // The string had no content and no closing quote
                tokens.push(Token::new(
                    TokenKind::StringLiteral(String::new()),
                    start,
                    1,
                ));
            } else {
                // +1 and -1 on the bounds to exclude quotation marks
                let string = std::str::from_utf8(&source[(start + 1)..(idx - 1)])
                    .unwrap()
                    .to_owned();

                tokens.push(Token::new(
                    TokenKind::StringLiteral(string),
                    start,
                    idx - start,
                ));
            }

            continue;
        }

        // Integer literals
        if source[idx].is_ascii_digit() {
            let start = idx;
            while idx < source.len() && source[idx].is_ascii_digit() {
                idx += 1;
            }

            let int_value =
                if let Ok(int_value) = std::str::from_utf8(&source[start..idx]).unwrap().parse() {
                    int_value
                } else {
                    errors.push(LexError::InvalidInt(Span::new(start, idx - start)));
                    0
                };

            tokens.push(Token::new(
                TokenKind::IntLiteral(int_value),
                start,
                idx - start,
            ));

            continue;
        }

        let mut unknown_char = |c: char| {
            tokens.push(Token::new(TokenKind::Unknown, idx, 1));
            errors.push(LexError::UnknownToken(c, Span::new(idx, 1)));
        };

        match source[idx] {
            b'(' => tokens.push(Token::new(TokenKind::OParen, idx, 1)),
            b')' => tokens.push(Token::new(TokenKind::CParen, idx, 1)),
            b'{' => tokens.push(Token::new(TokenKind::OBrace, idx, 1)),
            b'}' => tokens.push(Token::new(TokenKind::CBrace, idx, 1)),
            b';' => tokens.push(Token::new(TokenKind::SemiColon, idx, 1)),
            b',' => tokens.push(Token::new(TokenKind::Comma, idx, 1)),
            b':' => tokens.push(Token::new(TokenKind::Colon, idx, 1)),
            b'=' => {
                let token = match source.get(idx + 1) {
                    Some(b'=') => {
                        idx += 1;
                        Token::new(TokenKind::EqualEqual, idx - 1, 2)
                    }
                    _ => Token::new(TokenKind::Equal, idx, 2),
                };
                tokens.push(token);
            }
            b'<' => {
                let token = match source.get(idx + 1) {
                    Some(b'=') => {
                        idx += 1;
                        Token::new(TokenKind::LessThanEqual, idx - 1, 2)
                    }
                    _ => Token::new(TokenKind::LessThan, idx, 2),
                };
                tokens.push(token);
            }
            b'>' => {
                let token = match source.get(idx + 1) {
                    Some(b'=') => {
                        idx += 1;
                        Token::new(TokenKind::GreaterThanEqual, idx - 1, 2)
                    }
                    _ => Token::new(TokenKind::GreaterThan, idx, 2),
                };
                tokens.push(token);
            }
            b'.' => tokens.push(Token::new(TokenKind::Dot, idx, 1)),
            b'-' => match source.get(idx + 1) {
                Some(b'>') => {
                    tokens.push(Token::new(TokenKind::RightArrow, idx, 2));
                    idx += 1;
                }
                _ => unknown_char('-'),
            },
            e => unknown_char(e as char),
        }

        idx += 1;
    }

    (tokens, errors)
}
