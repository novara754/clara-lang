#![feature(box_patterns)]
#![allow(clippy::collapsible_match)]
#![feature(concat_bytes)]

pub mod codegen;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod typechecker;
