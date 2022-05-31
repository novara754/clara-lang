use ariadne::Source;
use error::ReportError;

mod error;
mod lexer;
mod parser;
mod span;

fn main() {
    let source_file = std::env::args()
        .nth(1)
        .expect("first program argument should be source file");

    let source = std::fs::read_to_string(source_file)
        .expect("first program argument should be readable source file");

    let (tokens, lex_errors) = lexer::lex(&source);

    for e in &lex_errors {
        e.report().print(Source::from(&source)).unwrap();
    }

    let (program, parse_errors) = parser::parse_program(&tokens, &mut 0);

    for e in &parse_errors {
        e.report().print(Source::from(&source)).unwrap();
    }
}
