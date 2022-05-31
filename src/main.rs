mod codegen;
mod error;
mod lexer;
mod parser;
mod span;
mod typechecker;

use std::{io::Write, path::PathBuf, process::Command};

use ariadne::Source;
use error::ReportError;

fn main() {
    let source_file = PathBuf::from(
        std::env::args()
            .nth(1)
            .expect("first program argument should be source file"),
    );

    let source = std::fs::read_to_string(&source_file)
        .expect("first program argument should be readable source file");

    let (tokens, lex_errors) = lexer::lex(&source);

    for e in &lex_errors {
        e.report().print(Source::from(&source)).unwrap();
    }

    let (program, parse_errors) = parser::parse_program(&tokens, &mut 0);

    for e in &parse_errors {
        e.report().print(Source::from(&source)).unwrap();
    }

    if !lex_errors.is_empty() || !parse_errors.is_empty() {
        std::process::exit(1);
    }

    // dbg!(&program);

    let typecheck_errors = typechecker::typecheck_program(&program);

    for e in &typecheck_errors {
        e.report().print(Source::from(&source)).unwrap();
    }

    if !typecheck_errors.is_empty() {
        std::process::exit(1);
    }

    let mut c_filepath = PathBuf::from("./build");
    c_filepath.push(
        source_file
            .file_stem()
            .unwrap_or_else(|| source_file.file_name().unwrap()),
    );
    c_filepath.set_extension("c");

    let mut exe_filepath = PathBuf::from("./build");
    exe_filepath.push(
        source_file
            .file_stem()
            .unwrap_or_else(|| source_file.file_name().unwrap()),
    );

    let mut output = std::fs::File::create(&c_filepath)
        .expect("should be able to create new file for codegen output");
    codegen::generate_c(&mut output, &program).unwrap();

    let gcc = PathBuf::from("gcc");
    let gcc_stderr = Command::new(gcc)
        .args([
            "-std=c11",
            "-Wall",
            "-Werror",
            "-Wextra",
            "-Wpedantic",
            "-I",
            "./runtime",
            "-o",
            &exe_filepath.clone().into_os_string().into_string().unwrap(),
            &c_filepath.clone().into_os_string().into_string().unwrap(),
        ])
        .output()
        .unwrap()
        .stderr;

    if !gcc_stderr.is_empty() {
        std::io::stderr().write_all(&gcc_stderr).unwrap();
        std::process::exit(1);
    }
}
