use ariadne::Source;
use clap::Parser;
use clara::{
    codegen,
    error::{JsonError, ReportError},
    lexer, parser, typechecker,
};
use std::{
    io::{Read, Write},
    path::PathBuf,
    process::Command,
};

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(long)]
    json_diagnostics: bool,

    #[clap(long)]
    no_emit: bool,

    input: String,
}

fn main() {
    let args = Args::parse();

    let source_filepath = args.input;
    let (source, source_file) = if source_filepath == "-" {
        let mut source = String::new();
        std::io::stdin()
            .read_to_string(&mut source)
            .expect("expected to be able to read source from stdin");
        (source, PathBuf::from("./out"))
    } else {
        (
            std::fs::read_to_string(&source_filepath)
                .expect("first program argument should be readable source file"),
            PathBuf::from(source_filepath),
        )
    };

    let (tokens, lex_errors) = lexer::lex(&source);

    if args.json_diagnostics {
        for e in &lex_errors {
            println!("{}", e.json());
        }
    } else {
        for e in &lex_errors {
            e.report().print(Source::from(&source)).unwrap();
        }
    }

    let (program, parse_errors) = parser::parse_program(&tokens, &mut 0);

    if args.json_diagnostics {
        for e in &parse_errors {
            println!("{}", e.json());
        }
    } else {
        for e in &parse_errors {
            e.report().print(Source::from(&source)).unwrap();
        }
    }

    if !lex_errors.is_empty() || !parse_errors.is_empty() {
        std::process::exit(1);
    }

    let (checked_program, typecheck_errors) = typechecker::typecheck_program(&program);

    if args.json_diagnostics {
        for e in &typecheck_errors {
            println!("{}", e.json());
        }
    } else {
        for e in &typecheck_errors {
            e.report().print(Source::from(&source)).unwrap();
        }
    }

    if args.no_emit {
        std::process::exit(0);
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
    codegen::generate_c(&mut output, &checked_program).unwrap();

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
