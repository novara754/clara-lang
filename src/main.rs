use clap::Parser;
use clara::{codegen, lexer, parser, span::FileId, typechecker};
use std::{io::Read, path::PathBuf};

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
            PathBuf::from(&source_filepath),
        )
    };

    let diagnostics_writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Always,
    );
    let mut files = codespan_reporting::files::SimpleFiles::new();

    let (tokens, lex_errors) = lexer::lex(FileId(0), &source);

    if args.json_diagnostics {
        for e in &lex_errors {
            println!("{}", e.json());
        }
    } else {
        files.add(source_filepath, &source);
        for e in &lex_errors {
            codespan_reporting::term::emit(
                &mut diagnostics_writer.lock(),
                &codespan_reporting::term::Config::default(),
                &files,
                &e.report(),
            )
            .unwrap();
        }
    }

    let (program, parse_errors) = parser::parse_program(&tokens, &mut 0);

    if args.json_diagnostics {
        for e in &parse_errors {
            println!("{}", e.json());
        }
    } else {
        for e in &parse_errors {
            codespan_reporting::term::emit(
                &mut diagnostics_writer.lock(),
                &codespan_reporting::term::Config::default(),
                &files,
                &e.report(),
            )
            .unwrap();
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
            codespan_reporting::term::emit(
                &mut diagnostics_writer.lock(),
                &codespan_reporting::term::Config::default(),
                &files,
                &e.report(),
            )
            .unwrap();
        }
    }

    if args.no_emit {
        std::process::exit(0);
    }

    if !typecheck_errors.is_empty() {
        std::process::exit(1);
    }

    let mut o_filepath = PathBuf::from("./build");
    o_filepath.push(
        source_file
            .file_stem()
            .unwrap_or_else(|| source_file.file_name().unwrap()),
    );
    codegen::generate_executable(&o_filepath, &checked_program).unwrap();
}
