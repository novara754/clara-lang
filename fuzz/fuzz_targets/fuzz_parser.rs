#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let (tokens, _) = clara::lexer::lex(&data);
    let _ = clara::parser::parse_program(&tokens, &mut 0);
});
