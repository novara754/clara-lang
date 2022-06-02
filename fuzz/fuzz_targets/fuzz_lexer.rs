#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let _ = clara::lexer::lex(&data);
});
