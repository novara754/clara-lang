mod lexer;

fn main() {
    let source_file = std::env::args()
        .nth(1)
        .expect("first program argument should be source file");

    let source = std::fs::read_to_string(source_file)
        .expect("first program argument should be readable source file");

    let (tokens, errors) = lexer::lex(&source);
    dbg!(tokens, errors);
}
