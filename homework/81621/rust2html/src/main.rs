mod lexer;
mod printers;

use lexer::tokenizer::Tokenizer;
use printers::html_printer::tokens_to_html;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).expect("No input filename provided");
    let text = std::fs::read_to_string(filename).expect("Could not read file");
    let mut lexer = Tokenizer::new(text);
    lexer.populate_tokens();
    let html = tokens_to_html(lexer.tokens);
    println!("{}", html);
}
