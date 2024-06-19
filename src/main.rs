mod ast;
mod colors;
mod lexer;
mod parser;
use ast::Program;
use lexer::{Lexer, Token};
use parser::Parser;
// use std::{env, fs};
use std::fs;

fn main() {
    // let args: Vec<String> = env::args().collect();

    let filename: &String = &"./tests/main.nv".to_string();
    let source_code: String = fs::read_to_string(filename).expect("deu bosta");

    let mut lexer: Lexer = Lexer::new(filename);
    let parser: Parser = Parser::new();

    let tokens: Vec<Token> = lexer.tokenize(&source_code);
    let _ast: Program = parser.produce_ast(tokens.clone(), &source_code);

    println!("{:#?}", _ast);
}
