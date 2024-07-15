mod ast;
mod checker;
mod code_generator;
mod colors;
mod compiler;
mod datatype;
mod lexer;
mod parser;

use ast::Program;
use checker::Checker;
use code_generator::Generator;
use compiler::Compiler;
use lexer::{Lexer, Token};
use parser::Parser;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename: &String = &args[1].to_string();
    let exp_path: Option<std::borrow::Cow<str>> = shellexpand::full(filename).ok();
    let can_path: Option<std::path::PathBuf> =
        std::fs::canonicalize(exp_path.unwrap().as_ref()).ok();
    let path = &can_path
        .unwrap()
        .into_os_string()
        .into_string()
        .ok()
        .unwrap();

    let full_path: &String = &path;
    let source_code: String = fs::read_to_string(full_path).expect(&filename);

    let mut lexer: Lexer = Lexer::new(&full_path);
    let tokens: Vec<Token> = lexer.tokenize(&source_code);

    let parser: Parser = Parser::new();
    let ast: Program = parser.produce_ast(tokens.clone(), &source_code);

    let mut checker: Checker = Checker::new(ast.clone(), &source_code, &full_path);
    for stmt in ast.clone().body {
        checker.check(stmt);
    }

    let mut generator: Generator = Generator::new(ast.clone(), &full_path);
    generator.generate();

    let mut compiler: Compiler = Compiler::new(&full_path);
    compiler.compile();
}
