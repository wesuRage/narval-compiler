mod code_generator {
    pub mod generator;
    pub mod risc_v;
    pub mod x86_64;
}

mod ast;
mod builtin;
mod checker;
mod colors;
mod compiler;
mod datatype;
mod lexer;
mod parser;

use crate::builtin::utilities;
use ast::Program;
use checker::{Checker, Namespace};
use clap::Parser as ArgParser;
use code_generator::generator::Generator;
use colors::printc;
use compiler::Compiler;
use lexer::{Lexer, Token};
use parser::Parser;
use std::{fs, process::exit};

/// Narval - The compiler for the Narval programming language.
#[derive(ArgParser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    /// Output file
    #[arg(short, long)]
    output: Option<String>,
    /// Architecture to be compiled on:
    /// x86_64, aarch64, risc-v
    #[arg(long, default_value = "x86_64")]
    arch: String,
    /// Keeps the intermediate assembly file
    #[arg(long, action = clap::ArgAction::Count)]
    keep_asm: u8,
    /// Just checks the code, doesn't generate anything
    #[arg(short, long, action = clap::ArgAction::Count)]
    just_check: u8,

    #[arg(short, long)]
    run: String,
}

fn expand_and_canonicalize_path(filename: &str) -> String {
    let exp_path = shellexpand::full(filename);
    match exp_path {
        Ok(expanded_path) => {
            let can_path: Option<std::path::PathBuf> =
                std::fs::canonicalize(expanded_path.as_ref()).ok();
            if let Some(can) = can_path {
                can.into_os_string().into_string().ok().unwrap()
            } else {
                printc("%%rERROR:%%! %%yFile not found.%%!");
                exit(1);
            }
        }
        Err(_) => {
            printc("%%rERROR:%%! %%yFile not found.%%!");
            exit(1);
        }
    }
}

fn main() {
    let cli: Cli = Cli::parse();
    let justcheck = cli.just_check;

    let filename: &String = &cli.run;

    // let filename: &String = &"./tests/main.nv".to_string();

    let full_path: String = expand_and_canonicalize_path(filename);

    let mut source_code: String =
        fs::read_to_string(&full_path).expect(&format!("Could not read file: {}", filename));
    source_code.push('\n');

    let mut lexer: Lexer = Lexer::new(&full_path);
    let tokens: Vec<Token> = lexer.tokenize(&source_code);

    let parser: Parser = Parser::new();
    let ast: Program = parser.produce_ast(tokens.clone(), &source_code);

    let namespace: &mut Vec<Namespace> = &mut vec![Namespace::new()];
    let mut checker: Checker = Checker::new(&ast, namespace, &source_code, &full_path);
    utilities(&mut checker);

    loop {
        for stmt in checker.current_body.2.clone() {
            checker.check(&stmt);
        }

        if checker.bodies.len() == 1 {
            break;
        }
    }
    if justcheck == 1 {
        return;
    }

    let mut generator: Generator = Generator::new(&ast, &full_path, cli.arch);
    // let mut generator: Generator = Generator::new(&ast, &full_path, "x86_64".to_string());
    generator.generate();

    let mut compiler: Compiler = Compiler::new(&full_path, cli.output);
    compiler.compile();
}
