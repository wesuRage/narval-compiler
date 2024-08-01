use std::io::{self, Write};

const RESET: &str = "\x1b[0m";
const RED: &str = "\x1b[91m";
const YELLOW: &str = "\x1b[93m";
const BLUE: &str = "\x1b[96m";
const GREEN: &str = "\x1b[92m";

pub fn printc(string: &str) {
    let escaped_string = string
        .replace("%%b", BLUE)
        .replace("%%g", GREEN)
        .replace("%%y", YELLOW)
        .replace("%%r", RED)
        .replace("%%!", RESET);
    println!("{}", escaped_string);
    io::stdout().flush().unwrap();
}
