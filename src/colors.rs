use std::io::{self, Write};

const RESET: &str = "\x1b[0m";
const RED: &str = "\x1b[91m";
const YELLOW: &str = "\x1b[93m";
const BLUE: &str = "\x1b[96m";
const GREEN: &str = "\x1b[92m";

pub fn printc(string: &str) {
    let mut escaped_string = string
        .replace("%%b", BLUE)
        .replace("%%g", GREEN)
        .replace("%%y", YELLOW)
        .replace("%%r", RED)
        .replace("%%!", RESET);
    escaped_string = escape(&escaped_string);
    println!("{}", escaped_string);
    io::stdout().flush().unwrap();
}

pub fn escape(string: &str) -> String {
    string.replace('%', "%%")
}
