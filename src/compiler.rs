use std::{env, process::Command};

pub struct Compiler<'a> {
    pub filename: &'a String,
}

impl<'a> Compiler<'a> {
    pub fn new(filename: &'a String) -> Compiler {
        Compiler { filename }
    }

    pub fn compile(&mut self) {
        let narval_home = match env::var("NARVAL_HOME") {
            Ok(val) => val,
            Err(_) => {
                eprintln!("Error: NARVAL_HOME environment variable not set.");
                return;
            }
        };

        let fasm_path = format!("{}/tools/fasm", narval_home);

        Command::new(&fasm_path).arg(self.filename).output();
    }
}
