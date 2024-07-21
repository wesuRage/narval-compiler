use std::process::Command;

pub struct Compiler<'a> {
    pub filename: &'a String,
    pub output_file: Option<String>,
}

impl<'a> Compiler<'a> {
    pub fn new(filename: &'a String, output_file: Option<String>) -> Compiler {
        Compiler {
            filename,
            output_file,
        }
    }

    pub fn compile(&mut self) {
        let mut file_name: String = self.filename.split("/").last().unwrap().to_owned();
        if file_name.contains(".") {
            file_name = file_name.split(".").next().unwrap().to_owned() + ".asm";
        } else {
            file_name = file_name.clone() + ".asm";
        }

        let mut fasm_command = Command::new("/bin/fasm");

        if let Some(ref output_file) = self.output_file {
            if output_file != self.filename {
                fasm_command
                    .arg(&file_name)
                    .arg(format!("-o {}", output_file));
            } else {
                fasm_command.arg(&file_name);
            }
        } else {
            fasm_command.arg(&file_name);
        }

        fasm_command
            .output()
            .expect("Error while compiling assembly file");

        // Command::new("/bin/rm")
        //     .arg(file_name)
        //     .output()
        //     .expect("Error while removing assembly file");
    }
}
