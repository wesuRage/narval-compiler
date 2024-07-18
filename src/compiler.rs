use std::process::Command;

pub struct Compiler<'a> {
    pub filename: &'a String,
}

impl<'a> Compiler<'a> {
    pub fn new(filename: &'a String) -> Compiler {
        Compiler { filename }
    }

    pub fn compile(&mut self) {
        let mut file_name: String = self.filename.split("/").last().unwrap().to_owned();
        if file_name.contains(".") {
            file_name = file_name.clone().split(".").next().unwrap().to_owned() + ".asm";
        } else {
            file_name = file_name.clone().to_owned() + ".asm";
        }

        if self.filename.contains(".") {
            Command::new("/bin/fasm")
                .arg(&file_name)
                .output()
                .expect("Error while compiling assembly file");

            // Command::new("/bin/rm")
            //     .arg(file_name)
            //     .output()
            //     .expect("Error while removing assembly file");
        } else {
            Command::new("/bin/fasm")
                .arg(format!("{}.asm -o {}.o", self.filename, self.filename))
                .output()
                .expect("Error while compiling assembly file");

            // Command::new("/bin/rm")
            //     .arg(file_name)
            //     .output()
            //     .expect("Error while removing assembly file");
        }
    }
}
