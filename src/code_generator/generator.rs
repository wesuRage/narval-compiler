use crate::ast::*;
// use crate::code_generator::arm64::Arm64Generator;
// use crate::code_generator::risc_v::RiscVGenerator;
use crate::code_generator::x86_64::X8664Generator;
use crate::colors::printc;

pub struct Generator<'a> {
    tree: &'a Program,
    filename: &'a String,
    arch: String,
}

impl<'a> Generator<'a> {
    pub fn new(tree: &'a Program, filename: &'a String, arch: String) -> Generator<'a> {
        Generator {
            tree,
            filename,
            arch,
        }
    }

    pub fn generate(&mut self) {
        match self.arch.as_str() {
            "x86_64" => {
                let mut generator: X8664Generator =
                    X8664Generator::new(self.tree.to_owned(), self.filename);
                generator.generate();
            }
            // "aarch64" => {
            //     let mut generator = Arm64Generator::new(self.tree.to_owned(), self.filename);
            //     generator.generate();
            // }
            // "risc-v" => {
            //     let mut generator = RiscVGenerator::new(self.tree.to_owned(), self.filename);
            //     generator.generate();
            // }
            _ => printc(
                format!("%%rError:%%! %%yArchitecture \"{}\" not found.", self.arch).as_str(),
            ),
        }
    }
}
