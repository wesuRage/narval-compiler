use crate::ast::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

#[derive(Clone)]
pub struct Generator<'a> {
    pub program: Program,
    pub filename: &'a String,
    pub segment: (Vec<String>, Vec<String>, Vec<String>),
    pub assembly: String,
    pub max_local_value: &'a str,
    pub values: HashMap<String, (String, NodeType)>,
    pub unitialized_strings_counter: usize,
}

#[derive(Clone, Debug)]
enum CallerType {
    Int(i128),
    Id(String),
    Str(String),
}

impl<'a> Generator<'a> {
    pub fn new(tree: Program, filename: &'a &String) -> Generator<'a> {
        let segment: (Vec<String>, Vec<String>, Vec<String>) = (
            vec!["segment readable writeable\n".to_string()],
            vec![
                "segment readable executable\n".to_string(),
                "main:\n".to_string(),
            ],
            vec![],
        );

        let assembly: String = String::new();
        let max_local_value = "db";
        let values: HashMap<String, (String, NodeType)> = HashMap::new();
        let unitialized_strings_counter = 0;

        Generator {
            program: tree,
            filename,
            segment,
            assembly,
            max_local_value,
            values,
            unitialized_strings_counter,
        }
    }

    pub fn generate(&mut self) {
        for stmt in self.program.clone().body {
            match stmt.expr {
                Some(Expr::VarDeclaration(decl)) => {
                    self.generate_var_declaration(decl);
                }
                Some(Expr::CallExpr(expr)) => self.generate_call_expr(expr),
                _ => (),
            }
        }

        self.assembly.push_str("format ELF64 executable\n");
        self.assembly.push_str("entry main\n");
        self.assembly.push_str("\n");
        self.assembly
            .push_str("include \"/root/rust/narval/libs/nv/standard.s\"\n");
        self.assembly.push_str("\n");

        let segment_writeable = &self.segment.0;
        let segment_executable_main = &self.segment.1;
        let segment_executable_after_main = &self.segment.2;

        for code in segment_writeable {
            self.assembly.push_str(code);
        }

        self.assembly.push_str("\n");

        for code in segment_executable_main {
            self.assembly.push_str(code);
        }
        self.assembly.push_str("\tpush 0\n");
        self.assembly.push_str("\tcall exit\n");
        self.assembly.push_str("\n");

        for code in segment_executable_after_main {
            self.assembly.push_str(code);
        }

        let mut file_name: String = self.filename.split("/").last().unwrap().to_owned();
        if file_name.contains(".") {
            file_name = file_name.clone().split(".").next().unwrap().to_owned() + ".asm";
        }

        let mut file: File = File::create(file_name).expect("Unable to create intermediate file");
        file.write_fmt(format_args!("{}", self.assembly))
            .expect("Unable to write to intermediate file");
    }

    fn generate_call_expr(&mut self, expr: CallExpr) {
        if let Expr::Identifier(Identifier { symbol, .. }) = *expr.caller {
            let caller: String = symbol;

            let args: Vec<Box<Expr>> = expr.args;

            let mut arg_stack: Vec<CallerType> = Vec::new();

            for arg in args {
                match *arg {
                    Expr::StringLiteral(str_lit) => {
                        let string: String = format!(
                            "\t__STR_{} db \"{}\", 0x0\n",
                            self.unitialized_strings_counter, str_lit.value
                        );

                        self.segment.0.push(string.clone());
                        arg_stack.push(CallerType::Str(format!(
                            "__STR_{}",
                            self.unitialized_strings_counter
                        )));
                        self.unitialized_strings_counter += 1;
                    }
                    Expr::NumericLiteral(num_lit) => {
                        let value: i128 = num_lit
                            .value
                            .parse()
                            .expect("Unable to parse numeric literal.");
                        arg_stack.push(CallerType::Int(value));
                    }
                    Expr::Identifier(id) => {
                        arg_stack.push(CallerType::Id(id.symbol));
                    }
                    _ => arg_stack.push(CallerType::Str(format!("_Invalid"))),
                }
            }

            arg_stack.reverse();

            for arg in arg_stack {
                match arg {
                    CallerType::Id(id) => self.segment.1.push(format!("\tpush {}\n", id)),
                    CallerType::Int(int) => self.segment.1.push(format!("\tpush {}\n", int)),
                    CallerType::Str(string) => self.segment.1.push(format!("\tpush {}\n", string)),
                };
            }

            self.segment.1.push(format!("\tcall {}\n", caller));
            self.segment.1.push("\n".to_string());
        };
    }

    fn generate_var_declaration(&mut self, declaration: VarDeclaration) {
        let identifier: String = declaration.identifier.unwrap();
        let constant: bool = declaration.constant;
        let data_size: &str = declaration.data_size.as_str();
        let declaration_value: Expr = *declaration.value;

        if constant {
            let directive: String = match data_size {
                "auto" => {
                    let content: (NodeType, String) =
                        self.return_expr_content(declaration_value.clone());
                    if let NodeType::NumericLiteral = content.0 {
                        let value: i128 = content.1.parse().unwrap_or_default();

                        if value <= 255 {
                            "db".to_string()
                        } else if value <= 65535 {
                            "dw".to_string()
                        } else if value <= 4294967295 {
                            "dd".to_string()
                        } else {
                            "dq".to_string()
                        }
                    } else {
                        self.get_directive(&content.1).unwrap_or("db".to_string())
                    }
                }
                _ => self.return_constant_directive_size(data_size),
            };

            let assembly_line: String = match declaration_value.clone().kind() {
                NodeType::Identifier => {
                    if let Expr::Identifier(ident) = declaration_value.clone() {
                        format!("\t{} {} {}\n", identifier, directive, ident.symbol)
                    } else {
                        format!("")
                    }
                }
                NodeType::NumericLiteral => {
                    if let Expr::NumericLiteral(num_lit) = declaration_value.clone() {
                        let value: i128 = num_lit
                            .value
                            .parse()
                            .expect("Unable to parse numeric value");
                        format!("\t{} {} {}\n", identifier, directive, value)
                    } else {
                        format!("")
                    }
                }
                NodeType::StringLiteral => {
                    if let Expr::StringLiteral(str_lit) = declaration_value.clone() {
                        format!(
                            "\t{} {} \"{}\", 0x0\n",
                            identifier, directive, str_lit.value
                        )
                    } else {
                        format!("")
                    }
                }
                NodeType::TrueLiteral => {
                    format!("\t{} db 0x1\n", identifier)
                }
                NodeType::FalseLiteral => {
                    format!("\t{} db 0x0\n", identifier)
                }
                NodeType::NullLiteral => {
                    format!("\t{} {} 0\n", identifier, directive)
                }
                _ => format!("\t{} {} {:?}\n", identifier, directive, declaration_value),
            };

            self.segment.0.push(assembly_line);

            let value_to_insert: (String, NodeType) = (directive, declaration_value.clone().kind());
            self.values.insert(identifier.clone(), value_to_insert);
        }
    }

    fn return_expr_content(&mut self, expr: Expr) -> (NodeType, String) {
        match expr {
            Expr::Identifier(val) => (NodeType::Identifier, val.symbol),
            Expr::NumericLiteral(val) => (NodeType::NumericLiteral, val.value.to_string()),
            Expr::StringLiteral(val) => (NodeType::StringLiteral, val.value.to_string()),
            Expr::NullLiteral(val) => (NodeType::NullLiteral, val.value.to_string()),
            Expr::TrueLiteral(val) => (NodeType::TrueLiteral, val.value),
            Expr::FalseLiteral(val) => (NodeType::FalseLiteral, val.value),
            _ => (NodeType::UndefinedLiteral, "undefined".to_string()),
        }
    }

    fn return_constant_directive_size(&mut self, data_size: &str) -> String {
        match data_size {
            "byte" => String::from("db"),
            "word" => String::from("dw"),
            "dword" => String::from("dd"),
            "qword" => String::from("dq"),
            _ => String::from("_Invalid"),
        }
    }

    fn get_directive(&self, key: &str) -> Option<String> {
        self.values.get(key).map(|(s, _)| s.clone())
    }
}
