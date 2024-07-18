use crate::ast::*;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Write;

#[derive(Clone)]
pub struct Generator<'a> {
    pub program: Program,
    pub filename: &'a String,
    pub segments: Segments,
    pub assembly: String,
    pub max_local_value: &'a str,
    pub values: HashMap<String, (String, NodeType)>,
    pub strings: HashMap<String, String>,
    pub unitialized_strings_counter: usize,
    pub memory_access: bool,
}

#[derive(Clone)]
pub struct Segments {
    pub data: Vec<String>,
    pub code_main: Vec<String>,
    pub code_other: Vec<String>,
}

#[derive(Clone, Debug)]
enum CallerType {
    Int(i128),
    Id(String),
    Str(String),
}

impl<'a> Generator<'a> {
    pub fn new(tree: Program, filename: &'a &String) -> Generator<'a> {
        let segments = Segments {
            data: vec!["segment readable writeable\n".to_string()],
            code_main: vec![
                "segment readable executable\n".to_string(),
                "main:\n".to_string(),
            ],
            code_other: vec![],
        };

        let assembly: String = String::new();
        let max_local_value: &str = "db";
        let values: HashMap<String, (String, NodeType)> = HashMap::new();
        let strings: HashMap<String, String> = HashMap::new();
        let unitialized_strings_counter: usize = 0;
        let memory_access: bool = false;

        Generator {
            program: tree,
            filename,
            segments,
            assembly,
            max_local_value,
            values,
            strings,
            unitialized_strings_counter,
            memory_access,
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

        let narval_home: String = match env::var("NARVAL_HOME") {
            Ok(val) => val,
            Err(_) => {
                eprintln!("Error: NARVAL_HOME environment variable not set.");
                return;
            }
        };

        self.assembly
            .push_str(format!("include \"{}/libs/standard.s\"\n", narval_home).as_str());
        self.assembly.push_str("\n");

        for code in &self.segments.data {
            self.assembly.push_str(code);
        }

        self.assembly.push_str("\n");

        for code in &self.segments.code_main {
            self.assembly.push_str(code);
        }
        self.assembly.push_str("\tpush 0\n");
        self.assembly.push_str("\tcall exit\n");
        self.assembly.push_str("\n");

        for code in &self.segments.code_other {
            self.assembly.push_str(code);
        }

        let mut file_name: String = self.filename.split("/").last().unwrap().to_owned();
        if file_name.contains(".") {
            file_name = file_name.split(".").next().unwrap().to_owned() + ".asm";
        } else {
            file_name = file_name + ".asm";
        }

        let mut file =
            File::create(file_name).expect("Unable to create intermediate assembly file");
        file.write_all(self.assembly.as_bytes())
            .expect("Unable to write to intermediate assembly file");
    }

    fn generate_call_expr(&mut self, expr: CallExpr) {
        if let Expr::Identifier(Identifier { symbol, .. }) = *expr.caller {
            let caller = symbol;
            let args = expr.args;

            let mut arg_stack: Vec<CallerType> = Vec::new();

            for arg in args {
                match *arg {
                    Expr::StringLiteral(ref str_lit) => {
                        if let Some(existing_identifier) = self.strings.get(&str_lit.value) {
                            arg_stack.push(CallerType::Str(existing_identifier.clone()));
                        } else {
                            let string = format!(
                                "\t__STR_{} db \"{}\", 0x0\n",
                                self.unitialized_strings_counter, str_lit.value
                            );

                            self.segments.data.push(string);
                            let identifier = format!("__STR_{}", self.unitialized_strings_counter);
                            self.strings
                                .insert(str_lit.value.clone(), identifier.clone());
                            arg_stack.push(CallerType::Str(identifier));
                            self.unitialized_strings_counter += 1;
                        }
                    }
                    Expr::NumericLiteral(ref num_lit) => {
                        let value: i128 = num_lit
                            .value
                            .parse()
                            .expect("Unable to parse numeric literal.");
                        arg_stack.push(CallerType::Int(value));
                    }
                    Expr::Identifier(ref id) => {
                        if self.memory_access {
                            let directive = self.values.get(&id.symbol).map(|(v, _)| v.clone());
                            let size = self.return_inverse_constant_directive_size(
                                directive.as_deref().unwrap(),
                            );
                            arg_stack.push(CallerType::Id(format!("{} [{}]", size, id.symbol)))
                        } else {
                            arg_stack.push(CallerType::Id(id.symbol.clone()));
                        }
                    }
                    Expr::CallExpr(ref call_expr) => {
                        self.generate_call_expr(call_expr.clone());

                        arg_stack.push(CallerType::Id("rax".to_string()));
                    }
                    Expr::BinaryExpr(binary_expr) => {
                        self.generate_binary_expr(Expr::BinaryExpr(binary_expr), None);
                    }
                    _ => panic!("Unexpected argument type in call expression."),
                }
            }

            arg_stack.reverse();

            for arg in arg_stack {
                match arg {
                    CallerType::Id(id) => {
                        if self.memory_access {
                            self.memory_access = false;
                            let split_temp: Vec<&str> = id.split('[').collect();
                            let extracted_id = split_temp.last().unwrap().replace("]", "");

                            let directive = self
                                .values
                                .get(&extracted_id)
                                .map(|(d, _)| d.clone())
                                .unwrap();
                            if directive != "db" {
                                let equivalent_directive =
                                    self.return_inverse_constant_directive_size(&directive);
                                self.segments.code_main.push(format!(
                                    "\tpush {} [{}]\n",
                                    equivalent_directive, extracted_id
                                ));
                            } else {
                                self.segments
                                    .code_main
                                    .push(format!("\tpush [{}]\n", extracted_id));
                            }
                        } else {
                            self.segments.code_main.push(format!("\tpush {}\n", id));
                        }
                    }

                    CallerType::Int(int) => {
                        self.segments.code_main.push(format!("\tpush {}\n", int))
                    }
                    CallerType::Str(string) => {
                        self.segments.code_main.push(format!("\tpush {}\n", string))
                    }
                };
            }

            self.segments.code_main.push(format!("\tcall {}\n", caller));
            self.segments.code_main.push("\n".to_string());
        } else {
            panic!("Caller must be an identifier.");
        }
    }

    fn generate_expr(&mut self, expr: Expr) {
        match expr {
            Expr::BinaryExpr(binary_expr) => {
                self.generate_binary_expr(Expr::BinaryExpr(binary_expr), None)
            }
            Expr::NumericLiteral(num_lit) => {
                self.segments
                    .code_main
                    .push(format!("\tmov rax, {}\n", num_lit.value));
            }
            Expr::Identifier(ident) => {
                self.memory_access = true;
                self.segments
                    .code_main
                    .push(format!("\tmovzx rax, [{}]\n", ident.symbol));
            }
            _ => panic!("Unsupported expression type"),
        }
    }

    fn generate_binary_expr(&mut self, expr: Expr, identifier: Option<String>) {
        if let Expr::BinaryExpr(binary_expr) = expr {
            self.generate_expr(*binary_expr.left);

            self.segments.code_main.push("\tmov rbx, rax\n".to_string());

            self.generate_expr(*binary_expr.right);

            match binary_expr.operator.as_str() {
                "+" => self.segments.code_main.push("\tadd rax, rbx\n".to_string()),
                "-" => self.segments.code_main.push("\tsub rax, rbx\n".to_string()),
                "*" => self
                    .segments
                    .code_main
                    .push("\timul rax, rbx\n".to_string()),
                "/" => {
                    self.segments
                        .code_main
                        .push("\txchg rax, rbx\n".to_string());
                    self.segments.code_main.push("\tcqo\n".to_string());
                    self.segments.code_main.push("\tidiv rbx\n".to_string());
                }
                "%" => {
                    self.segments
                        .code_main
                        .push("\txchg rax, rbx\n".to_string());
                    self.segments.code_main.push("\tcqo\n".to_string());
                    self.segments.code_main.push("\tidiv rbx\n".to_string());
                    self.segments.code_main.push("\tmov rax, rdx\n".to_string());
                }
                "<<" => self.segments.code_main.push("\tshl rax, cl\n".to_string()),
                ">>" => self.segments.code_main.push("\tshr rax, cl\n".to_string()),
                "**" => {
                    self.segments.code_main.push("\tmov rcx, rax\n".to_string());
                    self.segments.code_main.push("\tmov rax, 1\n".to_string());
                    self.segments
                        .code_main
                        .push("\ttest rcx, rcx\n".to_string());
                    self.segments.code_main.push("\tjz pow_end\n".to_string());
                    self.segments.code_main.push("pow_loop:\n".to_string());
                    self.segments
                        .code_main
                        .push("\timul rax, rbx\n".to_string());
                    self.segments
                        .code_main
                        .push("\tloop pow_loop\n".to_string());
                    self.segments.code_main.push("pow_end:\n".to_string());
                }
                "\\" => {
                    self.segments
                        .code_main
                        .push("\txchg rax, rbx\n".to_string());
                    self.segments.code_main.push("\tcqo\n".to_string());
                    self.segments.code_main.push("\tidiv rbx\n".to_string());
                }
                _ => (),
            }

            if let Some(id) = identifier {
                if let Some(_) = self.values.get(&id) {
                    self.memory_access = true;
                    self.segments
                        .code_main
                        .push(format!("\tmov [{}], rax\n", id));
                }
            }
        }
    }

    fn generate_var_declaration(&mut self, declaration: VarDeclaration) {
        let identifier: String = declaration.identifier.unwrap();
        let data_size: &str = declaration.data_size.as_str();
        let declaration_value: Expr = *declaration.value;
        let mut not_generated: bool = true;

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
            NodeType::BinaryExpr => {
                self.segments
                    .data
                    .push(format!("\t{} {} 0\n", identifier, directive.clone()));
                let value_to_insert: (String, NodeType) =
                    (directive.clone(), declaration_value.clone().kind());
                self.values.insert(identifier.clone(), value_to_insert);
                self.generate_binary_expr(declaration_value.clone(), Some(identifier.clone()));

                not_generated = false;
                format!("")
            }
            NodeType::CallExpr => {
                if let Expr::CallExpr(call) = declaration_value.clone() {
                    self.generate_call_expr(call);

                    let return_size = self.get_return_size(&directive);
                    self.segments
                        .code_main
                        .push(format!("\tmov [{}], {}\n", identifier, return_size));
                    self.memory_access = true;
                    format!("\t{} {} 0x0\n", identifier, &directive)
                } else {
                    format!("")
                }
            }
            _ => format!(
                "\t{} {} {:?}\n",
                identifier,
                directive,
                declaration_value.clone()
            ),
        };

        if not_generated {
            self.segments.data.push(assembly_line);
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

    fn return_inverse_constant_directive_size(&mut self, data_size: &str) -> String {
        match data_size {
            "db" => String::from("byte"),
            "dw" => String::from("word"),
            "dd" => String::from("dword"),
            "dq" => String::from("qword"),
            _ => String::from("_Invalid"),
        }
    }

    fn get_return_size(&mut self, directive: &str) -> String {
        match directive {
            "db" => String::from("al"),
            "dw" => String::from("ax"),
            "dd" => String::from("eax"),
            "dq" => String::from("rax"),
            _ => String::from("_Invalid"),
        }
    }

    fn get_directive(&self, key: &str) -> Option<String> {
        self.values.get(key).map(|(s, _)| s.clone())
    }
}
