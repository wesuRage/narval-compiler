use crate::ast::*;
use crate::colors::printc;
use crate::datatype::*;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Write;

#[derive(Clone)]
pub struct X8664Generator<'a> {
    pub program: Program,
    pub filename: &'a String,
    pub segments: Segments,
    pub assembly: String,
    pub max_local_directive: &'a str,
    pub identifier: HashMap<String, (String, NodeType)>,
    pub strings: HashMap<String, (String, Option<String>)>,
    pub unitialized_strings_counter: usize,
    pub unitialized_integer_counter: usize,
    pub string_pointer_counter: usize,
    pub integer_pointer_counter: usize,
    pub temp_return_counter: usize,
    pub memory_access: bool,
    pub current_string: Option<String>,
    pub current_int: i32,
    pub local_identifier: HashMap<String, String>,
    pub function_parameter: HashMap<String, (String, &'a str)>,
    pub asm_parameters: Vec<&'a str>,
}

#[derive(Clone)]
pub struct Segments {
    pub data: Vec<String>,
    pub code_main: Vec<String>,
    pub code_other: Vec<String>,
}

#[derive(Clone, Debug)]
enum CallerType {
    Int(String),
    Id(String),
    Str(String),
}

impl<'a> X8664Generator<'a> {
    pub fn new(tree: Program, filename: &'a String) -> X8664Generator<'a> {
        let segments = Segments {
            data: vec!["segment readable writeable\n".to_string()],
            code_main: vec![
                "segment readable executable\n".to_string(),
                "main:\n".to_string(),
            ],
            code_other: vec![],
        };

        let assembly: String = String::new();
        let max_local_directive: &str = "db";
        let identifier: HashMap<String, (String, NodeType)> = HashMap::new();
        let strings: HashMap<String, (String, Option<String>)> = HashMap::new();
        let unitialized_strings_counter: usize = 0;
        let unitialized_integer_counter: usize = 0;
        let temp_return_counter: usize = 0;
        let string_pointer_counter: usize = 0;
        let integer_pointer_counter: usize = 0;
        let memory_access: bool = false;
        let current_string: Option<String> = None;
        let current_int: i32 = 0;
        let local_identifier: HashMap<String, String> = HashMap::new();
        let function_parameter: HashMap<String, (String, &'a str)> = HashMap::new();
        let asm_parameters: Vec<&str> = vec![
            "rdi", "rsi", "rdx", "r10", "r8", "r9", "r11", "r12", "r13", "r14", "r15", "rcx", "rbx",
        ];

        X8664Generator {
            program: tree,
            filename,
            segments,
            assembly,
            max_local_directive,
            identifier,
            strings,
            unitialized_strings_counter,
            unitialized_integer_counter,
            temp_return_counter,
            string_pointer_counter,
            integer_pointer_counter,
            memory_access,
            current_string,
            current_int,
            local_identifier,
            function_parameter,
            asm_parameters,
        }
    }

    pub fn generate(&mut self) {
        let mut main: Vec<String> = self.segments.code_main.clone();
        let mut other: Vec<String> = self.segments.code_other.clone();
        for stmt in self.program.clone().body {
            match stmt.expr {
                Some(Expr::VarDeclaration(decl)) => {
                    self.generate_var_declaration(decl, &mut main, "".to_string());
                }
                Some(Expr::FunctionDeclaration(decl)) => {
                    self.generate_function_declaration(*decl, &mut other, "".to_string());
                }
                Some(Expr::CallExpr(expr)) => self.generate_call_expr(expr, &mut main),
                _ => (),
            }
        }

        self.assembly.push_str("format ELF64 executable\n");
        self.assembly.push_str("entry main\n");
        self.assembly.push_str("\n");

        let narval_home: String = match env::var("NARVAL_HOME") {
            Ok(val) => val,
            Err(_) => {
                printc("Error: \"NARVAL_HOME\" environment variable not set.\nCheck installation on: https://github.com/wesuRage/narval-compiler?tab=readme-ov-file#instala%C3%A7%C3%A3o-da-fonte");
                return;
            }
        };

        self.assembly
            .push_str(format!("include \"{}/libs/x86_64/standard.s\"\n", narval_home).as_str());
        self.assembly.push_str("\n");

        for code in &self.segments.data {
            self.assembly.push_str(code);
        }

        self.assembly.push_str("\n");

        for code in &main {
            self.assembly.push_str(code);
        }
        self.assembly.push_str("\tmov rdi, 0\n");
        self.assembly.push_str("\tcall exit\n");
        self.assembly.push_str("\n");

        for code in &other {
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

    fn generate_return_stmt(
        &mut self,
        return_stmt: Option<ReturnStmt>,
        return_size: String,
        scope: &mut Vec<String>,
    ) {
        if let Some(stmt) = return_stmt {
            if let Some(arg) = stmt.argument {
                let directive = match return_size.as_str() {
                    "al" => "db",
                    "ax" => "dw",
                    "eax" => "dd",
                    _ => "dq",
                };

                self.segments.data.push(format!(
                    "\t__TEMP_RETURN_{} {} 0\n",
                    self.temp_return_counter, directive
                ));
                self.identifier.insert(
                    format!("__TEMP_RETURN_{}", self.temp_return_counter),
                    (directive.to_string(), NodeType::VoidLiteral),
                );

                match arg {
                    Expr::Identifier(ident) => {
                        let param: Option<&&str> = self
                            .function_parameter
                            .get(&ident.symbol)
                            .map(|(_, stack)| stack);

                        if let Some(parameter) = param {
                            scope.push(format!(
                                "\tmov [__TEMP_RETURN_{}], {}\n",
                                self.temp_return_counter, parameter
                            ))
                        } else {
                            let local: Option<&String> =
                                self.local_identifier.get(&ident.symbol).map(|id| id);
                            if let Some(id) = local {
                                scope.push(format!("\tmov rax, [{}]\n", id));
                                scope.push(format!(
                                    "\tmov [__TEMP_RETURN_{}], rax\n",
                                    self.temp_return_counter,
                                ))
                            } else {
                                scope.push(format!("\tmov rax, [{}]\n", ident.symbol));
                                scope.push(format!(
                                    "\tmov [__TEMP_RETURN_{}], rax\n",
                                    self.temp_return_counter,
                                ))
                            }
                        }
                    }

                    Expr::NumericLiteral(num) => scope.push(format!(
                        "\tmov [__TEMP_RETURN_{}], {}\n",
                        self.temp_return_counter, num.value
                    )),
                    Expr::BinaryExpr(binop) => self.generate_expr(
                        Expr::BinaryExpr(binop),
                        Some(format!("__TEMP_RETURN_{}", self.temp_return_counter)),
                        scope,
                    ),
                    Expr::CallExpr(call) => {
                        self.generate_call_expr(call, scope);
                        scope.push(format!(
                            "\tmov [__TEMP_RETURN_{}], rax\n",
                            self.temp_return_counter,
                        ))
                    }
                    _ => println!("Expression not supported in return statement: {:?}", arg),
                }
                scope.push(format!(
                    "\tmov {}, [__TEMP_RETURN_{}]\n",
                    return_size, self.temp_return_counter
                ));

                self.temp_return_counter += 1;
            } else {
                scope.push(format!("\tmov {}, 0\n", return_size))
            }
        } else {
            panic!("No return statement found");
        }
    }

    fn generate_function_declaration(
        &mut self,
        declaration: FunctionDeclaration,
        scope: &mut Vec<String>,
        parent: String,
    ) {
        let name: String = declaration.name;
        let body: Vec<Stmt> = declaration.body;
        let parameters: Vec<(String, String, Datatype)> = declaration.parameters;
        let return_size: String = self.get_full_return_size(declaration.return_size.as_str());
        let mut new_scope: Vec<String> = vec![];

        let mut parameter_association: Vec<(String, &str)> = Vec::new();

        if !parent.is_empty() {
            scope.push(format!("{}_{}:\n", parent, name));
            self.local_identifier
                .insert(name.clone(), format!("{}_{}", parent, name));
        } else {
            scope.push(format!("{}:\n", name));
            let directive = self.get_return_size(&declaration.return_size.clone());
            self.identifier
                .insert(name.clone(), (directive, NodeType::FunctionDeclaration));
        }

        let mut param_index: usize = 0;
        for parameter in parameters {
            let directive = self.return_constant_directive_size(parameter.0.as_str());
            let node_type = match parameter.2 {
                Datatype::Integer => NodeType::NumericLiteral,
                Datatype::Decimal => NodeType::NumericLiteral,
                Datatype::Text => NodeType::StringLiteral,
                Datatype::Boolean => NodeType::BooleanLiteral,
                Datatype::Array(_) => NodeType::ArrayExpr,
                Datatype::Object(_) => NodeType::ObjectLiteral,
                Datatype::Tuple(_) => NodeType::TupleLiteral,
                Datatype::Enum(_, _) => NodeType::Enum,
                _ => NodeType::VoidLiteral,
            };

            self.identifier
                .insert(parameter.1.clone(), (directive, node_type));

            parameter_association.push((parameter.1.clone(), self.asm_parameters[param_index]));

            self.function_parameter.insert(
                parameter.1.clone(),
                parameter_association[param_index].clone(),
            );

            param_index += 1;
        }

        for stmt in body {
            match stmt.expr {
                Some(Expr::VarDeclaration(decl)) => {
                    self.generate_var_declaration(decl, &mut new_scope, name.clone());
                }
                // Some(Expr::FunctionDeclaration(decl)) => {
                //     self.generate_function_declaration(*decl, &mut scope, name.clone());
                // }
                Some(Expr::CallExpr(expr)) => self.generate_call_expr(expr, &mut new_scope),
                _ => (),
            }
            if !stmt.return_stmt.is_none() {
                self.generate_return_stmt(stmt.return_stmt, return_size.clone(), &mut new_scope);
            }
        }

        for instruction in new_scope {
            scope.push(instruction);
        }

        scope.push("\n".to_string());
        scope.push("\tret\n".to_string());
        scope.push("\n".to_string());
    }

    fn generate_call_expr(&mut self, expr: CallExpr, scope: &mut Vec<String>) {
        if let Expr::Identifier(Identifier { symbol, .. }) = *expr.caller {
            let caller: String = symbol;
            let args: Vec<Box<Expr>> = expr.args;

            let mut arg_stack: Vec<CallerType> = Vec::new();

            for arg in args {
                match *arg {
                    Expr::StringLiteral(ref str_lit) => {
                        if let Some(existing_identifier) = self.strings.get(&str_lit.value) {
                            arg_stack.push(CallerType::Str(existing_identifier.0.clone()));
                        } else {
                            let string = format!(
                                "\t__STR_{}_PTR db \"{}\"\n\t__STR_{} dq 2, __STR_{}_PTR\n",
                                self.unitialized_strings_counter,
                                str_lit.value,
                                self.unitialized_strings_counter,
                                self.unitialized_strings_counter
                            );

                            self.segments.data.push(string);
                            let identifier = format!("__STR_{}", self.unitialized_strings_counter);
                            self.strings
                                .insert(str_lit.value.clone(), (identifier.clone(), None));
                            arg_stack.push(CallerType::Str(identifier));
                            self.unitialized_strings_counter += 1;
                        }
                    }
                    Expr::NumericLiteral(ref num_lit) => {
                        let value: i128 = num_lit
                            .value
                            .parse()
                            .expect("Unable to parse numeric literal.");

                        let integer = format!(
                            "\t__INT_{}_PTR db {}\n\t__INT_{} db 0, __INT_{}_PTR\n",
                            self.unitialized_integer_counter,
                            value,
                            self.unitialized_integer_counter,
                            self.unitialized_integer_counter
                        );

                        self.segments.data.push(integer);

                        arg_stack.push(CallerType::Int(format!(
                            "__INT_{}",
                            self.unitialized_integer_counter
                        )));
                        self.unitialized_integer_counter += 1;
                    }
                    Expr::Identifier(ref id) => {
                        if self.memory_access {
                            let directive = self.identifier.get(&id.symbol).map(|(v, _)| v.clone());
                            let size = self.return_inverse_constant_directive_size(
                                directive.as_deref().unwrap(),
                            );
                            arg_stack.push(CallerType::Id(format!("{} {}", size, id.symbol)))
                        } else {
                            arg_stack.push(CallerType::Id(id.symbol.clone()));
                        }
                    }
                    Expr::CallExpr(ref call_expr) => {
                        self.generate_call_expr(call_expr.clone(), scope);

                        arg_stack.push(CallerType::Id("rax".to_string()));
                    }
                    Expr::BinaryExpr(binary_expr) => {
                        self.generate_expr(
                            Expr::BinaryExpr(binary_expr),
                            Some("rax".to_string()),
                            scope,
                        );
                    }
                    _ => panic!("Unexpected argument type in call expression."),
                }
            }

            let mut parameter_index: usize = 0;

            for arg in arg_stack {
                match arg {
                    CallerType::Id(id) => scope.push(format!(
                        "\tmov {}, {}\n",
                        self.asm_parameters[parameter_index],
                        id.split(" ").last().unwrap()
                    )),

                    CallerType::Int(int) => scope.push(format!(
                        "\tmov {}, {}\n",
                        self.asm_parameters[parameter_index], int
                    )),
                    CallerType::Str(string) => scope.push(format!(
                        "\tmov {}, {}\n",
                        self.asm_parameters[parameter_index], string
                    )),
                };

                parameter_index += 1;
            }

            scope.push(format!("\tcall {}\n", caller));
            scope.push("\n".to_string());
        } else {
            panic!("Caller must be an identifier.");
        }
    }

    fn generate_expr(&mut self, expr: Expr, identifier: Option<String>, scope: &mut Vec<String>) {
        match expr {
            Expr::BinaryExpr(binary_expr) => {
                let op = binary_expr.operator.as_str();

                //vou fazer prioridade aq
                if op == "**" {
                    self.generate_binary_exponential_expr(binary_expr, identifier, scope);
                } else if op == "*" || op == "/" || op == "\\" || op == "%" {
                    self.generate_binary_multiplicative_expr(binary_expr, identifier, scope);
                } else if op == "+" || op == "-" {
                    self.generate_binary_additive_expr(binary_expr, identifier, scope);
                } else if op == "<<" || op == ">>" {
                    self.generate_binary_bitshift_expr(binary_expr, identifier, scope);
                }
            }
            Expr::NumericLiteral(num) => {
                scope.push(format!("\tmov rax, {}\n", num.value));
                self.current_int = num.value.parse().ok().unwrap();
            }
            Expr::Identifier(ident) => {
                self.memory_access = true;
                let param = self
                    .function_parameter
                    .get(&ident.symbol)
                    .map(|(_, stack)| stack);

                if let Some(parameter) = param {
                    scope.push(format!("\tmov rax, {}\n", parameter))
                } else {
                    let local = self.local_identifier.get(&ident.symbol).map(|id| id);
                    if let Some(id) = local {
                        scope.push(format!("\tmov rax, [{}+8]\n", id));
                        let typ = self.identifier.get(id).map(|(_, t)| t).unwrap().to_owned();
                        if typ != NodeType::BinaryExpr {
                            scope.push("\tmov rax, [rax]\n".to_string());
                        }
                    } else {
                        scope.push(format!("\tmov rax, [{}+8]\n", ident.symbol));
                        let typ = self
                            .identifier
                            .get(&ident.symbol)
                            .map(|(_, t)| t)
                            .unwrap()
                            .to_owned();
                        if typ != NodeType::BinaryExpr {
                            scope.push("\tmov rax, [rax]\n".to_string());
                        }
                    }
                }
            }
            Expr::StringLiteral(string) => {
                let string = format!(
                    "\t__STR_{}_PTR db \"{}\", 0\n\t__STR_{} dq 2, __STR_{}_PTR\n",
                    self.unitialized_strings_counter,
                    string.value,
                    self.unitialized_strings_counter,
                    self.unitialized_strings_counter
                );

                self.segments.data.push(string);
                self.current_string =
                    Some(format!("[__STR_{}+8]", self.unitialized_strings_counter));
                self.unitialized_strings_counter += 1;
            }
            _ => {
                panic!("Unsupported expression type");
            }
        }
    }

    fn generate_binary_bitshift_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        scope: &mut Vec<String>,
    ) {
        self.generate_expr(*expr.left, identifier.clone(), scope);

        scope.push("\tmov rbx, rax\n".to_string());
        self.generate_expr(*expr.right, identifier.clone(), scope);

        match expr.operator.as_str() {
            "<<" => {
                scope.push("\tmov cl, byte [rax]\n".to_string());

                scope.push("\tshl rbx, cl\n".to_string());
            }
            ">>" => {
                scope.push("\tmov cl, byte [rax]\n".to_string());
                scope.push("\tshr rbx, cl\n".to_string());
            }
            _ => (),
        }

        self.generate_mov_identifier(identifier, scope);
    }

    fn generate_mov_identifier(&mut self, identifier: Option<String>, scope: &mut Vec<String>) {
        if let Some(ident) = identifier {
            if let Some(_) = self.identifier.get(&ident) {
                self.memory_access = true;

                let param: Option<&&str> =
                    self.function_parameter.get(&ident).map(|(_, stack)| stack);

                if let Some(parameter) = param {
                    scope.push(format!("\tmov {}, rax\n", parameter))
                } else {
                    let local = self.local_identifier.get(&ident).map(|id| id);
                    if let Some(id) = local {
                        scope.push(format!("\tmov [{}+8], rax\n", id))
                    } else {
                        scope.push(format!("\tmov [{}+8], rax\n", ident))
                    }
                }
            }
        }
    }

    fn generate_binary_additive_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        scope: &mut Vec<String>,
    ) {
        let left = *expr.left;
        let right = *expr.right;
        let are_eq: bool = left.kind() != NodeType::CallExpr && left == right;

        self.generate_expr(left, identifier.clone(), scope);

        if !are_eq {
            scope.push("\tmov rbx, rax\n".to_string());

            self.generate_expr(right, identifier.clone(), scope);
            match expr.operator.as_str() {
                "+" => {
                    scope.push("\tadd rax, rbx\n".to_string());
                    self.generate_mov_identifier(identifier, scope);
                }
                "-" => {
                    scope.push("\tsub rax, rbx\n".to_string());
                    self.generate_mov_identifier(identifier, scope);
                }
                _ => (),
            }
        } else {
            match expr.operator.as_str() {
                "+" => {
                    scope.push("\tadd rax, rax\n".to_string());
                    self.generate_mov_identifier(identifier, scope);
                }
                "-" => {
                    scope.push("\tsub rax, rax\n".to_string());
                    self.generate_mov_identifier(identifier, scope);
                }
                _ => (),
            }
        }
    }

    fn generate_binary_multiplicative_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        scope: &mut Vec<String>,
    ) {
        match expr.operator.as_str() {
            "*" => {
                let typ = match *expr.left {
                    Expr::StringLiteral(_) => "string",
                    _ => "nonstring",
                };

                if typ == "string" {
                    self.generate_expr(*expr.left, identifier.clone(), scope);

                    let s: String = <Option<String> as Clone>::clone(&self.current_string).unwrap();
                    self.generate_expr(*expr.right, identifier.clone(), scope);
                    scope.push(format!("\tmov rdi, rax\n"));
                    scope.push(format!("\tmov rsi, {}\n", s));
                    scope.push("\tcall __txt_repeater\n".to_string());

                    self.generate_mov_identifier(identifier, scope);
                } else {
                    let left = *expr.left;
                    let right = *expr.right;

                    let are_eq: bool = left.kind() != NodeType::CallExpr && left == right;

                    self.generate_expr(left, identifier.clone(), scope);
                    if !are_eq {
                        scope.push("\tmov rbx, rax\n".to_string());
                        self.generate_expr(right, identifier.clone(), scope);
                        scope.push("\timul rax, rbx\n".to_string());
                    } else {
                        scope.push("\timul rax, rax\n".to_string());
                    }

                    self.generate_mov_identifier(identifier, scope);
                }
            }
            "/" => {
                let left = *expr.left;
                let right = *expr.right;

                let are_eq: bool = left.kind() != NodeType::CallExpr && left == right;

                if are_eq {
                    scope.push("\tmov rax, 1\n".to_string());
                } else {
                    self.generate_expr(left, identifier.clone(), scope);
                    scope.push("\tmov rbx, rax\n".to_string());
                    self.generate_expr(right, identifier.clone(), scope);
                    scope.push("\txchg rax, rbx\n".to_string());
                    scope.push("\tcqo\n".to_string());
                    scope.push("\tidiv rbx\n".to_string());
                }
                self.generate_mov_identifier(identifier, scope);
            }
            "%" => {
                let left = *expr.left;
                let right = *expr.right;

                let are_eq: bool = left.kind() != NodeType::CallExpr && left == right;

                if are_eq {
                    scope.push("\txor rax, rax\n".to_string());
                } else {
                    self.generate_expr(left, identifier.clone(), scope);
                    scope.push("\tmov rbx, rax\n".to_string());
                    self.generate_expr(right, identifier.clone(), scope);
                    scope.push("\txchg rax, rbx\n".to_string());
                    scope.push("\tcqo\n".to_string());
                    scope.push("\tidiv rbx\n".to_string());
                    scope.push("\tmov rax, rdx\n".to_string());
                }

                self.generate_mov_identifier(identifier, scope);
            }

            "\\" => {
                let left = *expr.left;
                let right = *expr.right;

                let are_eq: bool = left.kind() != NodeType::CallExpr && left == right;

                if are_eq {
                    scope.push("\tmov rax, 1\n".to_string());
                } else {
                    self.generate_expr(left, identifier.clone(), scope);
                    scope.push("\tmov rbx, rax\n".to_string());
                    self.generate_expr(right, identifier.clone(), scope);
                    scope.push("\txchg rax, rbx\n".to_string());
                    scope.push("\tcqo\n".to_string());
                    scope.push("\tidiv rbx\n".to_string());
                }
                self.generate_mov_identifier(identifier, scope);
            }
            _ => (),
        }
    }

    fn generate_binary_exponential_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        scope: &mut Vec<String>,
    ) {
        let left = *expr.left;
        let right = *expr.right;

        let are_eq: bool = left.kind() != NodeType::CallExpr && left == right;

        self.generate_expr(left, identifier.clone(), scope);
        if !are_eq {
            scope.push("\tmov rbx, rax\n".to_string());
            self.generate_expr(right, identifier.clone(), scope);
            scope.push("\tmov rdi, rbx\n".to_string());
        } else {
            scope.push("\tmov rdi, rax\n".to_string());
        }
        scope.push("\tmov rsi, rax\n".to_string());
        scope.push("\tcall __pow\n".to_string());

        self.generate_mov_identifier(identifier, scope);
    }

    fn generate_bitwisenot_expr(
        &mut self,
        expr: Expr,
        identifier: Option<String>,
        scope: &mut Vec<String>,
    ) {
        if let Expr::UnaryBitwiseNotExpr(exp) = expr {
            self.generate_expr(*exp.operand, identifier, scope);
            scope.push("\tnot rax\n".to_string());
        } else {
            self.generate_expr(expr, identifier, scope);
        }
    }

    fn generate_var_declaration(
        &mut self,
        declaration: VarDeclaration,
        scope: &mut Vec<String>,
        parent: String,
    ) {
        let identifier: String = declaration.identifier.unwrap();
        let data_size: &str = declaration.data_size.as_str();
        let data_type: Datatype = declaration.data_type;
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
                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                        format!(
                            "\t{}_{} {} 1, {}\n",
                            parent, identifier, directive, ident.symbol
                        )
                    } else {
                        format!("\t{} {} 1, {}\n", identifier, directive, ident.symbol)
                    }
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

                    let integer = format!(
                        "\t__INT_{}_PTR db {}\n",
                        self.unitialized_integer_counter, value,
                    );

                    self.segments.data.push(integer);

                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                        let assign = format!(
                            "\t{}_{} {} 0, __INT_{}_PTR\n",
                            parent, identifier, directive, self.unitialized_integer_counter
                        );

                        self.unitialized_integer_counter += 1;

                        assign
                    } else {
                        let assign: String = format!(
                            "\t{} {} 0, __INT_{}_PTR\n",
                            identifier, directive, self.unitialized_integer_counter
                        );

                        self.unitialized_integer_counter += 1;

                        assign
                    }
                } else {
                    format!("")
                }
            }
            NodeType::StringLiteral => {
                if let Expr::StringLiteral(str_lit) = declaration_value.clone() {
                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                        self.segments.data.push(format!(
                            "\t__STR_PTR_{} db \"{}\", 0\n",
                            self.string_pointer_counter, str_lit.value
                        ));
                        let string = format!(
                            "\t{}_{} {} 2, __STR_PTR_{}\n",
                            parent, identifier, directive, self.string_pointer_counter
                        );
                        self.string_pointer_counter += 1;
                        string
                    } else {
                        self.segments.data.push(format!(
                            "\t__STR_PTR_{} db \"{}\", 0\n",
                            self.string_pointer_counter, str_lit.value
                        ));
                        let string = format!(
                            "\t{} {} 2, __STR_PTR_{}\n",
                            identifier, directive, self.string_pointer_counter
                        );

                        self.string_pointer_counter += 1;

                        string
                    }
                } else {
                    format!("")
                }
            }
            NodeType::BooleanLiteral => {
                if let Expr::BooleanLiteral(boolean) = declaration_value.clone() {
                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                        format!(
                            "\t{}_{} {} 3, \"{}\", 0\n",
                            parent, identifier, directive, boolean.value
                        )
                    } else {
                        format!(
                            "\t{} {} 3, \"{}\", 0\n",
                            identifier, directive, boolean.value
                        )
                    }
                } else {
                    format!("")
                }
            }
            NodeType::VoidLiteral => {
                if !parent.is_empty() {
                    self.local_identifier
                        .insert(identifier.clone(), format!("{}_{}", parent, identifier));

                    format!("\t{}_{} {} 4, 0\n", parent, identifier, directive)
                } else {
                    format!("\t{} db 4, 0\n", identifier)
                }
            }
            NodeType::BinaryExpr => {
                let typ = match data_type {
                    Datatype::Text => 2,
                    _ => 0,
                };

                if !parent.is_empty() {
                    self.local_identifier
                        .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                    self.segments.data.push(format!(
                        "\t{}_{} {} {}, 0\n",
                        parent,
                        identifier,
                        directive.clone(),
                        typ,
                    ));
                } else {
                    self.segments.data.push(format!(
                        "\t{} {} {}, 0\n",
                        identifier,
                        directive.clone(),
                        typ
                    ));
                }
                let value_to_insert: (String, NodeType) =
                    (directive.clone(), declaration_value.clone().kind());
                self.identifier.insert(identifier.clone(), value_to_insert);
                self.generate_expr(declaration_value.clone(), Some(identifier.clone()), scope);

                not_generated = false;
                format!("")
            }
            NodeType::CallExpr => {
                if let Expr::CallExpr(call) = declaration_value.clone() {
                    self.generate_call_expr(call, scope);

                    let return_size = self.get_return_size(&directive);
                    scope.push(format!("\tmov [{}+8], {}\n", identifier, return_size));
                    self.memory_access = true;

                    let typ = match data_type {
                        Datatype::Integer => "0",
                        Datatype::Text => "2",
                        Datatype::Boolean => "3",
                        Datatype::Void => "4",
                        _ => "1",
                    };

                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));

                        format!("\t{}_{} {} {}, 0\n", parent, identifier, &directive, typ)
                    } else {
                        format!("\t{} {} {}, 0\n", identifier, &directive, typ)
                    }
                } else {
                    format!("")
                }
            }
            _ => {
                if !parent.is_empty() {
                    format!(
                        "\t{}_{} {} {:?}\n",
                        parent,
                        identifier,
                        directive,
                        declaration_value.clone()
                    )
                } else {
                    format!(
                        "\t{} {} {:?}\n",
                        identifier,
                        directive,
                        declaration_value.clone()
                    )
                }
            }
        };

        if not_generated {
            self.segments.data.push(assembly_line);
            let value_to_insert: (String, NodeType) = (directive, declaration_value.clone().kind());
            self.identifier.insert(identifier.clone(), value_to_insert);
        }
    }

    fn return_expr_content(&mut self, expr: Expr) -> (NodeType, String) {
        match expr {
            Expr::Identifier(val) => (NodeType::Identifier, val.symbol),
            Expr::NumericLiteral(val) => (NodeType::NumericLiteral, val.value.to_string()),
            Expr::StringLiteral(val) => (NodeType::StringLiteral, val.value.to_string()),
            Expr::BooleanLiteral(val) => (NodeType::BooleanLiteral, val.value),
            _ => (NodeType::VoidLiteral, "()".to_string()),
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

    fn get_full_return_size(&mut self, directive: &str) -> String {
        match directive {
            "byte" => String::from("al"),
            "word" => String::from("ax"),
            "dword" => String::from("eax"),
            "qword" => String::from("rax"),
            "auto" => String::from("rax"),
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
        self.identifier.get(key).map(|(s, _)| s.clone())
    }
}
