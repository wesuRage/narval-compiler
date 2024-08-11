use crate::ast::*;
use crate::colors::printc;
use crate::datatype::*;
use core::panic;
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
    pub identifier: HashMap<String, (String, NodeType)>,
    pub strings: HashMap<String, (String, Option<String>)>,
    pub unitialized_strings_counter: usize,
    pub unitialized_integer_counter: usize,
    pub unitialized_boolean_counter: usize,
    pub unitialized_array_counter: usize,
    pub unitialized_tuple_counter: usize,
    pub unitialized_void_counter: usize,
    pub string_pointer_counter: usize,
    pub integer_pointer_counter: usize,
    pub temp_return_counter: usize,
    pub memory_access: bool,
    pub current_string: Option<String>,
    pub current_int: i32,
    pub local_identifier: HashMap<String, String>,
    pub function_parameter: HashMap<String, (String, &'a str)>,
    pub asm_parameters: Vec<&'a str>,
    pub for_counter: usize,
    pub for_iterators: HashMap<String, String>,
    pub for_range_is_string: bool,
    pub for_range_is_ascii: bool,
    pub for_range_is_array: bool,
    pub for_iterable_string: String,
    pub for_pointer_counter: usize,
    pub current_for_array: String,
    pub if_counter: usize,
    pub while_counter: usize,
    pub loop_counter: usize,
    pub values: HashMap<String, (String, Datatype)>,
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

        X8664Generator {
            program: tree,
            filename,
            segments,
            assembly: String::new(),
            identifier: HashMap::new(),
            strings: HashMap::new(),
            unitialized_strings_counter: 0,
            unitialized_integer_counter: 0,
            unitialized_boolean_counter: 0,
            unitialized_void_counter: 0,
            unitialized_array_counter: 0,
            unitialized_tuple_counter: 0,
            temp_return_counter: 0,
            string_pointer_counter: 0,
            integer_pointer_counter: 0,
            memory_access: false,
            current_string: None,
            current_int: 0,
            local_identifier: HashMap::new(),
            function_parameter: HashMap::new(),
            asm_parameters: vec![
                "rdi", "rsi", "rdx", "r10", "r8", "r9", "r11", "r12", "r13", "r14", "r15", "rcx",
                "rbx",
            ],
            for_counter: 0,
            for_iterators: HashMap::new(),
            for_range_is_string: false,
            for_range_is_ascii: false,
            for_range_is_array: false,
            for_iterable_string: "".to_string(),
            for_pointer_counter: 0,
            current_for_array: "".to_string(),
            if_counter: 0,
            while_counter: 0,
            loop_counter: 0,
            values: HashMap::new(),
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
                Some(Expr::ForStmt(statement)) => self.generate_for_stmt(statement, &mut main),
                Some(Expr::IfStmt(statement)) => self.generate_if_stmt(
                    *statement,
                    true,
                    "".to_string(),
                    &mut main,
                    "".to_string(),
                    "".to_string(),
                    0,
                ),
                Some(Expr::LoopStmt(inconditional_loop)) => {
                    self.generate_loop_stmt(inconditional_loop, &mut main)
                }
                Some(Expr::WhileStmt(while_loop)) => {
                    self.generate_while_stmt(*while_loop, &mut main)
                }
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
                    // Expr::TupleLiteral(tup) => self.generate_tuple_literal(tup),
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
                Some(Expr::ForStmt(s)) => self.generate_for_stmt(s, &mut new_scope),
                Some(Expr::IfStmt(s)) => self.generate_if_stmt(
                    *s,
                    true,
                    "".to_string(),
                    &mut new_scope,
                    "".to_string(),
                    "".to_string(),
                    0,
                ),
                Some(Expr::WhileStmt(w)) => self.generate_while_stmt(*w, &mut new_scope),
                Some(Expr::LoopStmt(l)) => self.generate_loop_stmt(l, scope),
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

                        let integer: String = format!(
                            "\t__INT_{} dq 0, {}\n",
                            self.unitialized_integer_counter, value,
                        );

                        self.segments.data.push(integer);

                        arg_stack.push(CallerType::Int(format!(
                            "__INT_{}",
                            self.unitialized_integer_counter
                        )));
                        self.unitialized_integer_counter += 1;
                    }
                    Expr::Identifier(ref expr_identifer) => {
                        if self.memory_access {
                            let directive: Option<String> = self
                                .identifier
                                .get(&expr_identifer.symbol)
                                .map(|(v, _)| v.clone());
                            let size = self.return_inverse_constant_directive_size(
                                directive.as_deref().unwrap_or("qword"),
                            );
                            let raw_pointers: Option<&String> =
                                self.strings.get(&expr_identifer.symbol).map(|(v, _)| v);

                            if let Some(ptr) = raw_pointers {
                                arg_stack.push(CallerType::Id(format!("{} {}", size, ptr)))
                            } else {
                                if let Some(ident) = Some(expr_identifer.symbol.clone()) {
                                    if let Some(_) = self.identifier.get(&ident) {
                                        self.memory_access = true;

                                        let param: Option<&&str> = self
                                            .function_parameter
                                            .get(&ident)
                                            .map(|(_, stack)| stack);

                                        if let Some(parameter) = param {
                                            arg_stack.push(CallerType::Id(format!("{}", parameter)))
                                        } else {
                                            let local =
                                                self.local_identifier.get(&ident).map(|id| id);
                                            if let Some(id) = local {
                                                arg_stack.push(CallerType::Id(format!("{}", id)))
                                            } else {
                                                arg_stack.push(CallerType::Id(format!("{}", ident)))
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            let raw_pointers =
                                self.strings.get(&expr_identifer.symbol).map(|(v, _)| v);

                            if let Some(reg) = raw_pointers {
                                arg_stack.push(CallerType::Id(format!("{}", reg)))
                            } else {
                                if let Some(ident) = Some(expr_identifer.symbol.clone()) {
                                    if let Some(_) = self.identifier.get(&ident) {
                                        self.memory_access = true;

                                        let param: Option<&&str> = self
                                            .function_parameter
                                            .get(&ident)
                                            .map(|(_, stack)| stack);

                                        if let Some(parameter) = param {
                                            arg_stack.push(CallerType::Id(format!("{}", parameter)))
                                        } else {
                                            let local =
                                                self.local_identifier.get(&ident).map(|id| id);
                                            if let Some(id) = local {
                                                arg_stack.push(CallerType::Id(format!("{}", id)))
                                            } else {
                                                arg_stack.push(CallerType::Id(format!("{}", ident)))
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Expr::CallExpr(ref call_expr) => {
                        scope.push("\tpush r15\n".to_string());
                        scope.push("\tmov r15, 1\n".to_string());
                        self.generate_call_expr(call_expr.clone(), scope);
                        scope.push("\tpop r15\n".to_string());

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
            Expr::UnaryMinusExpr(e) => {
                let expr = *e.operand;
                self.generate_expr(expr, None, scope);
                scope.push("\tneg rax\n".to_string())
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
                if let Some(id) = identifier {
                    if id.as_str() == ".range" {
                        let raw_value = string.value.chars();
                        for ascii in raw_value {
                            scope.push(format!("\tmov rax, {}\n", ascii as u8));
                        }
                        self.for_range_is_string = false;
                        self.for_range_is_ascii = true;
                    }
                } else {
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
                    let local: Option<&String> = self.local_identifier.get(&ident).map(|id| id);
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

                        if let Some((val, typ)) =
                            self.values.get(&ident.symbol).map(|(v, t)| (v, t))
                        {
                            let dat_type = match typ {
                                &Datatype::Integer => "0",
                                &Datatype::Text => "2",
                                &Datatype::Boolean => "3",
                                &Datatype::Array(_) => "5",
                                &Datatype::Tuple(_) => "6",
                                _ => "4",
                            };
                            format!("\t{}_{} {}, {}\n", parent, identifier, dat_type, val)
                        } else {
                            panic!("Value not found in identifier: {}", ident.symbol)
                        }
                    } else {
                        if let Some((val, typ)) =
                            self.values.get(&ident.symbol).map(|(v, t)| (v, t))
                        {
                            let dat_type = match typ {
                                &Datatype::Integer => "0",
                                &Datatype::Text => "2",
                                &Datatype::Boolean => "3",
                                &Datatype::Array(_) => "5",
                                &Datatype::Tuple(_) => "6",
                                _ => "4",
                            };
                            format!("\t{} {}, {}\n", identifier, dat_type, val)
                        } else {
                            panic!("Value not found in identifier: {}", ident.symbol)
                        }
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

                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                        let assign: String =
                            format!("\t{}_{} {} 0, {}\n", parent, identifier, directive, value);
                        self.values.insert(
                            format!("{}_{}", parent, identifier),
                            (value.to_string(), Datatype::Integer),
                        );
                        self.unitialized_integer_counter += 1;

                        self.values.insert(
                            format!("{}_{}", parent, identifier),
                            (value.to_string(), Datatype::Integer),
                        );

                        assign
                    } else {
                        let assign: String =
                            format!("\t{} {} 0, {}\n", identifier, directive, value);
                        self.values.insert(
                            format!("{}", identifier),
                            (value.to_string(), Datatype::Integer),
                        );
                        self.unitialized_integer_counter += 1;

                        self.values
                            .insert(identifier.clone(), (value.to_string(), Datatype::Integer));

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

                        self.values.insert(
                            format!("{}_{}", parent, identifier),
                            (str_lit.value, Datatype::Text),
                        );
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

                        self.values
                            .insert(identifier.clone(), (str_lit.value, Datatype::Text));
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

                        self.values.insert(
                            format!("{}_{}", parent, identifier.clone()),
                            ("0".to_string(), Datatype::Boolean),
                        );
                        format!(
                            "\t{}_{} {} 3, \"{}\", 0\n",
                            parent, identifier, directive, boolean.value
                        )
                    } else {
                        self.values
                            .insert(identifier.clone(), ("0".to_string(), Datatype::Boolean));
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

                    self.values
                        .insert(identifier.clone(), ("0".to_string(), Datatype::Void));
                    format!("\t{}_{} {} 4, 0\n", parent, identifier, directive)
                } else {
                    self.values
                        .insert(identifier.clone(), ("0".to_string(), Datatype::Void));
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
                    scope.push("\tpush r15\n".to_string());
                    scope.push("\tmov r15, 0\n".to_string());
                    self.generate_call_expr(call, scope);
                    scope.push("\tpop r15\n".to_string());

                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                    }

                    let cloned = self.local_identifier.clone();
                    let new_id: &String;
                    {
                        new_id = {
                            if let Some(id) = self.strings.get(&identifier).map(|(name, _)| name) {
                                id
                            } else if let Some(id) =
                                self.identifier.get(&identifier).map(|(name, _)| name)
                            {
                                id
                            } else if let Some(id) = cloned.get(&identifier) {
                                id
                            } else if let Some(id) = self
                                .function_parameter
                                .get(&identifier)
                                .map(|(name, _)| name)
                            {
                                id
                            } else {
                                &identifier
                            }
                        };
                    }

                    scope.push(format!("\tmov [{}+8], rax\n", new_id));

                    self.memory_access = true;

                    let typ = match data_type {
                        Datatype::Integer => "0",
                        Datatype::Text => "2",
                        Datatype::Boolean => "3",
                        Datatype::Void => "4",
                        Datatype::Array(_) => "5",
                        Datatype::Tuple(_) => "6",
                        _ => "1",
                    };

                    self.segments
                        .data
                        .push(format!("\t{} dq {}, 0\n", new_id, typ));
                    format!("")
                } else {
                    format!("")
                }
            }
            NodeType::ArrayExpr => {
                if let Expr::ArrayExpr(array) = declaration_value.clone() {
                    let elements = array.elements;
                    let mut elements_str = Vec::new();

                    for (index, element) in elements.iter().enumerate() {
                        match element {
                            Expr::StringLiteral(s) => {
                                let string_label =
                                    format!("__STR_{}", self.unitialized_strings_counter);
                                let string_def =
                                    format!("\t{} db \"{}\", 0x0\n", string_label, s.value);
                                self.segments.data.push(string_def);

                                let array_elem_data = format!(
                                    "\t__array_{}_{} dq 2, {}\n",
                                    identifier, index, string_label
                                );

                                let array_elem = format!("__array_{}_{}", identifier, index);

                                elements_str.push(array_elem);
                                self.segments.data.push(array_elem_data);
                                self.unitialized_strings_counter += 1;
                            }
                            Expr::NumericLiteral(n) => {
                                let array_elem_data = format!(
                                    "\t__array_{}_{} dq 0, {}\n",
                                    identifier, index, n.value
                                );

                                let array_elem = format!("__array_{}_{}", identifier, index);

                                elements_str.push(array_elem);
                                self.segments.data.push(array_elem_data);
                            }
                            Expr::Identifier(id) => {
                                let ident = self.search_symbol(&id.symbol);
                                if let Some((value, typ)) =
                                    self.values.get(&ident).map(|(v, t)| (v, t))
                                {
                                    let (placeholder, dat_type) = match typ {
                                        &Datatype::Integer => {
                                            let data = (
                                                format!(
                                                    "__INT_{}",
                                                    self.unitialized_integer_counter
                                                ),
                                                "0",
                                            );
                                            self.unitialized_integer_counter += 1;
                                            data
                                        }
                                        &Datatype::Text => {
                                            let data = (
                                                format!(
                                                    "__STR_{}",
                                                    self.unitialized_strings_counter
                                                ),
                                                "2",
                                            );
                                            self.unitialized_strings_counter += 1;
                                            data
                                        }
                                        &Datatype::Boolean => {
                                            let data = (
                                                format!(
                                                    "__BOOL_{}",
                                                    self.unitialized_boolean_counter
                                                ),
                                                "3",
                                            );
                                            self.unitialized_boolean_counter += 1;
                                            data
                                        }
                                        &Datatype::Array(_) => {
                                            let data = (
                                                format!("__ARR_{}", self.unitialized_array_counter),
                                                "5",
                                            );
                                            self.unitialized_array_counter += 1;
                                            data
                                        }
                                        &Datatype::Tuple(_) => {
                                            let data = (
                                                format!("__TUP_{}", self.unitialized_tuple_counter),
                                                "6",
                                            );
                                            self.unitialized_tuple_counter += 1;
                                            data
                                        }
                                        _ => {
                                            let data = (
                                                format!("__VOID_{}", self.unitialized_void_counter),
                                                "4",
                                            );
                                            self.unitialized_void_counter += 1;
                                            data
                                        }
                                    };

                                    let ptr: String;
                                    if dat_type == "2" {
                                        ptr = format!("\t{} db \"{}\", 0x0\n", placeholder, value);
                                    } else {
                                        ptr = format!("\t{} db {}\n", placeholder, value);
                                    };

                                    let array_elem_data = format!(
                                        "\t__array_{}_{} dq {}, {}\n",
                                        identifier, index, dat_type, placeholder
                                    );

                                    let array_elem = format!("__array_{}_{}", identifier, index);

                                    self.segments.data.push(ptr);
                                    self.segments.data.push(array_elem_data);
                                    elements_str.push(array_elem);
                                } else {
                                    panic!("Identifier not found: {}", identifier)
                                };
                            }
                            _ => println!("Element type not supported: {:?}", element),
                        }
                    }

                    let array_def =
                        format!("\t{} dq 5, {}, 0\n", identifier, elements_str.join(", "));
                    self.segments.data.push(array_def);
                    self.identifier
                        .insert(identifier.clone(), ("dq".to_string(), NodeType::ArrayExpr));
                    format!("")
                } else {
                    format!("")
                }
            }
            NodeType::TupleLiteral => {
                if let Expr::TupleLiteral(tuple) = declaration_value.clone() {
                    let elements = tuple.value;
                    format!("")
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

    fn get_directive(&mut self, key: &str) -> Option<String> {
        self.identifier.get(key).map(|(s, _)| s.clone())
    }

    fn generate_for_sequence(&mut self, sequence: Expr, scope: &mut Vec<String>) {
        match sequence {
            Expr::StringLiteral(s) => {
                let string: String = format!(
                    "\t__STR_{}_PTR db \"{}\", 0\n\t__STR_{} dq 2, __STR_{}_PTR\n",
                    self.unitialized_strings_counter,
                    s.value,
                    self.unitialized_strings_counter,
                    self.unitialized_strings_counter
                );

                self.segments.data.push(string);
                scope.push(format!(
                    "\tmov rdi, __STR_{}\n",
                    self.unitialized_strings_counter
                ));
                self.for_iterable_string = format!("__STR_{}", self.unitialized_strings_counter);

                scope.push("\tcall len\n".to_string());
                scope.push(format!("\txor rcx, rcx\n"));
                self.unitialized_strings_counter += 1;
                self.for_range_is_string = true;
                self.for_range_is_ascii = false;
                self.for_range_is_array = false;
            }
            Expr::NumericLiteral(e) => {
                let value: String = e.value;
                scope.push(format!("\tmov rax, {value}\n"));
                scope.push(format!("\txor rcx, rcx\n"));
                self.for_range_is_string = false;
                self.for_range_is_ascii = false;
                self.for_range_is_array = false;
            }
            Expr::RangeExpr(r) => {
                let start: Expr = r.start;
                let range: &str = r.range.as_str();
                let end: Expr = r.end;

                self.generate_expr(start, Some(".range".to_string()), scope);
                scope.push("\tmov rcx, rax\n".to_string());
                self.generate_expr(end, Some(".range".to_string()), scope);

                if range == "..=" {
                    scope.push("\tinc rax\n".to_string())
                }
                self.for_range_is_string = false;
                self.for_range_is_ascii = false;
                self.for_range_is_array = false;
            }
            Expr::Identifier(id) => {
                let identifier = id.symbol;
                let cloned = self.local_identifier.clone();
                let new_id: &String;
                {
                    new_id = {
                        if let Some(id) = self.strings.get(&identifier).map(|(name, _)| name) {
                            id
                        } else if let Some(id) = cloned.get(&identifier) {
                            id
                        } else if let Some(id) = self
                            .function_parameter
                            .get(&identifier)
                            .map(|(name, _)| name)
                        {
                            id
                        } else {
                            &identifier
                        }
                    };
                }

                let typ = self
                    .identifier
                    .get(new_id)
                    .map(|(_, t)| t)
                    .unwrap_or(&NodeType::ArrayExpr)
                    .to_owned();

                match typ {
                    NodeType::ArrayExpr => {
                        scope.push("\tmov rax, 0\n".to_string());
                        scope.push("\tmov rcx, rax\n".to_string());
                        scope.push(format!("\tmov rdi, {}\n", new_id));
                        scope.push("\tcall arrlen\n".to_string());
                        scope.push("\tmov rbx, 4\n".to_string());
                        scope.push("\tdiv rbx\n".to_string());
                        scope.push("\tinc rax\n".to_string());
                        self.current_for_array = new_id.to_owned();
                        self.for_range_is_array = true;
                        self.for_range_is_string = false;
                        self.for_range_is_ascii = false;
                    }
                    _ => println!("Type not supported on sequence"),
                }
            }
            _ => println!("Expression not supported on sequence: {:?}", sequence),
        }
    }

    fn generate_continue_expr(&mut self, counter: usize, parent: String, scope: &mut Vec<String>) {
        if parent.as_str().starts_with("__flow.for_") {
            scope.push("\tpop rax\n".to_string());
            scope.push("\tpop rcx\n".to_string());
            scope.push("\tinc rcx\n".to_string());
            scope.push(format!("\tjmp __flow.for_{}_loop\n", counter));
        } else if parent.as_str().starts_with("__flow.while_") {
            scope.push(format!("\tjmp __flow.while_{}\n", counter));
        } else {
            scope.push(format!("\tjmp __flow.loop_{}\n", counter));
        }
    }

    fn generate_for_stmt(&mut self, stmt: ForStmt, scope: &mut Vec<String>) {
        let items: Vec<String> = stmt.items;
        let sequence: Expr = *stmt.sequence;
        let body: Vec<Stmt> = stmt.body;
        let current_for: usize = self.for_counter;
        let label = format!("__flow.for_{}:\n", current_for);
        scope.push(label.clone());

        if items.len() == 1 {
            self.generate_for_sequence(sequence, scope);

            if self.for_range_is_array {
                self.strings.insert(
                    items[0].clone(),
                    (
                        format!("[__for_pointer_{}]", self.for_pointer_counter),
                        None,
                    ),
                );
            } else {
                self.strings.insert(
                    items[0].clone(),
                    (format!("__for_pointer_{}", self.for_pointer_counter), None),
                );
            }
        }

        scope.push(format!("__flow.for_{}_loop:\n", current_for));
        scope.push("\tcmp rcx, rax\n".to_string());
        scope.push(format!("\tje __flow.for_{}_end\n", current_for));
        scope.push("\tpush rcx\n".to_string());
        scope.push("\tpush rax\n".to_string());

        if self.for_range_is_ascii {
            self.segments.data.push(format!(
                "\t__for_pointer_{} dq 0, 0, 0\n",
                self.for_pointer_counter
            ));
            scope.push(format!(
                "\tmov qword [__for_pointer_{}], 0x2\n",
                self.for_pointer_counter
            ));
            scope.push(format!(
                "\tmov qword [__for_pointer_{}+8], rcx\n",
                self.for_pointer_counter
            ));
            scope.push(format!(
                "\tmov qword [__for_pointer_{}+16], 0x0\n",
                self.for_pointer_counter
            ));
            self.for_pointer_counter += 1;
        } else if self.for_range_is_string {
            self.segments.data.push(format!(
                "__for_pointer_{} dq 0, 0, 0",
                self.for_pointer_counter
            ));
            scope.push(format!(
                "\tmov qword [__for_pointer_{}], 0x2\n",
                self.for_pointer_counter
            ));
            scope.push(format!("\tmov rdx, [{}+8]\n", self.for_iterable_string));
            scope.push("\tmovzx rdx, byte [rdx+rcx]\n".to_string());
            scope.push(format!(
                "\tmov qword [__for_pointer_{}+8], rdx\n",
                self.for_pointer_counter
            ));
            scope.push(format!(
                "\tmov qword [__for_pointer_{}+16], 0x0\n",
                self.for_pointer_counter
            ));

            self.for_pointer_counter += 1;
        } else if self.for_range_is_array {
            self.segments.data.push(format!(
                "\t__for_pointer_{} dq 0\n",
                self.for_pointer_counter
            ));
            scope.push("\tinc rcx\n".to_string());
            scope.push(format!("\tmov rcx, [{}+rcx*8]\n", self.current_for_array));
            scope.push(format!(
                "\tmov [__for_pointer_{}], rcx\n",
                self.for_pointer_counter
            ));

            self.for_pointer_counter += 1;
        } else {
            self.segments.data.push(format!(
                "\t__for_pointer_{} dq 0, 0\n",
                self.for_pointer_counter
            ));
            scope.push(format!(
                "\tmov qword [__for_pointer_{}+8], rcx\n",
                self.for_pointer_counter
            ));

            self.for_pointer_counter += 1;
        }

        scope.push("\n".to_string());

        for stmt in body {
            match stmt.expr {
                Some(Expr::VarDeclaration(decl)) => {
                    self.generate_var_declaration(
                        decl,
                        scope,
                        format!("__for_{}", self.for_counter),
                    );
                }
                Some(Expr::CallExpr(expr)) => self.generate_call_expr(expr, scope),

                Some(Expr::ForStmt(s)) => {
                    self.for_counter += 1;
                    self.generate_for_stmt(s, scope)
                }
                Some(Expr::BreakExpr(_)) => {
                    scope.push(format!("\tjmp __flow.for_{}_end\n", current_for))
                }
                Some(Expr::ContinueExpr(_)) => {
                    self.generate_continue_expr(current_for, label.clone(), scope)
                }
                Some(Expr::IfStmt(s)) => self.generate_if_stmt(
                    *s,
                    true,
                    "".to_string(),
                    scope,
                    format!("__flow.for_{}_end", current_for),
                    format!("__flow.for_{}_loop", current_for),
                    current_for,
                ),
                Some(Expr::WhileStmt(w)) => self.generate_while_stmt(*w, scope),
                Some(Expr::LoopStmt(l)) => self.generate_loop_stmt(l, scope),
                _ => (),
            }
        }

        scope.push("\n".to_string());
        scope.push("\tpop rax\n".to_string());
        scope.push("\tpop rcx\n".to_string());
        scope.push("\tinc rcx\n".to_string());
        scope.push(format!("\tjmp __flow.for_{}_loop\n", current_for));
        scope.push(format!("__flow.for_{}_end:\n", current_for));
        self.for_counter += 1;
    }

    fn search_symbol(&self, symbol: &String) -> String {
        if let Some(s) = self.strings.get(symbol.as_str()).map(|(name, _)| name) {
            s.to_owned()
        } else if let Some(s) = self
            .function_parameter
            .get(symbol.as_str())
            .map(|(name, _)| name)
        {
            s.to_owned()
        } else if let Some(s) = self.local_identifier.get(symbol.as_str()) {
            s.to_owned()
        } else {
            symbol.to_string()
        }
    }
    fn generate_test_expr(&mut self, expr: Expr, scope: &mut Vec<String>) {
        match expr {
            Expr::BinaryExpr(binary_expr) => {
                println!("{:?}", binary_expr);
                unimplemented!()
            }
            Expr::NumericLiteral(num) => {
                let value: i32 = num.value.parse().ok().unwrap();
                scope.push(format!("\tmov rax, {value}\n"));
            }
            Expr::Identifier(ident) => {
                scope.push(format!(
                    "\tmov rax, [{}+8]\n",
                    self.search_symbol(&ident.symbol)
                ));
            }
            Expr::StringLiteral(string) => {
                scope.push(format!("\tmov rax, [{}+8]\n", string.value));
                scope.push("\tmov rax, [rax]\n".to_string());
            }
            _ => {
                panic!("Unsupported expression type");
            }
        }
    }

    fn generate_test(&mut self, test: Expr, scope: &mut Vec<String>) -> String {
        match test {
            Expr::LogicalNotExpr(e) => {
                return self.generate_test(Expr::LogicalNotExpr(e), scope);
            }
            Expr::BinaryExpr(e) => {
                let left = *e.left;
                let right = *e.right;
                let operator = &*e.operator;
                match operator {
                    "==" => {
                        self.generate_test_expr(left, scope);
                        scope.push("\tmov rbx, rax\n".to_string());
                        self.generate_test_expr(right, scope);
                        scope.push("\tcmp rax, rbx\n".to_string());
                        return "ne".to_string();
                    }
                    "!=" => {
                        self.generate_test_expr(left, scope);
                        scope.push("\tmov rbx, rax\n".to_string());
                        self.generate_test_expr(right, scope);
                        scope.push("\tcmp rax, rbx\n".to_string());
                        return "e".to_string();
                    }
                    "<" => {
                        self.generate_test_expr(left, scope);
                        scope.push("\tmov rbx, rax\n".to_string());
                        self.generate_test_expr(right, scope);
                        scope.push("\txchg rax, rbx\n".to_string());
                        scope.push("\tcmp rax, rbx\n".to_string());
                        return "g".to_string();
                    }
                    ">" => {
                        self.generate_test_expr(left, scope);
                        scope.push("\tmov rbx, rax\n".to_string());
                        self.generate_test_expr(right, scope);
                        scope.push("\tcmp rax, rbx\n".to_string());
                        return "l".to_string();
                    }
                    _ => println!("Unsupported operator: {operator}"),
                }
            }
            _ => println!("Expression not supported: {:?}", test),
        }
        return "".to_string();
    }

    fn generate_if_stmt(
        &mut self,
        stmt: IfStmt,
        hasend: bool,
        label: String,
        scope: &mut Vec<String>,
        breakable: String,
        continuable: String,
        parent_counter: usize,
    ) {
        let test = *stmt.test;
        let consequent = stmt.consequent;
        let alternate = stmt.alternate;

        let op = self.generate_test(test, scope);
        let label_consequent = format!("__flow.if_alternate_{}", self.if_counter);
        let label_end: String = if hasend {
            format!("__flow.if_end_{}", self.if_counter)
        } else {
            label
        };

        let mut hasret: bool = false;
        self.if_counter += 1;

        scope.push(format!("\tj{} {}\n", op, label_consequent));
        for s in consequent {
            match s.expr {
                Some(Expr::VarDeclaration(v)) => {
                    self.generate_var_declaration(v, scope, "".to_string())
                }
                Some(Expr::CallExpr(c)) => self.generate_call_expr(c, scope),
                Some(Expr::FunctionDeclaration(f)) => {
                    self.generate_function_declaration(*f, scope, "".to_string())
                }
                Some(Expr::IfStmt(s)) => {
                    let mut _value = self.generate_if_stmt(
                        *s,
                        true,
                        label_end.clone(),
                        scope,
                        breakable.clone(),
                        continuable.clone(),
                        parent_counter.clone(),
                    );
                }
                Some(Expr::ForStmt(s)) => {
                    self.for_counter += 1;
                    self.generate_for_stmt(s, scope)
                }
                Some(Expr::BreakExpr(_)) => scope.push(format!("\tjmp {breakable}\n")),
                Some(Expr::ContinueExpr(_)) => {
                    self.generate_continue_expr(parent_counter.clone(), continuable.clone(), scope)
                }

                Some(Expr::WhileStmt(w)) => self.generate_while_stmt(*w, scope),
                Some(Expr::LoopStmt(l)) => self.generate_loop_stmt(l, scope),
                _ => (),
            }
            hasret = s.return_stmt.is_some();
            if hasret {
                self.generate_return_stmt(s.return_stmt, "auto".to_string(), scope);
            }
        }
        if !hasret && hasend {
            scope.push(format!("\tjmp {}\n", label_end.as_str()));
        }
        scope.push(format!("{}:\n", label_consequent));

        if let Some(alt) = alternate {
            for s in alt {
                match s.expr {
                    Some(Expr::VarDeclaration(v)) => {
                        self.generate_var_declaration(v, scope, "".to_string())
                    }
                    Some(Expr::CallExpr(c)) => self.generate_call_expr(c, scope),
                    Some(Expr::IfStmt(s)) => self.generate_if_stmt(
                        *s,
                        true,
                        label_end.clone(),
                        scope,
                        breakable.clone(),
                        continuable.clone(),
                        parent_counter.clone(),
                    ),
                    Some(Expr::ForStmt(s)) => {
                        self.for_counter += 1;
                        self.generate_for_stmt(s, scope)
                    }
                    Some(Expr::BreakExpr(_)) => scope.push(format!("\tjmp {breakable}\n")),
                    Some(Expr::ContinueExpr(_)) => scope.push(format!("\tjmp {}\n", continuable)),
                    Some(Expr::WhileStmt(w)) => self.generate_while_stmt(*w, scope),
                    Some(Expr::LoopStmt(l)) => self.generate_loop_stmt(l, scope),
                    _ => (),
                }
                if s.return_stmt.is_some() {
                    self.generate_return_stmt(s.return_stmt, "auto".to_string(), scope)
                }
            }
        }

        if !hasret && hasend {
            scope.push(format!("{}:\n", label_end.as_str()));
        }
    }

    fn generate_loop_stmt(&mut self, stmt: LoopStmt, scope: &mut Vec<String>) {
        let body = stmt.body;
        let current_loop = self.loop_counter;
        let label = format!("__flow.loop_{}", current_loop);
        let label_exit = format!("__flow.loop_{}_end", current_loop);

        scope.push(format!("\t{}:\n", label));

        for statement in body {
            match statement.expr {
                Some(Expr::VarDeclaration(v)) => {
                    self.generate_var_declaration(v, scope, format!("{}", label))
                }
                Some(Expr::CallExpr(c)) => self.generate_call_expr(c, scope),
                Some(Expr::IfStmt(si)) => self.generate_if_stmt(
                    *si,
                    false,
                    "".to_string(),
                    scope,
                    format!("{}", label_exit),
                    label.clone(),
                    current_loop,
                ),
                Some(Expr::WhileStmt(w)) => self.generate_while_stmt(*w, scope),
                Some(Expr::BreakExpr(_)) => scope.push(format!("\tjmp {}\n", label_exit)),
                Some(Expr::ContinueExpr(_)) => {
                    self.generate_continue_expr(current_loop.clone(), label.clone(), scope)
                }
                Some(Expr::ForStmt(s)) => {
                    self.for_counter += 1;
                    self.generate_for_stmt(s, scope)
                }
                Some(Expr::LoopStmt(l)) => self.generate_loop_stmt(l, scope),
                _ => (),
            }
        }

        scope.push(format!("\tjmp {}\n", label));
        scope.push(format!("\t{}:\n", label_exit))
    }

    fn generate_while_stmt(&mut self, stmt: WhileStmt, scope: &mut Vec<String>) {
        let test: Expr = stmt.condition;
        let body: Vec<Stmt> = stmt.body;
        let current_while: usize = self.while_counter;
        let label: String = format!("__flow.while_{}", current_while);
        let label_exit: String = format!("__flow.while_{}_end", current_while);
        scope.push(format!("{}:\n", label.as_str()));
        if let Expr::BooleanLiteral(ref b) = test {
            if b.value == "false" {
                let op = self.generate_test(test, scope);
                scope.push(format!("\tj{} {}\n", op, label_exit.as_str()));
            }
        } else {
            let op = self.generate_test(test, scope);
            scope.push(format!("\tj{} {}\n", op, label_exit.as_str()));
        }
        self.while_counter += 1;

        for statement in body {
            match statement.expr {
                Some(Expr::VarDeclaration(v)) => {
                    self.generate_var_declaration(v, scope, format!("{}", label))
                }
                Some(Expr::CallExpr(c)) => self.generate_call_expr(c, scope),
                Some(Expr::IfStmt(si)) => self.generate_if_stmt(
                    *si,
                    false,
                    "".to_string(),
                    scope,
                    format!("{}", label_exit),
                    label.clone(),
                    current_while,
                ),
                Some(Expr::WhileStmt(w)) => self.generate_while_stmt(*w, scope),
                Some(Expr::BreakExpr(_)) => scope.push(format!("\tjmp {}\n", label_exit)),
                Some(Expr::ContinueExpr(_)) => {
                    self.generate_continue_expr(current_while.clone(), label.clone(), scope)
                }
                Some(Expr::ForStmt(s)) => {
                    self.for_counter += 1;
                    self.generate_for_stmt(s, scope)
                }
                Some(Expr::LoopStmt(l)) => self.generate_loop_stmt(l, scope),
                _ => (),
            }

            if statement.return_stmt.is_some() {
                self.generate_return_stmt(statement.return_stmt, "auto".to_string(), scope)
            }
        }

        scope.push(format!("\tjmp {}\n", label.as_str()));
        scope.push(format!("{}:\n", label_exit.as_str()))
    }

    fn generate_tuple_literal(&mut self, tup: TupleLiteral, name: String, scope: &mut Vec<String>) {
        let tname = if name.is_empty() {
            format!("__tuple_{}", self.unitialized_tuple_counter)
        } else {
            format!("__tuple_{}", name)
        };
        let length = tup.value.len();
        let mut data = &mut self.segments.data;
        let mut t: Vec<String> = Vec::new();
        for i in 0..length {
            let s = format!("{}_{}", tname, i);
            t.push(s);
        }

        for n in t {
            data.push(format!("{} dq 1, 0\n", n));
        }

        // data.push(format!("{} dq 6, "));

        // for n in t {
        //     let comma = data.push
        // }
    }
}
