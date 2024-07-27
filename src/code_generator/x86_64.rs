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
    pub memory_access: bool,
    pub current_string: Option<String>,
    pub current_int: i32,
    pub local_identifier: HashMap<String, String>,
    pub function_parameter: HashMap<String, (String, &'a str)>,
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
        let memory_access: bool = false;
        let current_string: Option<String> = None;
        let current_int: i32 = 0;
        let local_identifier: HashMap<String, String> = HashMap::new();
        let function_parameter: HashMap<String, (String, &'a str)> = HashMap::new();

        X8664Generator {
            program: tree,
            filename,
            segments,
            assembly,
            max_local_directive,
            identifier,
            strings,
            unitialized_strings_counter,
            memory_access,
            current_string,
            current_int,
            local_identifier,
            function_parameter,
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
        self.assembly.push_str("\tpush 0\n");
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
        segment: &mut Vec<String>,
    ) {
        if let Some(stmt) = return_stmt {
            if let Some(arg) = stmt.argument {
                let directive = match return_size.as_str() {
                    "al" => "db",
                    "ax" => "dw",
                    "eax" => "dd",
                    _ => "dq",
                };

                self.segments
                    .data
                    .push(format!("\t__TEMP_RETURN {} 0\n", directive));
                self.identifier.insert(
                    "__TEMP_RETURN".to_string(),
                    (directive.to_string(), NodeType::VoidLiteral),
                );

                match arg {
                    Expr::Identifier(ident) => {
                        let param: Option<&&str> = self
                            .function_parameter
                            .get(&ident.symbol)
                            .map(|(_, stack)| stack);

                        if let Some(parameter) = param {
                            segment.push(format!("\tmov [__TEMP_RETURN], {}\n", parameter))
                        } else {
                            let local: Option<&String> =
                                self.local_identifier.get(&ident.symbol).map(|id| id);
                            if let Some(id) = local {
                                segment.push(format!("\tmov rax, [{}]\n", id));
                                segment.push("\tmov [__TEMP_RETURN], rax\n".to_string())
                            } else {
                                segment.push(format!("\tmov rax, [{}]\n", ident.symbol));
                                segment.push("\tmov [__TEMP_RETURN], rax\n".to_string())
                            }
                        }
                    }

                    Expr::NumericLiteral(num) => {
                        segment.push(format!("\tmov [__TEMP_RETURN], {}\n", num.value))
                    }
                    Expr::BinaryExpr(binop) => self.generate_expr(
                        Expr::BinaryExpr(binop),
                        Some("__TEMP_RETURN".to_string()),
                        segment,
                    ),
                    _ => panic!("Expression not supported in return statement"),
                }
                segment.push(format!("\tmov {}, [__TEMP_RETURN]\n", return_size))
            } else {
                segment.push(format!("\tmov {}, 0\n", return_size))
            }
        } else {
            panic!("No return statement found");
        }
    }

    fn generate_function_declaration(
        &mut self,
        declaration: FunctionDeclaration,
        segment: &mut Vec<String>,
        parent: String,
    ) {
        let name: String = declaration.name;
        let body: Vec<Stmt> = declaration.body;
        let parameters: Vec<(String, String, Datatype)> = declaration.parameters;
        let return_size: String = self.get_full_return_size(declaration.return_size.as_str());
        let mut scope: Vec<String> = vec![];
        let mut parameters_swap: Vec<String> = vec![];
        let asm_parameters: Vec<&str> = vec![
            "rdi", "rsi", "rdx", "rcx", "rbx", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
        ];
        let mut parameter_association: Vec<(String, &str)> = Vec::new();
        let mut rbp: i32 = 16;

        if !parent.is_empty() {
            segment.push(format!("{}_{}:\n", parent, name));
            self.local_identifier
                .insert(name.clone(), format!("{}_{}", parent, name));
        } else {
            segment.push(format!("{}:\n", name));
            let directive = self.get_return_size(&declaration.return_size.clone());
            self.identifier
                .insert(name.clone(), (directive, NodeType::FunctionDeclaration));
        }

        segment.push("\tpush rbp\n".to_string());
        segment.push("\tmov rbp, rsp\n".to_string());
        segment.push("\n".to_string());

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

            parameter_association.push((parameter.1.clone(), asm_parameters[param_index]));

            self.function_parameter.insert(
                parameter.1.clone(),
                parameter_association[param_index].clone(),
            );

            parameters_swap.push(format!(
                "\tmov {}, [rbp+{}]\n",
                asm_parameters[param_index],
                rbp.clone()
            ));

            param_index += 1;
            rbp += 8;
        }

        for parameter in parameters_swap {
            segment.push(parameter);
        }

        for stmt in body {
            match stmt.expr {
                Some(Expr::VarDeclaration(decl)) => {
                    self.generate_var_declaration(decl, &mut scope, name.clone());
                }
                // Some(Expr::FunctionDeclaration(decl)) => {
                //     self.generate_function_declaration(*decl, &mut scope, name.clone());
                // }
                Some(Expr::CallExpr(expr)) => self.generate_call_expr(expr, &mut scope),
                _ => (),
            }
            if !stmt.return_stmt.is_none() {
                self.generate_return_stmt(stmt.return_stmt, return_size.clone(), &mut scope);
            }
        }

        for instruction in scope {
            segment.push(instruction);
        }

        segment.push("\n".to_string());
        segment.push("\tmov rsp, rbp\n".to_string());
        segment.push("\tpop rbp\n".to_string());
        segment.push("\tret\n".to_string());
        segment.push("\n".to_string());
    }

    fn generate_call_expr(&mut self, expr: CallExpr, segment: &mut Vec<String>) {
        if let Expr::Identifier(Identifier { symbol, .. }) = *expr.caller {
            let caller = symbol;
            let args = expr.args;

            let mut arg_stack: Vec<CallerType> = Vec::new();

            for arg in args {
                match *arg {
                    Expr::StringLiteral(ref str_lit) => {
                        if let Some(existing_identifier) = self.strings.get(&str_lit.value) {
                            arg_stack.push(CallerType::Str(existing_identifier.0.clone()));
                        } else {
                            let string = format!(
                                "\t__STR_{} db \"{}\", 0x0\n",
                                self.unitialized_strings_counter, str_lit.value
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
                        arg_stack.push(CallerType::Int(value));
                    }
                    Expr::Identifier(ref id) => {
                        if self.memory_access {
                            let directive = self.identifier.get(&id.symbol).map(|(v, _)| v.clone());
                            let size = self.return_inverse_constant_directive_size(
                                directive.as_deref().unwrap(),
                            );
                            arg_stack.push(CallerType::Id(format!("{} [{}]", size, id.symbol)))
                        } else {
                            arg_stack.push(CallerType::Id(id.symbol.clone()));
                        }
                    }
                    Expr::CallExpr(ref call_expr) => {
                        self.generate_call_expr(call_expr.clone(), segment);

                        arg_stack.push(CallerType::Id("rax".to_string()));
                    }
                    Expr::BinaryExpr(binary_expr) => {
                        self.generate_expr(
                            Expr::BinaryExpr(binary_expr),
                            Some("rax".to_string()),
                            segment,
                        );
                    }
                    _ => panic!("Unexpected argument type in call expression."),
                }
            }

            for arg in arg_stack {
                match arg {
                    CallerType::Id(id) => {
                        if self.memory_access {
                            self.memory_access = false;
                            let split_temp: Vec<&str> = id.split('[').collect();
                            let extracted_id = split_temp.last().unwrap().replace("]", "");

                            let directive = self
                                .identifier
                                .get(&extracted_id)
                                .map(|(d, _)| d.clone())
                                .unwrap();
                            if directive != "db" {
                                let equivalent_directive =
                                    self.return_inverse_constant_directive_size(&directive);
                                segment.push(format!(
                                    "\tpush {} [{}]\n",
                                    equivalent_directive, extracted_id
                                ));
                            } else {
                                segment.push(format!("\tpush [{}]\n", extracted_id));
                            }
                        } else {
                            segment.push(format!("\tpush {}\n", id));
                        }
                    }

                    CallerType::Int(int) => segment.push(format!("\tpush {}\n", int)),
                    CallerType::Str(string) => segment.push(format!("\tpush {}\n", string)),
                };
            }

            segment.push(format!("\tcall {}\n", caller));
            segment.push("\n".to_string());
        } else {
            panic!("Caller must be an identifier.");
        }
    }

    fn generate_expr(&mut self, expr: Expr, identifier: Option<String>, segment: &mut Vec<String>) {
        match expr {
            Expr::BinaryExpr(binary_expr) => {
                let op = binary_expr.operator.as_str();

                //vou fazer prioridade aq
                if op == "**" {
                    self.generate_binary_exponential_expr(binary_expr, identifier, segment);
                } else if op == "*" || op == "/" || op == "\\" || op == "%" {
                    self.generate_binary_multiplicative_expr(binary_expr, identifier, segment);
                } else if op == "+" || op == "-" {
                    self.generate_binary_additive_expr(binary_expr, identifier, segment);
                } else if op == "<<" || op == ">>" {
                    self.generate_binary_bitshift_expr(binary_expr, identifier, segment);
                }
            }
            Expr::NumericLiteral(num) => {
                segment.push(format!("\tmov rax, {}\n", num.value));
                self.current_int = num.value.parse().ok().unwrap();
            }
            Expr::Identifier(ident) => {
                self.memory_access = true;
                let param = self
                    .function_parameter
                    .get(&ident.symbol)
                    .map(|(_, stack)| stack);

                if let Some(parameter) = param {
                    segment.push(format!("\tmov rax, {}\n", parameter))
                } else {
                    let local = self.local_identifier.get(&ident.symbol).map(|id| id);
                    if let Some(id) = local {
                        segment.push(format!("\tmovzx rax, [{}]\n", id))
                    } else {
                        segment.push(format!("\tmovzx rax, [{}]\n", ident.symbol))
                    }
                }
            }
            Expr::StringLiteral(string) => {
                let string = format!(
                    "\t__STR_{} db \"{}\", 0x0\n",
                    self.unitialized_strings_counter, string.value
                );

                self.segments.data.push(string);
                self.current_string = Some(format!("__STR_{}", self.unitialized_strings_counter));
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
        segment: &mut Vec<String>,
    ) {
        self.generate_expr(*expr.left, identifier.clone(), segment);
        segment.push("\tmov rbx, rax\n".to_string());
        self.generate_expr(*expr.right, identifier.clone(), segment);

        match expr.operator.as_str() {
            "<<" => {
                segment.push("\tmov cl, byte [rax]\n".to_string());
                segment.push("\tshl rbx, cl\n".to_string());
            }
            ">>" => {
                segment.push("\tmov cl, byte [rax]\n".to_string());
                segment.push("\tshr rbx, cl\n".to_string());
            }
            _ => (),
        }

        self.generate_mov_identifier(identifier, segment);
    }

    fn generate_mov_identifier(&mut self, identifier: Option<String>, segment: &mut Vec<String>) {
        if let Some(ident) = identifier {
            if let Some(_) = self.identifier.get(&ident) {
                self.memory_access = true;

                let param: Option<&&str> =
                    self.function_parameter.get(&ident).map(|(_, stack)| stack);

                if let Some(parameter) = param {
                    segment.push(format!("\tmov {}, rax\n", parameter))
                } else {
                    let local = self.local_identifier.get(&ident).map(|id| id);
                    if let Some(id) = local {
                        segment.push(format!("\tmov [{}], rax\n", id))
                    } else {
                        segment.push(format!("\tmov [{}], rax\n", ident))
                    }
                }
            }
        }
    }

    fn generate_binary_additive_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        segment: &mut Vec<String>,
    ) {
        self.generate_expr(*expr.left, identifier.clone(), segment);

        segment.push("\tmov rbx, rax\n".to_string());

        self.generate_expr(*expr.right, identifier.clone(), segment);

        match expr.operator.as_str() {
            "+" => {
                segment.push("\tadd rax, rbx\n".to_string());
                self.generate_mov_identifier(identifier, segment);
            }
            "-" => {
                segment.push("\tsub rax, rbx\n".to_string());
                self.generate_mov_identifier(identifier, segment);
            }
            _ => (),
        }
    }

    fn generate_binary_multiplicative_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        segment: &mut Vec<String>,
    ) {
        match expr.operator.as_str() {
            "*" => unsafe {
                if expr.typ.as_ptr().as_ref().unwrap().to_owned().unwrap() == Datatype::Text {
                    self.generate_expr(*expr.left, identifier.clone(), segment);

                    let s: String = <Option<String> as Clone>::clone(&self.current_string).unwrap();
                    self.generate_expr(*expr.right, identifier.clone(), segment);
                    segment.push(format!("\tpush {}\n", s));
                    segment.push(format!("\tpush rax\n"));
                    segment.push("\ncall __txt_repeater\n".to_string());

                    self.generate_mov_identifier(identifier, segment);
                } else {
                    self.generate_expr(*expr.left, identifier.clone(), segment);
                    segment.push("\tmov rbx, rax\n".to_string());
                    self.generate_expr(*expr.right, identifier.clone(), segment);
                    segment.push("\timul rax, rbx\n".to_string());

                    self.generate_mov_identifier(identifier, segment);
                }
            },
            "/" => {
                self.generate_expr(*expr.left.clone(), identifier.clone(), segment);
                segment.push("\tmov rbx, rax\n".to_string());
                self.generate_expr(*expr.right.clone(), identifier.clone(), segment);
                segment.push("\txchg rax, rbx\n".to_string());
                segment.push("\tcqo\n".to_string());
                segment.push("\tidiv rbx\n".to_string());

                self.generate_mov_identifier(identifier, segment);
            }
            "%" => {
                self.generate_expr(*expr.left.clone(), identifier.clone(), segment);
                segment.push("\tmov rbx, rax\n".to_string());
                self.generate_expr(*expr.right.clone(), identifier.clone(), segment);
                segment.push("\txchg rax, rbx\n".to_string());
                segment.push("\tcqo\n".to_string());
                segment.push("\tidiv rbx\n".to_string());
                segment.push("\tmov rax, rdx\n".to_string());

                self.generate_mov_identifier(identifier, segment);
            }

            "\\" => {
                self.generate_expr(*expr.left.clone(), identifier.clone(), segment);
                segment.push("\tmov rbx, rax\n".to_string());
                self.generate_expr(*expr.right.clone(), identifier.clone(), segment);
                segment.push("\txchg rax, rbx\n".to_string());
                segment.push("\tcqo\n".to_string());
                segment.push("\tidiv rbx\n".to_string());

                self.generate_mov_identifier(identifier, segment);
            }
            _ => (),
        }
    }

    fn generate_binary_exponential_expr(
        &mut self,
        expr: BinaryExpr,
        identifier: Option<String>,
        segment: &mut Vec<String>,
    ) {
        self.generate_expr(*expr.left, identifier.clone(), segment);
        segment.push("\tmov rbx, rax\n".to_string());
        self.generate_expr(*expr.right, identifier.clone(), segment);

        segment.push("\tpush rax\n".to_string());
        segment.push("\tpush rbx\n".to_string());
        segment.push("\tcall __pow\n".to_string());

        self.generate_mov_identifier(identifier, segment);
    }

    fn generate_bitwisenot_expr(
        &mut self,
        expr: Expr,
        identifier: Option<String>,
        segment: &mut Vec<String>,
    ) {
        if let Expr::UnaryBitwiseNotExpr(exp) = expr {
            self.generate_expr(*exp.operand, identifier, segment);
            segment.push("\tnot rax\n".to_string());
        } else {
            self.generate_expr(expr, identifier, segment);
        }
    }

    fn generate_var_declaration(
        &mut self,
        declaration: VarDeclaration,
        segment: &mut Vec<String>,
        parent: String,
    ) {
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
                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                        format!(
                            "\t{}_{} {} {}\n",
                            parent, identifier, directive, ident.symbol
                        )
                    } else {
                        format!("\t{} {} {}\n", identifier, directive, ident.symbol)
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
                        format!("\t{}_{} {} {}\n", parent, identifier, directive, value)
                    } else {
                        format!("\t{} {} {}\n", identifier, directive, value)
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
                        format!(
                            "\t{}_{} {} \"{}\", 0x0\n",
                            parent, identifier, directive, str_lit.value
                        )
                    } else {
                        format!(
                            "\t{} {} \"{}\", 0x0\n",
                            identifier, directive, str_lit.value
                        )
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
                            "\t{}_{} {} \"{}\", 0x0\n",
                            parent, identifier, directive, boolean.value
                        )
                    } else {
                        format!(
                            "\t{} {} \"{}\", 0x0\n",
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

                    format!("\t{}_{} {} 0\n", parent, identifier, directive)
                } else {
                    format!("\t{} db 0\n", identifier)
                }
            }
            NodeType::BinaryExpr => {
                if !parent.is_empty() {
                    self.local_identifier
                        .insert(identifier.clone(), format!("{}_{}", parent, identifier));
                    self.segments.data.push(format!(
                        "\t{}_{} {} 0\n",
                        parent,
                        identifier,
                        directive.clone()
                    ));
                } else {
                    self.segments
                        .data
                        .push(format!("\t{} {} 0\n", identifier, directive.clone()));
                }
                let value_to_insert: (String, NodeType) =
                    (directive.clone(), declaration_value.clone().kind());
                self.identifier.insert(identifier.clone(), value_to_insert);
                self.generate_expr(declaration_value.clone(), Some(identifier.clone()), segment);

                not_generated = false;
                format!("")
            }
            NodeType::CallExpr => {
                if let Expr::CallExpr(call) = declaration_value.clone() {
                    self.generate_call_expr(call, segment);

                    let return_size = self.get_return_size(&directive);
                    segment.push(format!("\tmov [{}], {}\n", identifier, return_size));
                    self.memory_access = true;

                    if !parent.is_empty() {
                        self.local_identifier
                            .insert(identifier.clone(), format!("{}_{}", parent, identifier));

                        format!("\t{}_{} {} 0x0\n", parent, identifier, &directive)
                    } else {
                        format!("\t{} {} 0x0\n", identifier, &directive)
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
            // mais ou menos quanto tempo até voce voltar? (responda no zap)
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
