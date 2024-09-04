use crate::ast::*;
use crate::colors::printc;
use crate::datatype::*;
use crate::lexer::{Token, TokenType};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    errstate: bool,
    lines: Option<Vec<String>>,
    index: usize,
    in_function_declaration_state: bool,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            tokens: Vec::new(),
            errstate: false,
            lines: None,
            index: 0,
            in_function_declaration_state: false,
        }
    }

    fn not_eof(&self) -> bool {
        self.at().token_type != TokenType::Eof
    }

    fn at(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn eat(&mut self) -> Token {
        let current_index = self.index;
        self.index += 1;
        self.tokens[current_index].clone()
    }

    fn next(&self) -> &Token {
        if self.tokens.get(self.index + 1).is_none() {
            return self.at();
        }

        self.tokens.get(self.index + 1).unwrap()
    }

    fn parse_data_type(&mut self) -> Datatype {
        let data_type: Datatype = match self.at().token_type {
            TokenType::Void => {
                self.eat();
                Datatype::Void
            }
            TokenType::Text => {
                self.eat();
                Datatype::Text
            }
            TokenType::Integer => {
                self.eat();
                Datatype::Integer
            }
            TokenType::Decimal => {
                self.eat();
                Datatype::Decimal
            }
            TokenType::Bool => {
                self.eat();
                Datatype::Boolean
            }
            TokenType::Object => {
                self.eat();
                self.expect(TokenType::LessThan, "\"<\" Expected.");

                let dt: Datatype = self.parse_data_type();
                self.expect(TokenType::GreaterThan, "\">\" Expected.");
                Datatype::Object(Box::new(dt))
            }
            TokenType::Array => {
                self.eat();
                self.expect(TokenType::LessThan, "\"<\" Expected.");

                let dt: Datatype = self.parse_data_type();
                self.expect(TokenType::GreaterThan, "\">\" Expected.");
                Datatype::Array(Box::new(dt))
            }
            TokenType::OParen => {
                self.eat();
                let mut types: Vec<Box<Datatype>> = Vec::new();
                types.push(Box::new(self.parse_data_type()));

                while self.at().token_type == TokenType::Comma {
                    self.eat();
                    types.push(Box::new(self.parse_data_type()));
                }
                self.expect(TokenType::CParen, "\")\" Expected.");
                if types.len() == 1 {
                    return Datatype::Tuple(types[0].clone());
                } else {
                    return Datatype::Tuple(Box::new(Datatype::_Multitype(types)));
                }
            }

            _ => {
                self.error(
                    "Expected one of primitive types: text, integer, decimal, boolean, Array<T>, Tuple<T>, Object<T> or _.",
                );

                Datatype::_NOTYPE
            }
        };

        data_type
    }

    fn expect(&mut self, expected_type: TokenType, err: &str) -> Token {
        let prev: Token = self.eat();

        if prev.token_type == TokenType::Eof {
            return prev;
        }

        if prev.token_type != expected_type {
            if prev.token_type == TokenType::_Invalid {
                self.error(&format!(
                    "Expected '{:?}' token, but got a bad token: '{}'",
                    expected_type, prev.value
                ));
            } else {
                self.error(err);
            }
        }

        prev
    }

    fn error(&mut self, message: &str) {
        let token: &Token = &self.at();
        let filename: &String = &token.filename;
        let column: (usize, usize) = token.column;
        let lineno: usize = token.lineno;
        let lineshift: String = " ".repeat(3 + (lineno.to_string().len()));
        let line: &String = &self.lines.as_ref().unwrap()[lineno - 1];
        let column_repr: String = format!(
            "{}{}",
            " ".repeat(column.0 - 1),
            "^".repeat(token.value.len())
        );

        let formatted_message: String = format!(
            "%%b{}%%!:%%y{}%%!:%%y{}%%!:\n%%r{} %%y{}%%!\n\t{} | {}\n\t{}%%r{}%%!",
            filename, lineno, column.0, "ERROR:", message, lineno, line, lineshift, column_repr,
        );
        printc(&formatted_message);

        self.errstate = true;
        self.eat();
    }

    pub fn produce_ast(mut self, tokens: Vec<Token>, source_code: &str) -> Program {
        self.tokens = tokens;
        self.lines = Some(source_code.split('\n').map(String::from).collect());

        let mut program: Program = Program {
            kind: NodeType::Program,
            body: Vec::new(),
        };

        while self.at().token_type != TokenType::Eof {
            program.body.push(self.parse_stmt());
        }

        program
    }

    fn parse_stmt(&mut self) -> Stmt {
        let curtoken: &Token = &self.at();
        match curtoken.token_type {
            TokenType::Import => self.parse_import_stmt(),

            TokenType::Export => self.parse_export_stmt(),

            TokenType::If => self.parse_if_stmt(),

            TokenType::Asm => self.parse_asm_stmt(),

            TokenType::Mov => self.parse_mov_stmt(),

            TokenType::Loop => self.parse_loop_stmt(),

            TokenType::For => self.parse_for_stmt(),

            TokenType::While => self.parse_while_stmt(),

            TokenType::Class => self.parse_class(),

            TokenType::Enum => self.parse_enum(),

            TokenType::Private | TokenType::Public => self.parse_class_statement_declaration(),

            TokenType::Var => self.parse_var_variable_declaration(),

            TokenType::Label => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let expr: Expr = self.parse_function_declaration();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Stmt {
                    kind: NodeType::FunctionDeclaration,
                    expr: Some(expr),
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                }
            }

            TokenType::Resb | TokenType::Resw | TokenType::Resd | TokenType::Resq => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let data_size: String = self.eat().value;

                let mut _size: Option<String> = Some("1".to_string());

                if self.at().token_type == TokenType::OBracket {
                    self.eat();
                    _size = Some(format!("{}[{}]", data_size, self.eat().value));
                    self.expect(TokenType::CBracket, "\"]\" Expected.");
                }
                let identifer: Option<String> = Some(self.eat().value);

                self.expect(TokenType::Colon, "\":\" Expected.");

                let data_type: Datatype = self.parse_data_type();

                let stmt_expr = self.parse_var_declaration(
                    data_size, data_type, true, column, position, lineno, identifer,
                );

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(stmt_expr),
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                }
            }

            TokenType::Auto
            | TokenType::Byte
            | TokenType::Word
            | TokenType::Dword
            | TokenType::Qword => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let data_size: Token = self.eat();

                if data_size.token_type == TokenType::Auto
                    && self.at().token_type == TokenType::OBracket
                {
                    self.error("Automatic size cannot have brackets.");
                }

                let identifier: Option<String> = Some(self.eat().value);

                self.expect(TokenType::Colon, "\":\" Expected.");

                let data_type: Datatype = self.parse_data_type();

                let stmt_expr: Expr = self.parse_var_declaration(
                    data_size.value,
                    data_type,
                    true,
                    column,
                    position,
                    lineno,
                    identifier,
                );

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(stmt_expr),
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                }
            }

            TokenType::Return => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let return_expr: ReturnStmt = self.parse_return_stmt();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Stmt {
                    kind: NodeType::Stmt,
                    expr: None,
                    return_stmt: Some(return_expr),
                    column,
                    position,
                    lineno,
                }
            }

            TokenType::Identifier | TokenType::String | TokenType::Number => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                if !matches!(
                    self.next().token_type,
                    TokenType::Semicolon
                        | TokenType::Attribution
                        | TokenType::Colon
                        | TokenType::OBracket
                        | TokenType::BitwiseAnd
                        | TokenType::BitwiseOr
                        | TokenType::BitwiseXor
                        | TokenType::ShiftLeft
                        | TokenType::ShiftRight
                        | TokenType::Plus
                        | TokenType::Power
                        | TokenType::Minus
                        | TokenType::Mod
                        | TokenType::Mul
                        | TokenType::Div
                ) {
                    if self.next().token_type == TokenType::Dot
                        || self.next().token_type == TokenType::OParen
                        || self.next().token_type == TokenType::CBrace
                    {
                        let expr: Option<Expr> = Some(self.parse_call_member_expr(None));

                        column.1 = self.at().column.1 - 1;
                        position.1 = self.at().position.1 - 1;

                        let return_expr: ReturnStmt = ReturnStmt {
                            kind: NodeType::ReturnStmt,
                            argument: expr.clone(),
                            column,
                            position,
                            lineno,
                        };

                        if self.at().token_type == TokenType::Semicolon {
                            self.eat();
                            column.1 = self.at().column.1 - 1;
                            position.1 = self.at().position.1 - 1;

                            return Stmt {
                                kind: NodeType::CallExpr,
                                expr,
                                return_stmt: None,
                                column,
                                position,
                                lineno,
                            };
                        }

                        Stmt {
                            kind: NodeType::Stmt,
                            expr: None,
                            return_stmt: Some(return_expr),
                            column,
                            position,
                            lineno,
                        }
                    } else {
                        let expr: Expr = self.parse_expr();

                        column.1 = self.at().column.1 - 1;
                        position.1 = self.at().position.1 - 1;

                        Stmt {
                            kind: expr.kind(),
                            expr: Some(expr),
                            return_stmt: None,
                            column,
                            position,
                            lineno,
                        }
                    }
                } else {
                    let mut column: (usize, usize) = self.at().column;
                    let mut position: (usize, usize) = self.at().position;
                    let mut column_var: (usize, usize) = self.at().column;
                    let mut position_var: (usize, usize) = self.at().position;
                    let lineno: usize = self.at().lineno;

                    let mut expr: Expr = Expr::VoidLiteral(VoidLiteral {
                        kind: NodeType::VoidLiteral,
                        value: "()",
                        typ: Some(Datatype::_NOTYPE),
                        column,
                        position,
                        lineno,
                    });

                    loop {
                        match self.next().token_type {
                            TokenType::BitwiseAnd
                            | TokenType::BitwiseOr
                            | TokenType::BitwiseXor
                            | TokenType::ShiftLeft
                            | TokenType::ShiftRight
                            | TokenType::Plus
                            | TokenType::Power
                            | TokenType::Minus
                            | TokenType::Mod
                            | TokenType::Mul
                            | TokenType::Div => {
                                expr = self.parse_bitwise_expr();
                            }
                            TokenType::Attribution => {
                                expr = self.parse_assignment_expr();
                            }
                            TokenType::OParen => {
                                expr = self.parse_expr();
                                let args: Vec<Box<Expr>> = self.parse_arguments();

                                column.1 = self.at().column.1 - 1;
                                position.1 = self.at().position.1 - 1;

                                expr = Expr::CallExpr(CallExpr {
                                    kind: NodeType::CallExpr,
                                    caller: Box::new(expr),
                                    args,
                                    typ: RefCell::new(None),
                                    column,
                                    position,
                                    lineno,
                                });
                            }
                            TokenType::OBracket => {
                                expr = self.parse_primary_expr();
                                expr = self.parse_array_access_expr(expr);
                            }
                            TokenType::Colon => {
                                let identifier: String = self.eat().value;
                                self.eat();
                                let data_type: Datatype = self.parse_data_type();
                                if self.at().token_type == TokenType::Semicolon {
                                    self.eat();

                                    column.1 = self.at().column.1 - 1;
                                    position.1 = self.at().position.1 - 1;

                                    return Stmt {
                                        kind: NodeType::VarDeclaration,
                                        expr: Some(Expr::VarDeclaration(VarDeclaration {
                                            kind: NodeType::VarDeclaration,
                                            constant: true,
                                            data_size: "auto".to_string(),
                                            data_type,
                                            identifier: Some(identifier),
                                            value: Box::new(Expr::VoidLiteral(VoidLiteral {
                                                kind: NodeType::VoidLiteral,
                                                value: "()",
                                                typ: Some(Datatype::Void),
                                                column: self.at().column,
                                                position: self.at().position,
                                                lineno,
                                            })),
                                            inferred: false,
                                            column,
                                            position,
                                            lineno,
                                        })),
                                        return_stmt: None,
                                        column,
                                        position,
                                        lineno,
                                    };
                                } else {
                                    self.expect(TokenType::Attribution, "\"=\" Expected.");

                                    let value: Box<Expr>;

                                    match self.at().token_type {
                                        TokenType::OBracket => {
                                            value = Box::new(self.parse_array_expr());
                                        }
                                        TokenType::OBrace => {
                                            value = Box::new(self.parse_object_expr());
                                        }
                                        _ => {
                                            value = Box::new(*self.parse_ternary_expr());
                                        }
                                    }

                                    column_var.1 = self.at().column.1 - 1;
                                    position_var.1 = self.at().position.1 - 1;

                                    self.expect(TokenType::Semicolon, "\";\" Expected");

                                    return Stmt {
                                        kind: NodeType::VarDeclaration,
                                        expr: Some(Expr::VarDeclaration(VarDeclaration {
                                            kind: NodeType::VarDeclaration,
                                            constant: true,
                                            data_size: "auto".to_string(),
                                            data_type,
                                            identifier: Some(identifier),
                                            value,
                                            inferred: false,
                                            column: column_var,
                                            position: position_var,
                                            lineno,
                                        })),

                                        return_stmt: None,
                                        column,
                                        position,
                                        lineno,
                                    };
                                }
                            }

                            _ => break,
                        }
                    }

                    if self.next().token_type == TokenType::Semicolon {
                        self.eat();
                        self.eat();
                    }

                    column.1 = self.at().column.1 - 1;
                    position.1 = self.at().position.1 - 1;

                    Stmt {
                        kind: expr.kind(),
                        expr: Some(expr),
                        return_stmt: None,
                        column,
                        position,
                        lineno,
                    }
                }
            }

            _ => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                //parse_range
                let expr: Expr = self.parse_expr();
                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Stmt {
                    kind: expr.kind(),
                    expr: Some(expr),
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                }
            }
        }
    }

    fn parse_enum(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let name = self
            .expect(TokenType::Identifier, "Identifier Expected.")
            .value;

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut items: Vec<(String, i32)> = Vec::new();
        let mut counter: i32 = 0;

        while self.at().token_type != TokenType::CBrace {
            let identifier: String = self
                .expect(TokenType::Identifier, "Identifier Expected.")
                .value;

            if self.at().token_type == TokenType::Attribution {
                self.eat();
                let num = self
                    .expect(TokenType::Number, "Index Expected.")
                    .value
                    .parse()
                    .ok()
                    .unwrap();

                items.push((identifier, num));

                if self.at().token_type == TokenType::Comma {
                    self.eat();
                }

                continue;
            }

            if self.at().token_type == TokenType::Comma {
                self.eat();
            }

            items.push((identifier, counter));
            counter += 1;
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::Enum,
            expr: Some(Expr::Enum(Enum {
                kind: NodeType::Enum,
                name,
                items,
                column,
                position,
                lineno,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_while_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let condition: Expr = self.parse_logical_expr();
        let mut body: Vec<Stmt> = Vec::new();

        self.expect(TokenType::OBrace, "\"{\" Expected.");
        while self.at().token_type != TokenType::CBrace && self.at().token_type != TokenType::Eof {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::WhileStmt,
            expr: Some(Expr::WhileStmt(Box::new(WhileStmt {
                kind: NodeType::WhileStmt,
                condition,
                body,
                column,
                position,
                lineno,
            }))),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_var_variable_declaration(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_val: (usize, usize) = self.at().column;
        let mut position_val: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let identifier: String = self.eat().value;

        column_val.1 = self.at().column.1 - 1;
        position_val.1 = self.at().position.1 - 1;

        if self.at().token_type == TokenType::Semicolon {
            self.eat();
            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            return Stmt {
                kind: NodeType::VarDeclaration,
                expr: Some(Expr::VarDeclaration(VarDeclaration {
                    kind: NodeType::VarDeclaration,
                    constant: false,
                    data_size: "auto".to_string(),
                    data_type: Datatype::Void,
                    identifier: Some(identifier),
                    value: Box::from(Expr::VoidLiteral(VoidLiteral {
                        kind: NodeType::VoidLiteral,
                        value: "()",
                        typ: Some(Datatype::Void),
                        column: self.at().column,
                        position: self.at().position,
                        lineno,
                    })),
                    inferred: false,
                    column,
                    position,
                    lineno,
                })),
                return_stmt: None,
                column,
                position,
                lineno,
            };
        }

        if self.at().token_type == TokenType::Colon {
            self.eat();

            let data_type: Datatype = self.parse_data_type();
            column_val.1 = self.at().column.1 - 1;
            position_val.1 = self.at().position.1 - 1;

            if self.at().token_type == TokenType::Semicolon {
                self.eat();
                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                return Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(Expr::VarDeclaration(VarDeclaration {
                        kind: NodeType::VarDeclaration,
                        constant: false,
                        data_size: "auto".to_string(),
                        data_type,
                        identifier: Some(identifier),
                        value: Box::from(Expr::VoidLiteral(VoidLiteral {
                            kind: NodeType::VoidLiteral,
                            value: "()",
                            typ: Some(Datatype::Void),
                            column: self.at().column,
                            position: self.at().position,
                            lineno,
                        })),
                        inferred: false,
                        column: column_val,
                        position: position_val,
                        lineno,
                    })),
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                };
            } else {
                self.expect(TokenType::Attribution, "\"=\" Expected.");

                let value: Box<Expr>;

                match self.at().token_type {
                    TokenType::OBracket => {
                        value = Box::new(self.parse_array_expr());
                    }

                    TokenType::OBrace => value = Box::new(self.parse_object_expr()),
                    _ => {
                        value = Box::new(*self.parse_ternary_expr());
                    }
                }

                column_val.1 = self.at().column.1 - 1;
                position_val.1 = self.at().position.1 - 1;

                self.expect(TokenType::Semicolon, "\";\" Expected");

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                return Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(Expr::VarDeclaration(VarDeclaration {
                        kind: NodeType::VarDeclaration,
                        constant: false,
                        data_size: "auto".to_string(),
                        data_type,
                        identifier: Some(identifier),
                        value,
                        inferred: false,
                        column: column_val,
                        position: position_val,
                        lineno,
                    })),
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                };
            }
        } else {
            self.expect(TokenType::Attribution, "\"=\" Expected.");

            let value: Box<Expr>;

            match self.at().token_type {
                TokenType::OBracket => {
                    value = Box::new(self.parse_array_expr());
                }

                TokenType::OBrace => value = Box::new(self.parse_object_expr()),
                _ => {
                    value = Box::new(*self.parse_ternary_expr());
                }
            }
            column_val.1 = self.at().column.1 - 1;
            position_val.1 = self.at().position.1 - 1;

            self.expect(TokenType::Semicolon, "\";\" Expected");

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            return Stmt {
                kind: NodeType::VarDeclaration,
                expr: Some(Expr::VarDeclaration(VarDeclaration {
                    kind: NodeType::VarDeclaration,
                    constant: false,
                    data_size: "auto".to_string(),
                    data_type: Datatype::Void,
                    identifier: Some(identifier),
                    value,
                    inferred: true,
                    column,
                    position,
                    lineno,
                })),
                return_stmt: None,
                column,
                position,
                lineno,
            };
        }
    }

    fn parse_for_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let mut items: Vec<String> = Vec::new();

        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::In {
            items.push(
                self.expect(TokenType::Identifier, "Identifier Expected.")
                    .value,
            );

            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        self.expect(TokenType::In, "\"in\" Expected.");

        let seq: Expr = self.parse_range_expr();

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();

        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::ForStmt,
            expr: Some(Expr::ForStmt(ForStmt {
                kind: NodeType::ForStmt,
                sequence: Box::new(seq),
                items,
                body,
                column,
                position,
                lineno,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_range_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;
        if self.at().token_type == TokenType::Range
            || self.at().token_type == TokenType::RangeInclusive
        {
            let range: String = self.eat().value;
            let mut expr: Expr = self.parse_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;
            expr = Expr::RangeExpr(Box::new(RangeExpr {
                kind: NodeType::RangeExpr,
                start: Expr::NumericLiteral(NumericLiteral {
                    kind: NodeType::NumericLiteral,
                    value: String::from("0"),
                    typ: Some(Datatype::Integer),
                    column,
                    position,
                    lineno,
                }),
                range,
                end: expr,
                column,
                position,
                lineno,
            }));

            return expr;
        }
        let mut expr: Expr = self.parse_expr();

        if self.at().token_type == TokenType::Range
            || self.at().token_type == TokenType::RangeInclusive
        {
            let range: String = self.eat().value;
            let end = self.parse_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;
            expr = Expr::RangeExpr(Box::new(RangeExpr {
                kind: NodeType::RangeExpr,
                start: expr,
                range,
                end,
                column,
                position,
                lineno,
            }))
        }

        expr
    }
    fn parse_loop_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();
        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();

        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::LoopStmt,
            expr: Some(Expr::LoopStmt(LoopStmt {
                kind: NodeType::LoopStmt,
                body,
                column,
                position,
                lineno,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_array_access_expr(&mut self, array: Expr) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_array: (usize, usize) = self.at().column;
        let mut position_array: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        if self.at().token_type == TokenType::OBracket {
            self.eat();
            if self.at().token_type == TokenType::CBracket {
                self.error("Array index Expected.");
            }
            let index_expr: Expr = self.parse_expr();

            self.expect(TokenType::CBracket, "\"]\" Expected.");

            column_array.1 = self.at().column.1 - 1;
            position_array.1 = self.at().position.1 - 1;

            if self.at().token_type == TokenType::Attribution {
                self.eat();
                let value_expr: Expr = self.parse_expr();

                self.expect(TokenType::Semicolon, "\";\" Expected.");

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Expr::AssignmentExpr(AssignmentExpr {
                    kind: NodeType::AssignmentExpr,
                    assigne: Box::new(Expr::ArrayAccess(ArrayAccess {
                        kind: NodeType::ArrayAccess,
                        array: Box::new(array),
                        index: Box::new(index_expr),
                        typ: RefCell::new(None),
                        column: column_array,
                        position: position_array,
                        lineno,
                    })),
                    value: Box::new(value_expr),
                    column,
                    position,
                    lineno,
                })
            } else if self.at().token_type == TokenType::OParen
                || self.at().token_type == TokenType::Dot
                || self.at().token_type == TokenType::OBracket
            {
                let expr: Option<Expr> = Some(Expr::ArrayAccess(ArrayAccess {
                    kind: NodeType::ArrayAccess,
                    array: Box::new(array),
                    index: Box::new(index_expr),
                    typ: RefCell::new(None),
                    column: column_array,
                    position: position_array,
                    lineno,
                }));

                self.parse_call_member_expr(expr)
            } else {
                Expr::ArrayAccess(ArrayAccess {
                    kind: NodeType::ArrayAccess,
                    array: Box::new(array),
                    index: Box::new(index_expr),
                    typ: RefCell::new(None),
                    column: column_array,
                    position: position_array,
                    lineno,
                })
            }
        } else {
            self.parse_call_member_expr(None)
        }
    }

    fn parse_mov_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_mov: (usize, usize) = self.at().column;
        let mut position_mov: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;
        self.eat();

        self.expect(TokenType::OParen, "\"(\" Expected.");
        let mut values: Vec<(String, String)> = Vec::new();

        let val1 = self.expect(TokenType::String, "String Expected.").value;
        self.expect(TokenType::Comma, "\",\" Expected.");
        let val2 = self.expect(TokenType::String, "String Expected.").value;

        values.push((val1, val2));

        self.expect(TokenType::CParen, "\")\" Expected.");

        column_mov.1 = self.at().column.1 - 1;
        position_mov.1 = self.at().position.1 - 1;

        self.expect(TokenType::Semicolon, "\";\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        let expr: Option<Expr> = Some(Expr::MovStmt(MovStmt {
            kind: NodeType::MovStmt,
            values,
            column: column_mov,
            position: position_mov,
            lineno,
        }));

        Stmt {
            kind: NodeType::MovStmt,
            expr,
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_asm_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_asm: (usize, usize) = self.at().column;
        let mut position_asm: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut asm_code: Vec<Expr> = Vec::new();

        self.eat();
        self.expect(TokenType::OParen, "\"(\" Expected.");

        while self.at().token_type != TokenType::CParen {
            self.expect(TokenType::OBracket, "\"[\" Expected.");

            while self.at().token_type != TokenType::CBracket {
                asm_code.push(self.parse_expr());

                if self.at().token_type == TokenType::Comma {
                    self.eat();
                }
            }
            self.expect(TokenType::CBracket, "\"]\" Expected.");
        }

        self.expect(TokenType::CParen, "\")\" Expected.");

        column_asm.1 = self.at().column.1 - 1;
        position_asm.1 = self.at().position.1 - 1;

        self.expect(TokenType::Semicolon, "\";\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::AsmStmt,
            expr: Some(Expr::AsmStmt(AsmStmt {
                kind: NodeType::AsmStmt,
                code: asm_code,
                column: column_asm,
                position: position_asm,
                lineno,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_arguments(&mut self) -> Vec<Box<Expr>> {
        let mut args: Vec<Box<Expr>> = Vec::new();

        self.eat();

        while self.at().token_type != TokenType::CParen {
            let arg: Expr = self.parse_expr();
            args.push(Box::new(arg));
            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        self.eat();

        args
    }

    fn parse_if_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let test: Expr = self.parse_logical_expr();

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut consequent: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            consequent.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        let mut alternate: Option<Vec<Stmt>> = None;
        while self.not_eof()
            && (self.at().token_type == TokenType::Else || self.at().token_type == TokenType::Elif)
        {
            self.eat();

            if self.at().token_type == TokenType::If || self.at().token_type == TokenType::Elif {
                alternate = Some(vec![self.parse_if_stmt()]);
                break;
            } else {
                self.expect(TokenType::OBrace, "\"{\" Expected.");

                let mut else_body: Vec<Stmt> = Vec::new();
                while self.not_eof() && self.at().token_type != TokenType::CBrace {
                    else_body.push(self.parse_stmt());
                }

                self.expect(TokenType::CBrace, "\"}\" Expected.");
                alternate = Some(else_body);
            }
        }

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::IfStmt,
            expr: Some(Expr::IfStmt(Box::new(IfStmt {
                kind: NodeType::IfStmt,
                test: Box::new(test),
                consequent,
                alternate,
                column,
                position,
                lineno,
            }))),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_logical_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut expr: Expr = self.parse_equality_expr();

        while self.at().token_type == TokenType::And || self.at().token_type == TokenType::Or {
            let operator: String = self.eat().value.clone();
            let right: Expr = self.parse_equality_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            expr = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(expr),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        expr
    }

    fn parse_equality_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_relational_expr();

        while self.at().token_type == TokenType::Equals
            || self.at().token_type == TokenType::NotEquals
        {
            let operator: String = self.eat().value.clone();
            let right: Expr = self.parse_relational_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        left
    }

    fn parse_relational_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_logical_not_expr();

        while self.at().token_type == TokenType::LessThan
            || self.at().token_type == TokenType::LessThanOrEqual
            || self.at().token_type == TokenType::GreaterThan
            || self.at().token_type == TokenType::GreaterThanOrEqual
        {
            let operator: String = self.eat().value.clone();
            let right: Expr = self.parse_logical_not_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        left
    }

    fn parse_logical_not_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::Not {
            let mut column: (usize, usize) = self.at().column;
            let mut position: (usize, usize) = self.at().position;
            let lineno: usize = self.at().lineno;

            self.eat();
            let operand: Expr = self.parse_logical_not_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            Expr::LogicalNotExpr(LogicalNotExpr {
                kind: NodeType::LogicalNotExpr,
                operand: Box::new(operand),
                typ: None,
                column,
                position,
                lineno,
            })
        } else {
            self.parse_unary_expr()
        }
    }

    fn parse_import_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_import: (usize, usize) = self.at().column;
        let mut position_import: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let mut paths: HashMap<StringLiteral, Option<Identifier>> = HashMap::new();

        while self.at().token_type != TokenType::Semicolon {
            let column_s: (usize, usize) = self.at().column;
            let position_s: (usize, usize) = self.at().position;
            let lineno_s: usize = self.at().lineno;

            let path_value = self
                .expect(TokenType::String, "String Expected.")
                .value
                .clone();
            let path = StringLiteral {
                kind: NodeType::StringLiteral,
                value: path_value,
                typ: Some(Datatype::Text),
                column: column_s,
                position: position_s,
                lineno: lineno_s,
            };

            let mut alias: Option<Identifier> = None;

            if self.at().token_type == TokenType::As {
                self.eat();
                let column_i: (usize, usize) = self.at().column;
                let position_i: (usize, usize) = self.at().position;
                let lineno_i: usize = self.at().lineno;

                let alias_value = self
                    .expect(TokenType::Identifier, "Identifier Expected.")
                    .value
                    .clone();
                alias = Some(Identifier {
                    kind: NodeType::Identifier,
                    symbol: alias_value,
                    typ: RefCell::new(None),
                    column: column_i,
                    position: position_i,
                    lineno: lineno_i,
                });
            }

            paths.insert(path, alias);

            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        column_import.1 = self.at().column.1 - 1;
        position_import.1 = self.at().position.1 - 1;
        self.expect(TokenType::Semicolon, "\";\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::ImportStmt,
            expr: Some(Expr::ImportStmt(ImportStmt {
                kind: NodeType::Stmt,
                paths,
                column: column_import,
                position: position_import,
                lineno,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_export_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        if self.at().token_type == TokenType::Export {
            self.error("Expected something to export.");
        }
        let statement: Box<Stmt> = Box::new(self.parse_stmt());

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Stmt {
            kind: NodeType::ExportStmt,
            expr: Some(Expr::ExportStmt(ExportStmt {
                kind: NodeType::Stmt,
                statement,
                column,
                position,
                lineno,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_return_stmt(&mut self) -> ReturnStmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.expect(TokenType::Return, "\"return\" Expected.");
        if self.at().token_type == TokenType::Semicolon {
            self.eat();
            return ReturnStmt {
                kind: NodeType::ReturnStmt,
                argument: None,
                column,
                position,
                lineno,
            };
        }

        let expr: Expr = match self.at().token_type {
            TokenType::OBracket => self.parse_array_expr(),
            TokenType::OBrace => self.parse_object_expr(),
            _ => *self.parse_ternary_expr(),
        };

        if self.tokens.get(self.index - 1).unwrap().token_type == TokenType::Semicolon {
            column.1 = self.at().column.1 - 2;
            position.1 = self.at().position.1 - 2;
            return ReturnStmt {
                kind: NodeType::ReturnStmt,
                argument: Some(expr),
                column,
                position,
                lineno,
            };
        }
        self.expect(TokenType::Semicolon, "\";\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        ReturnStmt {
            kind: NodeType::ReturnStmt,
            argument: Some(expr),
            column,
            position,
            lineno,
        }
    }

    fn parse_function_declaration(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();
        let name: String = self
            .expect(TokenType::Identifier, "Function name expected")
            .value;
        let mut parameters: Vec<(String, String, Datatype)> = Vec::new();

        if self.at().token_type == TokenType::OParen {
            self.eat();

            if self.at().token_type == TokenType::CParen {
                self.error("Parameter(s) Expected inside parentheses.");
                self.eat();
            } else {
                while self.at().token_type != TokenType::CParen {
                    let size: String = match self.at().token_type {
                        TokenType::Byte | TokenType::Word | TokenType::Dword | TokenType::Qword => {
                            self.eat().value
                        }
                        _ => "auto".to_string(),
                    };

                    let identifier: String = self
                        .expect(TokenType::Identifier, "Parameter name expected")
                        .value;

                    let mut data_type: Datatype = Datatype::Void;
                    if self.at().token_type == TokenType::Colon {
                        self.eat();
                        match self.at().token_type {
                            TokenType::Text
                            | TokenType::Integer
                            | TokenType::Decimal
                            | TokenType::Array
                            | TokenType::Object
                            | TokenType::LessThan => {
                                data_type = self.parse_data_type();
                            }
                            _ => data_type = Datatype::Void,
                        }
                    }

                    parameters.push((size, identifier, data_type));

                    if self.at().token_type == TokenType::Comma {
                        self.eat();
                    } else if self.at().token_type != TokenType::CParen {
                        self.error("Expected ',' or ')' after parameter");
                        break;
                    }
                }
                self.expect(TokenType::CParen, "\")\" Expected.");
            }
        }

        let mut return_size: String = "auto".to_string();
        let mut return_type: Datatype = Datatype::Void;

        if self.at().token_type == TokenType::Colon {
            self.eat();
            match self.at().token_type {
                TokenType::Void
                | TokenType::Text
                | TokenType::Integer
                | TokenType::Decimal
                | TokenType::Array
                | TokenType::Object => {
                    return_type = self.parse_data_type();
                }
                TokenType::Auto
                | TokenType::Byte
                | TokenType::Word
                | TokenType::Dword
                | TokenType::Qword => {
                    return_size = self.eat().value;
                    match self.at().token_type {
                        TokenType::Void
                        | TokenType::Text
                        | TokenType::Integer
                        | TokenType::Decimal
                        | TokenType::Array
                        | TokenType::Object => {
                            return_type = self.parse_data_type();
                        }
                        _ => self.error("Expected type after size."),
                    }
                }
                _ => self.error("Unexpected token "),
            }
        }

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        self.in_function_declaration_state = true;

        let mut body: Vec<Stmt> = Vec::new();

        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.in_function_declaration_state = false;

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        body.push(Stmt {
            kind: NodeType::_EOL,
            expr: Some(Expr::_EOL(_EOL {
                kind: NodeType::_EOL,
            })),
            return_stmt: None,
            column,
            position,
            lineno,
        });

        Expr::FunctionDeclaration(Box::new(FunctionDeclaration {
            kind: NodeType::FunctionDeclaration,
            return_size,
            return_type,
            name,
            parameters,
            body,
            column,
            position,
            lineno,
        }))
    }

    fn parse_class_statement_declaration(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let access_modifier: String = match self.at().token_type {
            TokenType::Public | TokenType::Private => self.eat().value.clone(),
            _ => "private".to_string(),
        };

        column.1 = self.at().column.1;
        position.1 = self.at().position.1;

        if self.at().token_type == TokenType::Label {
            return Stmt {
                kind: NodeType::ClassFunctionDeclaration,
                expr: Some(Expr::ClassFunctionDeclaration(Box::new(
                    ClassFunctionDeclaration {
                        kind: NodeType::ClassFunctionDeclaration,
                        access_modifier,
                        function: self.parse_function_declaration(),
                        column,
                        position,
                        lineno,
                    },
                ))),
                return_stmt: None,
                column,
                position,
                lineno,
            };
        }

        Stmt {
            kind: NodeType::ClassVarDeclaration,
            expr: Some(Expr::ClassVarDeclaration(Box::new(ClassVarDeclaration {
                kind: NodeType::ClassVarDeclaration,
                access_modifier,
                var: self.parse_stmt(),
                column,
                position,
                lineno,
            }))),
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_class(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();

        let name: String = self
            .expect(TokenType::Identifier, "Identifier Expected.")
            .value
            .clone();

        let mut super_class: Option<String> = Some(String::new());

        if self.at().token_type == TokenType::OParen {
            self.eat();
            if let Some(class) = super_class.as_mut() {
                class.push_str(self.eat().value.clone().as_str());
            }

            self.expect(TokenType::CParen, "\")\" Expected.");
        }

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        let expr: Option<Expr> = Some(Expr::ClassDeclaration(Box::new(ClassDeclaration {
            kind: NodeType::ClassDeclaration,
            name,
            super_class,
            body,
            column,
            position,
            lineno,
        })));

        Stmt {
            kind: NodeType::ClassDeclaration,
            expr,
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    fn parse_var_declaration(
        &mut self,
        data_size: String,
        data_type: Datatype,
        is_constant: bool,
        mut column: (usize, usize),
        mut position: (usize, usize),
        lineno: usize,
        mut identifier: Option<String>,
    ) -> Expr {
        if is_constant {
            if identifier.is_none() {
                identifier = Some(
                    self.expect(TokenType::Identifier, "Identifier Expected.")
                        .value,
                );
            }

            self.expect(TokenType::Attribution, "\"=\" Expected.");

            let value: Box<Expr>;

            match self.at().token_type {
                TokenType::OBracket => {
                    value = Box::new(self.parse_array_expr());
                }

                TokenType::OBrace => value = Box::new(self.parse_object_expr()),
                _ => {
                    value = Box::new(*self.parse_ternary_expr());
                }
            }

            self.expect(TokenType::Semicolon, "\";\" Expected");

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            Expr::VarDeclaration(VarDeclaration {
                kind: NodeType::VarDeclaration,
                constant: is_constant,
                data_size,
                data_type,
                identifier,
                value,
                inferred: false,
                column,
                position,
                lineno,
            })
        } else {
            let identifier: String = self
                .expect(TokenType::Identifier, "Identifier Expected.")
                .value;

            self.expect(TokenType::Attribution, "\"=\" Expected.");

            let value: Box<Expr>;

            match self.at().token_type {
                TokenType::OBracket => {
                    value = Box::new(self.parse_array_expr());
                }

                TokenType::OBrace => value = Box::new(self.parse_object_expr()),
                _ => {
                    value = Box::new(*self.parse_ternary_expr());
                }
            }

            self.expect(TokenType::Semicolon, "\";\" Expected");

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            Expr::VarDeclaration(VarDeclaration {
                kind: NodeType::VarDeclaration,
                constant: is_constant,
                data_size,
                data_type,
                identifier: Some(identifier),
                value,
                inferred: false,
                column,
                position,
                lineno,
            })
        }
    }

    fn parse_ternary_expr(&mut self) -> Box<Expr> {
        if self.at().token_type == TokenType::Identifier
            && self.next().token_type == TokenType::OBracket
        {
            let identifier: Expr = self.parse_primary_expr();
            return Box::new(self.parse_array_access_expr(identifier));
        }

        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let condition: Expr = self.parse_logical_expr();

        if self.at().token_type != TokenType::QuestionMark {
            return Box::new(condition);
        }
        self.eat();

        let consequent: Box<Expr> = self.parse_nested_ternary_expr();

        if self.at().token_type != TokenType::Colon {
            self.error("\":\" Expected after '?' in ternary expression.");
            return Box::new(condition);
        }
        self.eat();

        let alternate = self.parse_nested_ternary_expr();

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Box::new(Expr::TernaryExpr(TernaryExpr {
            kind: NodeType::TernaryExpr,
            condition: Box::new(condition),
            consequent,
            alternate,
            typ: RefCell::new(None),
            column,
            position,
            lineno,
        }))
    }

    fn parse_nested_ternary_expr(&mut self) -> Box<Expr> {
        if self.at().token_type == TokenType::OParen {
            self.eat();
            let expr: Box<Expr> = self.parse_ternary_expr();
            self.expect(TokenType::CParen, "\")\" Expected.");
            expr
        } else if self.at().token_type == TokenType::OBrace {
            let mut column: (usize, usize) = self.at().column;
            let mut position: (usize, usize) = self.at().position;
            let lineno: usize = self.at().lineno;

            self.eat();
            let mut statements: Vec<Stmt> = Vec::new();

            while self.at().token_type != TokenType::CBrace {
                statements.push(self.parse_stmt());
            }
            self.expect(TokenType::CBrace, "\"}\" Expected.");

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            Box::new(Expr::BlockExpr(Box::new(BlockExpr {
                kind: NodeType::BlockExpr,
                statements,
                column,
                position,
                lineno,
            })))
        } else {
            Box::new(self.parse_expr())
        }
    }

    fn parse_unary_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let expr: Expr = match self.at().token_type {
            TokenType::Minus => {
                self.eat();
                let operand: Expr = self.parse_unary_expr();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Expr::UnaryMinusExpr(UnaryMinusExpr {
                    kind: NodeType::UnaryMinusExpr,
                    operand: Box::new(operand),
                    typ: RefCell::new(None),
                    column,
                    position,
                    lineno,
                })
            }
            TokenType::Not => {
                self.eat();
                let operand: Expr = self.parse_unary_expr();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Expr::LogicalNotExpr(LogicalNotExpr {
                    kind: NodeType::LogicalNotExpr,
                    operand: Box::new(operand),
                    typ: None,
                    column,
                    position,
                    lineno,
                })
            }
            TokenType::BitwiseNot => {
                self.eat();
                let operand: Expr = self.parse_unary_expr();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Expr::UnaryBitwiseNotExpr(UnaryBitwiseNotExpr {
                    kind: NodeType::UnaryBitwiseNotExpr,
                    operand: Box::new(operand),
                    typ: Some(Datatype::Integer),
                    column,
                    position,
                    lineno,
                })
            }
            TokenType::Increment => {
                self.eat();
                let operand: Expr = self.parse_unary_expr();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Expr::PreIncrementExpr(PreIncrementExpr {
                    kind: NodeType::PreIncrementExpr,
                    operand: Box::new(operand),
                    column,
                    position,
                    lineno,
                })
            }
            TokenType::Decrement => {
                self.eat();
                let operand: Expr = self.parse_unary_expr();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Expr::PreDecrementExpr(PreDecrementExpr {
                    kind: NodeType::PreDecrementExpr,
                    operand: Box::new(operand),
                    column,
                    position,
                    lineno,
                })
            }
            _ => {
                let exp = self.parse_bitwise_expr();
                if self.at().token_type == TokenType::Increment
                    || self.at().token_type == TokenType::Decrement
                {
                    return self.parse_postfix_expr(exp);
                }
                return exp;
            }
        };

        expr
    }

    fn parse_postfix_expr(&mut self, mut expr: Expr) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        loop {
            match self.at().token_type {
                TokenType::Increment => {
                    self.eat();
                    column.1 = self.at().column.1 - 1;
                    position.1 = self.at().position.1 - 1;

                    expr = Expr::PostIncrementExpr(PostIncrementExpr {
                        kind: NodeType::PostIncrementExpr,
                        operand: Box::new(expr.clone()),
                        column,
                        position,
                        lineno,
                    });
                }
                TokenType::Decrement => {
                    self.eat();
                    column.1 = self.at().column.1 - 1;
                    position.1 = self.at().position.1 - 1;

                    expr = Expr::PostDecrementExpr(PostDecrementExpr {
                        kind: NodeType::PostDecrementExpr,
                        operand: Box::new(expr.clone()),
                        column,
                        position,
                        lineno,
                    });
                }
                _ => break,
            }
        }

        expr.clone()
    }

    fn parse_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::Minus
            || self.at().token_type == TokenType::Increment
            || self.at().token_type == TokenType::Decrement
            || self.at().token_type == TokenType::Not
            || self.at().token_type == TokenType::BitwiseNot
            || self.next().token_type == TokenType::Increment
            || self.next().token_type == TokenType::Decrement
        {
            return self.parse_unary_expr();
        }

        match self.next().token_type {
            TokenType::BitwiseAnd
            | TokenType::BitwiseOr
            | TokenType::ShiftLeft
            | TokenType::ShiftRight
            | TokenType::BitwiseXor
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Mul
            | TokenType::Div
            | TokenType::IntegerDiv
            | TokenType::Mod => self.parse_bitwise_expr(),
            _ => self.parse_assignment_expr(),
        }
    }

    fn parse_assignment_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = *self.parse_ternary_expr();

        while self.at().token_type == TokenType::Attribution {
            self.eat();
            let right: Expr = match self.at().token_type {
                TokenType::OBracket => self.parse_array_expr(),
                TokenType::OBrace => self.parse_object_expr(),
                _ => *self.parse_ternary_expr(),
            };

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::AssignmentExpr(AssignmentExpr {
                kind: NodeType::AssignmentExpr,
                assigne: Box::new(left),
                value: Box::new(right),
                column,
                position,
                lineno,
            });

            self.expect(TokenType::Semicolon, "\";\" Expected.");
        }

        left
    }

    fn parse_array_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        if self.at().token_type != TokenType::OBracket {
            return self.parse_object_expr();
        }

        let mut array: Vec<Expr> = Vec::new();

        self.eat();

        while self.at().token_type != TokenType::CBracket {
            array.push(self.parse_array_expr());

            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        self.expect(TokenType::CBracket, "\"]\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Expr::ArrayExpr(ArrayExpr {
            kind: NodeType::ArrayExpr,
            elements: array,
            typ: RefCell::new(None),
            column,
            position,
            lineno,
        })
    }

    fn parse_object_expr(&mut self) -> Expr {
        if self.at().token_type != TokenType::OBrace {
            return self.parse_bitwise_expr();
        }

        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat();
        let mut properties: Vec<Property> = Vec::new();

        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            let mut column_property: (usize, usize) = self.at().column;
            let mut position_property: (usize, usize) = self.at().position;
            let lineno_property: usize = self.at().lineno;

            let key: String = self
                .expect(TokenType::Identifier, "Key Identifier Expected.")
                .value;

            let value: Option<Box<Expr>> = if self.at().token_type == TokenType::Comma {
                Some(Box::new(Expr::StringLiteral(StringLiteral {
                    kind: NodeType::StringLiteral,
                    value: key.clone(),
                    typ: Some(Datatype::Text),
                    column: self.at().column,
                    position: self.at().position,
                    lineno: self.at().lineno,
                })))
            } else {
                self.expect(TokenType::Colon, "\":\" Expected.");

                Some(Box::new(self.parse_expr()))
            };

            column_property.1 = self.at().column.1 - 1;
            position_property.1 = self.at().position.1 - 1;

            properties.push(Property {
                kind: NodeType::Property,
                key,
                value,
                column: column_property,
                position: position_property,
                lineno: lineno_property,
            });

            if self.at().token_type == TokenType::Comma {
                self.eat();
            } else if self.at().token_type != TokenType::CBrace {
                self.error("Expected ',' or '}' following property");
                break;
            }
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");
        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        Expr::ObjectLiteral(ObjectLiteral {
            kind: NodeType::ObjectLiteral,
            properties,
            typ: RefCell::new(None),
            column,
            position,
            lineno,
        })
    }

    fn parse_bitwise_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_additive_expr();

        while self.at().token_type == TokenType::BitwiseOr
            || self.at().token_type == TokenType::BitwiseAnd
            || self.at().token_type == TokenType::BitwiseXor
            || self.at().token_type == TokenType::ShiftLeft
            || self.at().token_type == TokenType::ShiftRight
        {
            let operator: String = self.eat().value;
            let right: Expr = self.parse_additive_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        left
    }

    fn parse_additive_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_multiplicative_expr();

        while self.at().token_type == TokenType::Plus || self.at().token_type == TokenType::Minus {
            let operator: String = self.eat().value;
            let right: Expr = self.parse_multiplicative_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        left
    }

    fn parse_multiplicative_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_exponential_expr();

        while self.at().token_type == TokenType::Mul
            || self.at().token_type == TokenType::Div
            || self.at().token_type == TokenType::IntegerDiv
            || self.at().token_type == TokenType::Mod
        {
            let operator: String = self.eat().value;
            let right: Expr = self.parse_exponential_expr();

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        left
    }

    fn parse_exponential_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_call_member_expr(None);

        while self.at().token_type == TokenType::Power {
            let operator: String = self.eat().value;
            let right: Expr = self.parse_call_member_expr(None);

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
                typ: RefCell::new(None),
                column,
                position,
                lineno,
            });
        }

        left
    }

    fn parse_call_member_expr(&mut self, statement: Option<Expr>) -> Expr {
        let mut expr: Expr;

        if let Some(stmt) = statement {
            expr = stmt;
        } else {
            expr = self.parse_primary_expr();
        }

        loop {
            match self.at().token_type {
                TokenType::Dot => {
                    let mut column_member: (usize, usize) = self.at().column;
                    let mut position_member: (usize, usize) = self.at().position;
                    let lineno: usize = self.at().lineno;
                    self.eat();
                    let column_identifier: (usize, usize) = self.at().column;
                    let position_identifier: (usize, usize) = self.at().position;
                    let property: Token =
                        self.expect(TokenType::Identifier, "Identifier Expected.");
                    column_member.1 = self.at().column.1 - 1;
                    position_member.1 = self.at().position.1 - 1;
                    expr = Expr::MemberExpr(MemberExpr {
                        kind: NodeType::MemberExpr,
                        object: Box::new(expr),
                        property: Box::new(Expr::Identifier(Identifier {
                            kind: NodeType::Identifier,
                            symbol: property.value,
                            typ: RefCell::new(None),
                            column: column_identifier,
                            position: position_identifier,
                            lineno,
                        })),
                        typ: RefCell::new(None),
                        column: column_member,
                        position: position_member,
                        lineno,
                    });
                }
                TokenType::OParen => {
                    expr = self.parse_call_expr(expr);
                }

                TokenType::OBracket => {
                    expr = self.parse_array_access_expr(expr);
                }
                TokenType::Semicolon => {
                    return expr;
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_call_expr(&mut self, caller: Expr) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.expect(TokenType::OParen, "\"(\" Expected.");
        let args: Vec<Box<Expr>> = self.parse_args();

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        let expr: Expr = Expr::CallExpr(CallExpr {
            kind: NodeType::CallExpr,
            caller: Box::new(caller),
            args,
            typ: RefCell::new(None),
            column,
            position,
            lineno,
        });

        if self.in_function_declaration_state {
            if self.at().token_type != TokenType::Semicolon
                && self.next().token_type == TokenType::CBrace
            {
                self.expect(TokenType::Semicolon, "\";\" Expected.");
            } else {
                return expr;
            }
        } else if self.at().token_type == TokenType::Semicolon {
            return expr;
        } else {
            if self.at().token_type == TokenType::Semicolon
                && self.next().token_type == TokenType::CBrace
            {
                self.error("\";\" Unexpected.");
            }
        }

        expr
    }

    fn parse_args(&mut self) -> Vec<Box<Expr>> {
        let mut args: Vec<Box<Expr>> = Vec::new();
        if self.at().token_type != TokenType::CParen {
            args = self.parse_arguments_list();
        }

        self.expect(TokenType::CParen, "\")\" Expected.");

        args
    }

    fn parse_arguments_list(&mut self) -> Vec<Box<Expr>> {
        let mut args: Vec<Box<Expr>> = Vec::new();

        args.push(Box::new(self.parse_object_expr()));

        while self.at().token_type == TokenType::Comma {
            self.eat();

            if self.at().token_type != TokenType::CParen {
                args.push(Box::new(self.parse_object_expr()));
            }
        }

        args
    }

    fn parse_primary_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::OParen {
            let column: (usize, usize) = self.at().column;
            let position: (usize, usize) = self.at().position;
            let lineno: usize = self.at().lineno;

            self.eat();
            let expr: Expr = self.parse_expr();

            if self.at().token_type == TokenType::Comma {
                let mut values: Vec<Expr> = Vec::new();
                values.push(expr);
                while self.at().token_type == TokenType::Comma {
                    self.eat();
                    values.push(self.parse_expr());
                    if self.at().token_type == TokenType::CParen {
                        self.eat();
                        break;
                    }
                }

                return Expr::TupleLiteral(TupleLiteral {
                    kind: NodeType::TupleLiteral,
                    value: values,
                    typ: RefCell::new(None),
                    column,
                    position,
                    lineno,
                });
            }

            self.expect(TokenType::CParen, "\")\" Expected.");

            expr
        } else {
            self.parse_essential_expr()
        }
    }

    fn parse_essential_expr(&mut self) -> Expr {
        match self.at().token_type {
            TokenType::Identifier => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                return Expr::Identifier(Identifier {
                    kind: NodeType::Identifier,
                    symbol: self.eat().value,
                    typ: RefCell::new(None),
                    column,
                    position,
                    lineno,
                });
            }
            TokenType::Number => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                return Expr::NumericLiteral(NumericLiteral {
                    kind: NodeType::NumericLiteral,
                    value: self.eat().value,
                    typ: Some(Datatype::Integer),
                    column,
                    position,
                    lineno,
                });
            }
            TokenType::String => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                return Expr::StringLiteral(StringLiteral {
                    kind: NodeType::StringLiteral,
                    value: self.eat().value,
                    typ: Some(Datatype::Text),
                    column,
                    position,
                    lineno,
                });
            }
            TokenType::Null => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                self.eat();
                return Expr::VoidLiteral(VoidLiteral {
                    kind: NodeType::VoidLiteral,
                    value: "()",
                    typ: Some(Datatype::_NOTYPE),
                    column,
                    position,
                    lineno,
                });
            }

            TokenType::True => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                self.eat();
                return Expr::BooleanLiteral(BooleanLiteral {
                    kind: NodeType::BooleanLiteral,
                    value: "true".to_string(),
                    typ: Some(Datatype::Boolean),
                    column,
                    position,
                    lineno,
                });
            }

            TokenType::False => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                self.eat();
                return Expr::BooleanLiteral(BooleanLiteral {
                    kind: NodeType::BooleanLiteral,
                    value: "false".to_string(),
                    typ: Some(Datatype::Boolean),
                    column,
                    position,
                    lineno,
                });
            }

            TokenType::Continue => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                self.eat();

                self.expect(TokenType::Semicolon, "\";\" Expected.");

                return Expr::ContinueExpr(ContinueExpr {
                    kind: NodeType::ContinueExpr,
                    column,
                    position,
                    lineno,
                });
            }

            TokenType::Break => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                self.eat();

                self.expect(TokenType::Semicolon, "\";\" Expected.");
                return Expr::BreakExpr(BreakExpr {
                    kind: NodeType::BreakExpr,
                    column,
                    position,
                    lineno,
                });
            }
            _ => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                self.error(&format!("Unexpected token: {:?}", self.at().token_type));

                return Expr::VoidLiteral(VoidLiteral {
                    kind: NodeType::VoidLiteral,
                    value: "()",
                    typ: Some(Datatype::_NOTYPE),
                    column,
                    position,
                    lineno,
                });
            }
        }
    }
}
