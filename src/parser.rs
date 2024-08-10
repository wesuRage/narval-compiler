use crate::ast::*;
use crate::colors::printc;
use crate::datatype::*;
use crate::lexer::{Token, TokenType};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>, // Um vetor de tokens que representa a entrada do parser
    errstate: bool,     // Um indicador de estado de erro para o parser
    lines: Option<Vec<String>>, // Uma opção de vetor de strings representando as linhas do código fonte
    index: usize,               // Um índice para rastrear a posição atual durante o parsing
    in_function_declaration_state: bool, // Define o estado de uma chamada de função estar ou não no final de uma função
}

impl Parser {
    // Método de criação de uma nova instância de Parser
    pub fn new() -> Parser {
        // Criação de uma nova instância de Parser com valores padrão
        Parser {
            tokens: Vec::new(),                   // Inicializa o vetor de tokens como vazio
            errstate: false,                      // Define o estado de erro como falso
            lines: None,                          // Inicializa as linhas como nenhuma (None)
            index: 0,                             // Define o índice inicial como 0
            in_function_declaration_state: false, // Define o estado como falso
        }
    }

    // Método para verificar se não é o final do arquivo (end of file - EOF)
    fn not_eof(&self) -> bool {
        self.at().token_type != TokenType::Eof
    }

    // Método para obter o token atual
    fn at(&self) -> &Token {
        &self.tokens[self.index] // Retorna o token na posição atual do índice
    }

    // Método para consumir o token atual e avançar para o próximo
    fn eat(&mut self) -> Token {
        let current_index = self.index; // Salva o índice atual
        self.index += 1; // Avança para o próximo token
        self.tokens[current_index].clone() // Retorna o token atual consumido
    }

    // Método para retornar o próximo token sem consumi-lo
    fn next(&self) -> &Token {
        if self.tokens.get(self.index + 1).is_none() {
            return self.at();
        }

        self.tokens.get(self.index + 1).unwrap()
    }

    // Método para analisar o tipo de dado
    fn parse_data_type(&mut self) -> Datatype {
        // Match para determinar o tipo de token atual
        let data_type: Datatype = match self.at().token_type {
            // Se o token atual for Void, Text, Integer, Decimal, Bool Object, Array ou Tuple
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
                self.expect(TokenType::LessThan, "\"<\" Expected."); // Verifica se o próximo token é "<"

                // Parsear o tipo interno recursivamente
                let dt: Datatype = self.parse_data_type();
                self.expect(TokenType::GreaterThan, "\">\" Expected."); // Verifica se o próximo token é ">"
                Datatype::Object(Box::new(dt))
            }
            TokenType::Array => {
                self.eat();
                self.expect(TokenType::LessThan, "\"<\" Expected."); // Verifica se o próximo token é "<"

                // Parsear o tipo interno recursivamente
                let dt: Datatype = self.parse_data_type();
                self.expect(TokenType::GreaterThan, "\">\" Expected."); // Verifica se o próximo token é ">"
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
            // Se o token atual não corresponder a nenhum tipo esperado
            _ => {
                self.error(
                    "Expected one of primitive types: text, integer, decimal, boolean, Array<T>, Tuple<T>, Object<T> or _.",
                );
                // Gera um erro indicando o tipo esperado
                Datatype::_NOTYPE
            }
        };

        data_type
    }

    // Método para esperar um tipo específico de token
    fn expect(&mut self, expected_type: TokenType, err: &str) -> Token {
        let prev: Token = self.eat(); // Consome o token anterior

        // Se o tipo do token anterior for EOF, retorna o próprio token
        if prev.token_type == TokenType::Eof {
            return prev;
        }

        // Verifica se o tipo do token anterior é o esperado
        if prev.token_type != expected_type {
            // Se o tipo do token anterior for inválido, gera um erro especial
            if prev.token_type == TokenType::_Invalid {
                self.error(&format!(
                    "Expected '{:?}' token, but got a bad token: '{}'",
                    expected_type, prev.value
                ));
            } else {
                self.error(err); // Gera um erro indicando o tipo esperado
            }
        }

        prev // Retorna o token anterior
    }

    // Método para lidar com erros durante o parsing
    fn error(&mut self, message: &str) {
        let token: &Token = &self.at(); // Obtém o token atual
        let filename: &String = &token.filename; // Obtém o nome do arquivo do token
        let column: (usize, usize) = token.column; // Obtém a posição da coluna do token
        let lineno: usize = token.lineno; // Obtém o número da linha do token
        let lineshift: String = " ".repeat(3 + (lineno.to_string().len()));
        let line: &String = &self.lines.as_ref().unwrap()[lineno - 1]; // Obtém a linha do código fonte
        let column_repr: String = format!(
            "{}{}",
            " ".repeat(column.0 - 1),
            "^".repeat(token.value.len())
        ); // Representação visual da posição do erro na linha

        // Formata a mensagem de erro com informações relevantes
        let formatted_message: String = format!(
            "%%b{}%%!:%%y{}%%!:%%y{}%%!:\n%%r{} %%y{}%%!\n\t{} | {}\n\t{}%%r{}%%!",
            filename, lineno, column.0, "ERROR:", message, lineno, line, lineshift, column_repr,
        );
        printc(&formatted_message); // Imprime a mensagem formatada com cores

        self.errstate = true; // Define o estado de erro do parser como verdadeiro
        self.eat(); // Consome o token atual para evitar ciclos infinitos
    }

    // Método para produzir a AST (Árvore de Sintaxe Abstrata) a partir dos tokens fornecidos e do código fonte
    pub fn produce_ast(mut self, tokens: Vec<Token>, source_code: &str) -> Program {
        self.tokens = tokens; // Armazena os tokens no parser
        self.lines = Some(source_code.split('\n').map(String::from).collect()); // Armazena as linhas do código fonte no parser

        let mut program: Program = Program {
            kind: NodeType::Program,
            body: Vec::new(),
        }; // Inicializa a estrutura do programa na AST

        // Loop para analisar cada token e construir a AST
        while self.at().token_type != TokenType::Eof {
            program.body.push(self.parse_stmt()); // Analisa e adiciona a instrução ao vetor de corpo
        }

        program // Retorna a AST do programa
    }

    // Método para analisar uma declaração de instrução
    fn parse_stmt(&mut self) -> Stmt {
        let curtoken: &Token = &self.at(); // Obtém o token atual
        match curtoken.token_type {
            // Se o token atual for um Import
            TokenType::Import => self.parse_import_stmt(),
            // Se o token atual for um Export
            TokenType::Export => self.parse_export_stmt(),
            // Se o token atual for um If
            TokenType::If => self.parse_if_stmt(),
            // Se o token atual for um asm, analisa um statement de código assembly arbitrário
            TokenType::Asm => self.parse_asm_stmt(),
            // Se o token atual for um mov
            TokenType::Mov => self.parse_mov_stmt(),
            // Se o token atual for um loop
            TokenType::Loop => self.parse_loop_stmt(),
            // Se o token atual for um for
            TokenType::For => self.parse_for_stmt(),
            // Se o token atual for um while
            TokenType::While => self.parse_while_stmt(),
            // Se o token atual for uma unit
            TokenType::Unit => self.parse_unit(),
            // Se o token atual for um enum
            TokenType::Enum => self.parse_enum(),
            // Se o token atual for private ou public, analisa um statement unit
            TokenType::Private | TokenType::Public => self.parse_unit_statement_declaration(),
            // Analisa declaração de variaveis simples, tipadas ou não
            TokenType::Var => self.parse_var_variable_declaration(),
            // Analisa declaração de funções
            TokenType::Label => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let expr: Expr = self.parse_function_declaration();

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                Stmt {
                    kind: NodeType::FunctionDeclaration,
                    expr: Some(expr), // Analisa a declaração da função
                    return_stmt: None,
                    column,
                    position,
                    lineno,
                }
            }
            // Se o token atual for Resb, Resw, Resd, ou Resq, analisa uma declaração de variável
            TokenType::Resb | TokenType::Resw | TokenType::Resd | TokenType::Resq => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let data_size: String = self.eat().value; // Obtém o tamanho dos dados

                let mut _size: Option<String> = Some("1".to_string());
                // Se houver colchetes, obtém o tamanho especificado
                if self.at().token_type == TokenType::OBracket {
                    _size = Some(format!(
                        "{}[{}]",
                        data_size,
                        self.expect(TokenType::Number, "Integer Expected.").value
                    ));
                    self.expect(TokenType::CBracket, "\"]\" Expected.");
                }
                let identifer: Option<String> = Some(self.eat().value);

                self.expect(TokenType::Colon, "\":\" Expected.");

                let data_type: Datatype = self.parse_data_type(); // Analisa o tipo de dado

                let stmt_expr = self.parse_var_declaration(
                    data_size, data_type, true, column, position, lineno, identifer,
                ); // Analisa a declaração da variável

                // Atualiza a coluna e a posição antes de retornar o Stmt
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
            // Se o token atual for Auto, Byte, Word, Dword, ou Qword, analisa uma declaração de variável
            TokenType::Auto
            | TokenType::Byte
            | TokenType::Word
            | TokenType::Dword
            | TokenType::Qword => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let data_size: Token = self.eat(); // Obtém o tamanho dos dados

                // Verifica se é uma declaração de variável automática que não pode ter colchetes
                if data_size.token_type == TokenType::Auto
                    && self.at().token_type == TokenType::OBracket
                {
                    self.error("Automatic size cannot have brackets.");
                }

                let identifier: Option<String> = Some(self.eat().value);

                self.expect(TokenType::Colon, "\":\" Expected.");

                let data_type: Datatype = self.parse_data_type(); // Analisa o tipo de dado

                let stmt_expr: Expr = self.parse_var_declaration(
                    data_size.value,
                    data_type,
                    true,
                    column,
                    position,
                    lineno,
                    identifier,
                ); // Analisa a declaração da variável

                // Atualiza a coluna e a posição antes de retornar o Stmt
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
            // Se o token atual for Return, analisa uma declaração de retorno
            TokenType::Return => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let return_expr: ReturnStmt = self.parse_return_stmt(); // Analisa a expressão de retorno

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

            // Se o token atual for Identifier, String, ou Number, analisa uma expressão ou chamada de função
            TokenType::Identifier | TokenType::String | TokenType::Number => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                // Verifica se o próximo token não é um dos tokens que indicam o final da expressão
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
                    // Analisa se o retorno é um member expression ou função
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

                        // Se conter um ";" faz o parsing como call expr normal
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

                        // Senão, retorna como return statement
                        Stmt {
                            kind: NodeType::Stmt,
                            expr: None,
                            return_stmt: Some(return_expr),
                            column,
                            position,
                            lineno,
                        }
                    } else {
                        // Se não for um member expr, continua
                        let expr: Expr = self.parse_expr(); // Analisa a expressão

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
                    // Placeholder inicial para a variável expr.
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

                            // Outros casos possíveis
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

            // Outros casos para outros tipos de tokens...
            _ => {
                let mut column: (usize, usize) = self.at().column;
                let mut position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;

                let expr: Expr = self.parse_expr(); // Analisa a expressão
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
        // Consome o token de enum
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
        // Consome o token de while
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

        // faz o parsing de "var identifier: type;" e "var identifier: type = value"
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

                // Parseia a expressão de valor
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
                // Espera pelo ponto e vírgula ";"
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

            // Parseia a expressão de valor
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
            // Espera pelo ponto e vírgula ";"
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

        self.eat(); // consome o token de for

        let mut items: Vec<String> = Vec::new();

        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::In {
            items.push(
                self.expect(TokenType::Identifier, "Identifier Expected.")
                    .value,
            ); // adiciona um item aos itens

            // Se houver uma vírgula, consome-a
            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        self.expect(TokenType::In, "\"in\" Expected.");

        let seq: Expr = self.parse_range_expr();

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();

        // Enquanto não for end of file ou "}", faz o parsing de statements
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

        self.eat(); // consome o token de loop
        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();

        // Enquanto não for end of file ou "}", faz o parsing de statements
        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        // Retorna o loop statement
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

    // Método para analisar uma expressão de acesso a array
    fn parse_array_access_expr(&mut self, array: Expr) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_array: (usize, usize) = self.at().column;
        let mut position_array: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        // Verifica se há uma abertura de colchete
        if self.at().token_type == TokenType::OBracket {
            self.eat(); // Consome o token de abertura de colchete
            if self.at().token_type == TokenType::CBracket {
                self.error("Array index Expected.");
            }
            let index_expr: Expr = self.parse_expr(); // Analisa a expressão do índice do array

            // Verifica se há um fechamento de colchete
            self.expect(TokenType::CBracket, "\"]\" Expected.");

            column_array.1 = self.at().column.1 - 1;
            position_array.1 = self.at().position.1 - 1;

            // Verifica se há uma atribuição após o índice do array
            if self.at().token_type == TokenType::Attribution {
                self.eat(); // Consome o operador de atribuição
                let value_expr: Expr = self.parse_expr(); // Analisa a expressão do valor atribuído

                self.expect(TokenType::Semicolon, "\";\" Expected.");

                column.1 = self.at().column.1 - 1;
                position.1 = self.at().position.1 - 1;

                // Constrói uma expressão de atribuição
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
            // Se não houver abertura de colchete, retorna apenas a expressão do objeto/array
            self.parse_call_member_expr(None)
        }
    }

    // Método para analias mov statements
    fn parse_mov_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_mov: (usize, usize) = self.at().column;
        let mut position_mov: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;
        self.eat(); // Consome o token de "mov"

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

    // Método para injeção de código assembly arbitrário
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

    // Método para analisar os argumentos de uma chamada de função
    fn parse_arguments(&mut self) -> Vec<Box<Expr>> {
        let mut args: Vec<Box<Expr>> = Vec::new(); // Inicializa um vetor para armazenar os argumentos

        self.eat(); // Consome o token de abertura de parênteses

        // Loop para analisar cada argumento
        while self.at().token_type != TokenType::CParen {
            let arg: Expr = self.parse_expr(); // Analisa a expressão do argumento
            args.push(Box::new(arg)); // Armazena o argumento no vetor
            if self.at().token_type == TokenType::Comma {
                self.eat(); // Consome a vírgula entre os argumentos
            }
        }

        self.eat(); // Consome o token de fechamento de parênteses

        args // Retorna o vetor de argumentos
    }

    // Método para analisar um if statement
    fn parse_if_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat(); // Consome o token "if"

        // Analisa a expressão lógica dentro do if
        let test: Expr = self.parse_logical_expr();

        // Espera a abertura de um bloco
        self.expect(TokenType::OBrace, "\"{\" Expected.");

        // Analisa o corpo do "if"
        let mut consequent: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            consequent.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        // Analisa o bloco "else" ou "elif", se presente
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
            let operator: String = self.eat().value.clone(); // Consome o operador lógico
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

    // Método para analisar expressões de igualdade
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

    // Método para analisar expressões relacionais
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

    // Método para analisar uma expressão de negação lógica
    fn parse_logical_not_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::Not {
            let mut column: (usize, usize) = self.at().column;
            let mut position: (usize, usize) = self.at().position;
            let lineno: usize = self.at().lineno;

            self.eat(); // Consome o operador de negação lógica
            let operand: Expr = self.parse_logical_not_expr(); // Analisa a expressão unária seguinte

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
            self.parse_unary_expr() // Chama a análise da próxima expressão unária
        }
    }

    // Método para analisar uma declaração de importação
    fn parse_import_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let mut column_import: (usize, usize) = self.at().column;
        let mut position_import: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat(); // Consome o token "import"

        let mut paths: HashMap<StringLiteral, Option<Identifier>> = HashMap::new();

        while self.at().token_type != TokenType::Semicolon {
            let column_s: (usize, usize) = self.at().column;
            let position_s: (usize, usize) = self.at().position;
            let lineno_s: usize = self.at().lineno;

            // Parse `path` as `StringLiteral`
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

            // Parseia "alias" como "identifier" se existir
            if self.at().token_type == TokenType::As {
                self.eat(); // Consome o token "as"
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
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o token ";"

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

    // Método para analisar uma declaração de exportação
    fn parse_export_stmt(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat(); // Consome o token "export"

        if self.at().token_type == TokenType::Export {
            self.error("Expected something to export.");
        }
        let statement: Box<Stmt> = Box::new(self.parse_stmt());

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        // Retorna uma estrutura Stmt representando a declaração de exportação
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

    // Método para analisar uma declaração de retorno
    fn parse_return_stmt(&mut self) -> ReturnStmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.expect(TokenType::Return, "\"return\" Expected."); // Consome o token "return"
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
        // Usa parse_call_member_expr para analisar a expressão de retorno completa
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
                argument: Some(expr), // Retorna a expressão analisada
                column,
                position,
                lineno,
            };
        }
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o ponto e vírgula

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        ReturnStmt {
            kind: NodeType::ReturnStmt,
            argument: Some(expr), // Retorna a expressão analisada
            column,
            position,
            lineno,
        }
    }

    // Método para analisar a declaração de uma função
    fn parse_function_declaration(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat(); // Consome o token "label"
        let name: String = self
            .expect(TokenType::Identifier, "Function name expected") // Analisa e armazena o nome da função
            .value;
        let mut parameters: Vec<(String, String, Datatype)> = Vec::new(); // Inicializa um vetor para armazenar os parâmetros da função

        // Verifica se há parênteses para delimitar os parâmetros
        if self.at().token_type == TokenType::OParen {
            self.eat(); // Consome o token "("

            // Verifica se há parâmetros dentro dos parênteses
            if self.at().token_type == TokenType::CParen {
                self.error("Parameter(s) Expected inside parentheses."); // Emite um erro se não houver parâmetros
                self.eat(); // Consome o token ")"
            } else {
                // Loop para analisar cada parâmetro
                while self.at().token_type != TokenType::CParen {
                    let size: String = match self.at().token_type {
                        TokenType::Byte | TokenType::Word | TokenType::Dword | TokenType::Qword => {
                            self.eat().value
                        }
                        _ => "auto".to_string(),
                    };

                    let identifier: String = self
                        .expect(TokenType::Identifier, "Parameter name expected") // Analisa e armazena o nome do parâmetro
                        .value;

                    let mut data_type: Datatype = Datatype::Void; // Analisa e obtém o tipo de dados do parâmetro
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

                    parameters.push((size, identifier, data_type)); // Adiciona o parâmetro ao vetor de parâmetros

                    if self.at().token_type == TokenType::Comma {
                        self.eat(); // Consome a vírgula entre os parâmetros
                    } else if self.at().token_type != TokenType::CParen {
                        self.error("Expected ',' or ')' after parameter"); // Emite um erro se não houver uma vírgula ou parêntese após o parâmetro
                        break;
                    }
                }
                self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
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

        self.expect(TokenType::OBrace, "\"{\" Expected."); // Verifica e consome o token "{"

        self.in_function_declaration_state = true; // Define o estado de estar dentro de uma declaração de função

        let mut body: Vec<Stmt> = Vec::new(); // Inicializa um vetor para armazenar o corpo da função

        // Loop para analisar cada instrução no corpo da função
        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt()); // Analisa e adiciona a instrução ao vetor de corpo
        }

        self.in_function_declaration_state = false; // Reseta o estado ao sair da função

        self.expect(TokenType::CBrace, "\"}\" Expected."); // Verifica e consome o token "}"

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        // End of Label
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

        // Retorna uma estrutura Expr representando a declaração da função
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

    fn parse_unit_statement_declaration(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let access_modifier: String = match self.at().token_type {
            TokenType::Public | TokenType::Private => self.eat().value.clone(),
            _ => "private".to_string(),
        };

        // Define as novas posições após análise
        column.1 = self.at().column.1;
        position.1 = self.at().position.1;

        if self.at().token_type == TokenType::Label {
            return Stmt {
                kind: NodeType::UnitFunctionDeclaration,
                expr: Some(Expr::UnitFunctionDeclaration(Box::new(
                    UnitFunctionDeclaration {
                        kind: NodeType::UnitFunctionDeclaration,
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
            kind: NodeType::UnitVarDeclaration,
            expr: Some(Expr::UnitVarDeclaration(Box::new(UnitVarDeclaration {
                kind: NodeType::UnitVarDeclaration,
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

    // Método para analisar uma unidade (unit)
    fn parse_unit(&mut self) -> Stmt {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.eat(); // Consome o token "unit"

        // Nome da unit
        let name: String = self
            .expect(TokenType::Identifier, "Identifier Expected.")
            .value
            .clone();

        // Herança de units
        let mut super_units: Option<Vec<String>> = Some(Vec::new());

        if self.at().token_type == TokenType::OParen {
            self.eat(); // Consome o token "("
            while self.at().token_type == TokenType::Identifier {
                if let Some(units) = super_units.as_mut() {
                    units.push(self.eat().value.clone());
                }

                if self.at().token_type == TokenType::Comma {
                    self.eat(); // Consome o token ","
                }
            }
            self.expect(TokenType::CParen, "\")\" Expected.");
        }

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        // Corpo da unit
        let mut body: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        let expr: Option<Expr> = Some(Expr::UnitDeclaration(Box::new(UnitDeclaration {
            kind: NodeType::UnitDeclaration,
            name,
            super_units,
            body,
            column,
            position,
            lineno,
        })));

        Stmt {
            kind: NodeType::UnitDeclaration,
            expr,
            return_stmt: None,
            column,
            position,
            lineno,
        }
    }

    // Método para analisar uma declaração de variável
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
        // Se a variável é constante
        if is_constant {
            // Pega o identificador da variável
            if identifier.is_none() {
                identifier = Some(
                    self.expect(TokenType::Identifier, "Identifier Expected.")
                        .value,
                );
            }

            // Espera pelo token de atribuição "="
            self.expect(TokenType::Attribution, "\"=\" Expected.");

            // Parseia a expressão de valor
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

            // Espera pelo ponto e vírgula ";"
            self.expect(TokenType::Semicolon, "\";\" Expected");

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            // Retorna uma declaração de constante
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
            // Pega o identificador da variável
            let identifier: String = self
                .expect(TokenType::Identifier, "Identifier Expected.")
                .value;
            // Espera pelo token de atribuição "="
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

            // Espera pelo ponto e vírgula ";"
            self.expect(TokenType::Semicolon, "\";\" Expected");

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            // Retorna uma declaração de variável
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

    // Método para analisar uma expressão ternária
    fn parse_ternary_expr(&mut self) -> Box<Expr> {
        // Verifica se o token atual é um identificador que pode ser seguido por um acesso a índice de array
        if self.at().token_type == TokenType::Identifier
            && self.next().token_type == TokenType::OBracket
        {
            let identifier: Expr = self.parse_primary_expr();
            return Box::new(self.parse_array_access_expr(identifier));
            // Chamada de parse_array_access_expr sem argumentos
        }

        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let condition: Expr = self.parse_logical_expr(); // Analisa a condição da expressão ternária

        // Verifica se há um operador ternário "?"
        if self.at().token_type != TokenType::QuestionMark {
            return Box::new(condition); // Retorna a condição se não houver operador ternário
        }
        self.eat(); // Consome o operador ternário "?"

        let consequent: Box<Expr> = self.parse_nested_ternary_expr(); // Analisa a expressão consequente

        // Verifica se há um token ":"
        if self.at().token_type != TokenType::Colon {
            self.error("\":\" Expected after '?' in ternary expression.");
            return Box::new(condition); // Retorna a condição em caso de erro
        }
        self.eat(); // Consome o token ":"

        let alternate = self.parse_nested_ternary_expr(); // Analisa a expressão alternativa

        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        // Retorna uma estrutura Box<Expr> representando a expressão ternária
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

    // Método para analisar uma expressão ternária aninhada
    fn parse_nested_ternary_expr(&mut self) -> Box<Expr> {
        if self.at().token_type == TokenType::OParen {
            // Verifica se a expressão está entre parênteses
            self.eat(); // Consome o token "("
            let expr: Box<Expr> = self.parse_ternary_expr(); // Analisa a expressão ternária
            self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
            expr // Retorna a expressão analisada
        } else if self.at().token_type == TokenType::OBrace {
            let mut column: (usize, usize) = self.at().column;
            let mut position: (usize, usize) = self.at().position;
            let lineno: usize = self.at().lineno;

            // Verifica se a expressão está entre chaves
            self.eat(); // Consome o token "{"
            let mut statements: Vec<Stmt> = Vec::new();
            // Loop para analisar cada instrução dentro do bloco
            while self.at().token_type != TokenType::CBrace {
                statements.push(self.parse_stmt()); // Analisa e adiciona a instrução ao vetor de instruções
            }
            self.expect(TokenType::CBrace, "\"}\" Expected."); // Verifica e consome o token "}"

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;

            // Retorna uma expressão representando um bloco de instruções
            Box::new(Expr::BlockExpr(Box::new(BlockExpr {
                kind: NodeType::BlockExpr,
                statements,
                column,
                position,
                lineno,
            })))
        } else {
            Box::new(self.parse_expr()) // Retorna a expressão analisada
        }
    }

    // Método para analisar uma expressão unária
    fn parse_unary_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let expr: Expr = match self.at().token_type {
            TokenType::Minus => {
                self.eat(); // Consome o operador de menos unário
                let operand: Expr = self.parse_unary_expr(); // Analise a expressão unária seguinte

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
                self.eat(); // Consome o operador de negação lógica
                let operand: Expr = self.parse_unary_expr(); // Analise a expressão unária seguinte

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
                self.eat(); // Consome o operador de bitwise not
                let operand: Expr = self.parse_unary_expr(); // Analise a expressão unária seguinte

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
                self.eat(); // Consome o operador de pré-incremento
                let operand: Expr = self.parse_unary_expr(); // Analise a expressão unária seguinte

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
                self.eat(); // Consome o operador de pré-decremento
                let operand: Expr = self.parse_unary_expr(); // Analise a expressão unária seguinte

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
            } // Analise a expressão primária e depois verifique por pós-fixação
        };

        expr
    }

    /// Método para analisar uma expressão pós-fixada (incremento/decremento após a expressão)
    fn parse_postfix_expr(&mut self, mut expr: Expr) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        loop {
            match self.at().token_type {
                TokenType::Increment => {
                    self.eat(); // Consome o operador de pós-incremento
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
                    self.eat(); // Consome o operador de pós-decremento
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
                _ => break, // Sai do loop se não houver mais operadores pós-fixados
            }
        }

        expr.clone() // Retorna a expressão final
    }

    // Atualize o método parse_expr para chamar parse_unary_expr
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

    // Método para analisar uma expressão de atribuição
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

            // só que o problema é que tambem ele é analisado mas nao é jogado pra lugar nenhum
            // precisava que ele pegasse o expr anterior e somasse com o novo analisado
            //
            //hmmmmmm
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

    // Método para analisar uma expressão de array
    fn parse_array_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;
        // Verifica se o token é "[", se não for, ele passa para outro parsing
        if self.at().token_type != TokenType::OBracket {
            return self.parse_object_expr();
        }

        // Cria uma variável para armazenar um array
        let mut array: Vec<Expr> = Vec::new();
        // Consome o token de colchetes
        self.eat();

        // Enquanto o token atual for diferente de "]"
        while self.at().token_type != TokenType::CBracket {
            // Coloca na variável de arrays uma expressão
            array.push(self.parse_array_expr());

            // Se houver vígula, consome ela
            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        // Espera que haja o fechamento de colchetes
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

    // Método para analisar uma expressão de objeto
    fn parse_object_expr(&mut self) -> Expr {
        // Verifica se o token atual é uma abertura de chave "{"
        if self.at().token_type != TokenType::OBrace {
            return self.parse_bitwise_expr();
        }

        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        // Consome a abertura de chave "{"
        self.eat();
        let mut properties: Vec<Property> = Vec::new();

        // Loop até encontrar o fechamento de chave "}" ou fim do arquivo
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            let mut column_property: (usize, usize) = self.at().column;
            let mut position_property: (usize, usize) = self.at().position;
            let lineno_property: usize = self.at().lineno;

            // Pega a chave do objeto
            let key: String = self
                .expect(TokenType::Identifier, "Key Identifier Expected.")
                .value;

            // Verifica se o próximo token após a chave é uma vírgula
            // Se sim, assumimos que a chave é também o valor associado
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
                // Espera pelo token de dois pontos ":"
                self.expect(TokenType::Colon, "\":\" Expected.");
                // Parseia o valor associado à chave
                Some(Box::new(self.parse_expr()))
            };

            column_property.1 = self.at().column.1 - 1;
            position_property.1 = self.at().position.1 - 1;
            // Adiciona a propriedade ao vetor de propriedades
            properties.push(Property {
                kind: NodeType::Property,
                key,
                value,
                column: column_property,
                position: position_property,
                lineno: lineno_property,
            });

            // Se o próximo token for uma vírgula ",", consome-a
            if self.at().token_type == TokenType::Comma {
                self.eat();
            } else if self.at().token_type != TokenType::CBrace {
                // Se não for uma vírgula ou fechamento de chave, é um erro
                self.error("Expected ',' or '}' following property");
                break;
            }
        }

        // Espera pelo fechamento de chave "}"
        self.expect(TokenType::CBrace, "\"}\" Expected.");
        column.1 = self.at().column.1 - 1;
        position.1 = self.at().position.1 - 1;

        // Retorna um literal de objeto
        Expr::ObjectLiteral(ObjectLiteral {
            kind: NodeType::ObjectLiteral,
            properties,
            typ: RefCell::new(None),
            column,
            position,
            lineno,
        })
    }

    // Método para analisar uma expressão bitwise
    fn parse_bitwise_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_additive_expr(); // Analisa a expressão aditiva à esquerda

        // Loop para lidar com operadores de bitwise
        while self.at().token_type == TokenType::BitwiseOr
            || self.at().token_type == TokenType::BitwiseAnd
            || self.at().token_type == TokenType::BitwiseXor
            || self.at().token_type == TokenType::ShiftLeft
            || self.at().token_type == TokenType::ShiftRight
        {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_additive_expr(); // Analisa a expressão aditiva à direita

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;
            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
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

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão aditiva
    fn parse_additive_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_multiplicative_expr(); // Analisa a expressão multiplicativa à esquerda

        // Loop para lidar com operadores de adição e subtração
        while self.at().token_type == TokenType::Plus || self.at().token_type == TokenType::Minus {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_multiplicative_expr(); // Analisa a expressão multiplicativa à direita

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;
            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
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

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão multiplicativa
    fn parse_multiplicative_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_exponential_expr(); // Analisa a expressão exponencial à esquerda

        // Loop para lidar com operadores de multiplicação, divisão e módulo
        while self.at().token_type == TokenType::Mul
            || self.at().token_type == TokenType::Div
            || self.at().token_type == TokenType::IntegerDiv
            || self.at().token_type == TokenType::Mod
        {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_exponential_expr(); // Analisa a expressão exponencial à direita

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;
            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
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

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão exponencial
    fn parse_exponential_expr(&mut self) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        let mut left: Expr = self.parse_call_member_expr(None); // Analisa a expressão de chamada/membro à esquerda

        // Loop para lidar com operadores de exponenciação
        while self.at().token_type == TokenType::Power {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_call_member_expr(None); // Analisa a expressão de chamada/membro à direita

            column.1 = self.at().column.1 - 1;
            position.1 = self.at().position.1 - 1;
            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
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

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão de chamada ou membro
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
                    self.eat(); // Consome o ponto
                    let column_identifier: (usize, usize) = self.at().column;
                    let position_identifier: (usize, usize) = self.at().position;
                    let property: Token =
                        self.expect(TokenType::Identifier, "Identifier Expected.");
                    column_member.1 = self.at().column.1 - 1; //ovotatupeneiracebolaavepãopauguloso
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
                    expr = self.parse_call_expr(expr); // Analisa a expressão de chamada associada
                }

                TokenType::OBracket => {
                    expr = self.parse_array_access_expr(expr);
                }
                TokenType::Semicolon => {
                    // self.eat();
                    return expr;
                }
                _ => break, // Sai do loop se não houver mais operações
            }
        }

        expr // Retorna a expressão final
    }

    // Método para analisar uma expressão de chamada
    fn parse_call_expr(&mut self, caller: Expr) -> Expr {
        let mut column: (usize, usize) = self.at().column;
        let mut position: (usize, usize) = self.at().position;
        let lineno: usize = self.at().lineno;

        self.expect(TokenType::OParen, "\"(\" Expected."); // Espera o token '('
        let args: Vec<Box<Expr>> = self.parse_args(); // Analisa os argumentos da chamada de função

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

        // Verifica se a chamada de função está dentro de uma declaração de função
        if self.in_function_declaration_state {
            // Verifica se o próximo token após o fechamento de parênteses é um ponto e vírgula
            if self.at().token_type != TokenType::Semicolon
                && self.next().token_type == TokenType::CBrace
            {
                self.expect(TokenType::Semicolon, "\";\" Expected.");
            } else {
                return expr;
            }
        } else if self.at().token_type == TokenType::Semicolon {
            // Se não estiver dentro de uma declaração de função e o próximo token for ponto e vírgula,
            // retorna a chamada de função diretamente
            return expr;
        } else {
            // Se o próximo token não for ponto e vírgula e não estiver dentro de uma declaração de função,
            // ocorreu um erro de sintaxe
            if self.at().token_type == TokenType::Semicolon
                && self.next().token_type == TokenType::CBrace
            {
                self.error("\";\" Unexpected.");
            }
        }

        expr
    }

    // Método para analisar os argumentos de uma chamada de função
    fn parse_args(&mut self) -> Vec<Box<Expr>> {
        let mut args: Vec<Box<Expr>> = Vec::new();
        if self.at().token_type != TokenType::CParen {
            args = self.parse_arguments_list(); // Analisa a lista de argumentos
        }

        self.expect(TokenType::CParen, "\")\" Expected."); // Espera o token ')'

        args
    }

    // Método para analisar uma lista de argumentos
    fn parse_arguments_list(&mut self) -> Vec<Box<Expr>> {
        let mut args: Vec<Box<Expr>> = Vec::new();

        // Adiciona o primeiro argumento à lista
        args.push(Box::new(self.parse_object_expr()));

        // Loop enquanto houver vírgulas e tokens de argumentos válidos
        while self.at().token_type == TokenType::Comma {
            self.eat(); // Consome a vírgula

            // Verifica e adiciona o próximo argumento à lista
            if self.at().token_type != TokenType::CParen {
                args.push(Box::new(self.parse_object_expr()));
            }
        }

        args // Retorna a lista de argumentos analisada
    }
    //como?

    fn parse_primary_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::OParen {
            let column: (usize, usize) = self.at().column;
            let position: (usize, usize) = self.at().position;
            let lineno: usize = self.at().lineno;
            // Se for um parêntese aberto, consome o token e analisa a expressão dentro dos parênteses
            self.eat();
            let expr: Expr = self.parse_expr(); // Analisa a expressão dentro dos parênteses
            if self.at().token_type == TokenType::Comma {
                self.eat();

                let mut values: Vec<Expr> = Vec::new();
                values.push(expr);
                let mut did = false;
                while self.at().token_type != TokenType::CParen {
                    if !did {
                        self.expect(TokenType::Comma, "Expected \",\" here.");
                        did = true;
                    }
                    values.push(self.parse_expr());

                    if self.at().token_type == TokenType::Comma {
                        self.eat();
                    }
                }

                self.expect(TokenType::CParen, "\")\" Expected."); // Espera um parêntese fechado

                return Expr::TupleLiteral(TupleLiteral {
                    kind: NodeType::TupleLiteral,
                    value: values,
                    typ: RefCell::new(None),
                    column,
                    position,
                    lineno,
                }); // Retorna a expressão analisada
            }

            self.expect(TokenType::CParen, "\")\" Expected."); // Espera um parêntese fechado
            expr // Retorna a expressão analisada
        } else {
            self.parse_essential_expr()
        }
    }
    // Método para analisar uma expressão primária
    fn parse_essential_expr(&mut self) -> Expr {
        // Verifica o tipo de token atual
        match self.at().token_type {
            TokenType::Identifier => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                // Se for um identificador, cria uma expressão de identificador
                return Expr::Identifier(Identifier {
                    kind: NodeType::Identifier,
                    symbol: self.eat().value, // Consome o token e obtém o valor do identificador
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
                // Se for um número, cria uma expressão de literal numérico
                return Expr::NumericLiteral(NumericLiteral {
                    kind: NodeType::NumericLiteral,
                    value: self.eat().value, // Consome o token e obtém o valor numérico
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
                // Se for uma string, cria uma expressão de literal de string
                //
                return Expr::StringLiteral(StringLiteral {
                    kind: NodeType::StringLiteral,
                    value: self.eat().value, // Consome o token e obtém o valor da string
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
                // Se for nulo, consome o token e cria uma expressão de literal nulo
                self.eat();
                return Expr::VoidLiteral(VoidLiteral {
                    kind: NodeType::VoidLiteral,
                    value: "()", // Define o valor como "void"
                    typ: Some(Datatype::_NOTYPE),
                    column,
                    position,
                    lineno,
                });
            }

            // Se for um true
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

            // Se for um false
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

            // Se for um break;
            TokenType::Break => {
                let column: (usize, usize) = self.at().column;
                let position: (usize, usize) = self.at().position;
                let lineno: usize = self.at().lineno;
                self.eat();
                // Espera um semicolon
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
                // Se for qualquer outro tipo de token
                self.error(&format!("Unexpected token: {:?}", self.at().token_type)); // Gera um erro indicando um token inesperado

                // Retorna uma expressão void como fallback
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
