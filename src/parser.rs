use crate::ast::*;
use crate::colors::{escape, printc};
use crate::lexer::{Token, TokenType};
use std::collections::HashMap;

// Implementação de métodos para a enumeração Expr
impl Expr {
    // Método que retorna o tipo do nó da árvore de sintaxe abstrata (AST)
    pub fn kind(&self) -> NodeType {
        // O match é usado para verificar o tipo de expressão e retornar o tipo correspondente
        match self {
            Expr::BinaryExpr(_) => NodeType::BinaryExpr, // Se for uma expressão binária
            Expr::NullLiteral(_) => NodeType::NullLiteral, // Se for um literal nulo
            Expr::NumericLiteral(_) => NodeType::NumericLiteral, // Se for um literal numérico
            Expr::Identifier(_) => NodeType::Identifier, // Se for um identificador
            Expr::VarDeclaration(_) => NodeType::VarDeclaration, // Se for uma declaração de variável
            Expr::AssignmentExpr(_) => NodeType::AssignmentExpr, // Se for uma expressão de atribuição
            Expr::ObjectLiteral(_) => NodeType::ObjectLiteral,   // Se for um literal de objeto
            Expr::MemberExpr(_) => NodeType::MemberExpr,         // Se for uma expressão de membro
            Expr::CallExpr(_) => NodeType::CallExpr,             // Se for uma chamada de função
            Expr::FunctionDeclaration(_) => NodeType::FunctionDeclaration, // Se for uma declaração de função
            Expr::StringLiteral(_) => NodeType::StringLiteral, // Se for um literal de string
            Expr::ImportStmt(_) => NodeType::ImportStmt, // Se for uma declaração de importação
            Expr::ExportStmt(_) => NodeType::ExportStmt, // Se for uma declaração de exportação
            Expr::IfStmt(_) => NodeType::IfStmt, // Se for uma declaração de condicional 'if'
            Expr::TernaryExpr(_) => NodeType::TernaryExpr, // Se for uma expressão ternária
            Expr::BlockExpr(_) => NodeType::BlockExpr, // Se for uma expressão de bloco
            Expr::AsmStmt(_) => NodeType::AsmStmt, // Se for um statement de assembly
            Expr::MovStmt(_) => NodeType::MovStmt, // Se for um statement de mov em assembly
            Expr::ArrayExpr(_) => NodeType::ArrayExpr, // Se for um array
            Expr::ArrayAccess(_) => NodeType::ArrayAccess, // Se for um array access
            Expr::UndefinedLiteral(_) => NodeType::UndefinedLiteral, // Se for undefined
            Expr::BreakExpr(_) => NodeType::BreakExpr, // Se for um break
            Expr::LoopStmt(_) => NodeType::LoopStmt, // Se for um loop statement
            Expr::ForStmt(_) => NodeType::ForStmt, // Se for um for statement
            Expr::LogicalNotExpr(_) => NodeType::LogicalNotExpr, // Se for uma expressão como !x
            Expr::UnaryMinusExpr(_) => NodeType::UnaryMinusExpr, // Se for uma expressão como -x
            Expr::PreIncrementExpr(_) => NodeType::PreIncrementExpr, // Se for uma expressão como ++x
            Expr::PostIncrementExpr(_) => NodeType::PostIncrementExpr, // Se for uma expressão como x++
            Expr::PreDecrementExpr(_) => NodeType::PreDecrementExpr, // Se for uma expressão como --x
            Expr::PostDecrementExpr(_) => NodeType::PostDecrementExpr, // Se for uma expressão como x--
            Expr::TrueLiteral(_) => NodeType::TrueLiteral,             // Se for true
            Expr::FalseLiteral(_) => NodeType::FalseLiteral,           // Se for false
            Expr::UnitDeclaration(_) => NodeType::UnitDeclaration, // Se for uma declaração de unit
            Expr::UnitVarDeclaration(_) => NodeType::UnitVarDeclaration, // Se for uma declaração de variável dentro de uma unit
            Expr::UnitFunctionDeclaration(_) => NodeType::UnitFunctionDeclaration, // Se for uma declaração de função dentro de uma unit
        }
    }
}

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
    fn parse_data_type(&mut self) -> String {
        let mut data_type: String = String::new();

        // Match para determinar o tipo de token atual
        match self.at().token_type {
            // Se o token atual for UndefinedType, Text, Integer, Decimal ou Bool
            TokenType::UndefinedType
            | TokenType::Text
            | TokenType::Integer
            | TokenType::Decimal
            | TokenType::Bool => {
                data_type.push_str(&self.eat().value); // Consome o token e adiciona seu valor à string
            }
            // Se o token atual for um Array
            TokenType::Array | TokenType::Object => {
                data_type.push_str(&self.eat().value); // Consome o token e adiciona seu valor à string
                self.expect(TokenType::LessThan, "\"<\" Expected."); // Verifica se o próximo token é "<"

                // Parsear o tipo interno recursivamente
                data_type.push('<');
                data_type.push_str(&self.parse_data_type());

                while self.at().token_type == TokenType::Pipe {
                    data_type.push('|');
                    self.eat();
                    data_type.push_str(&self.parse_data_type());
                }

                self.expect(TokenType::GreaterThan, "\">\" Expected."); // Verifica se o próximo token é ">"
                data_type.push('>');
            }
            // Se o token atual for "<", indicando um tipo de dado composto
            TokenType::LessThan => {
                self.eat(); // Consome o token "<"
                data_type.push('<');
                data_type.push_str(&self.parse_data_type());

                while self.at().token_type == TokenType::Pipe {
                    data_type.push('|');
                    self.eat();
                    data_type.push_str(&self.parse_data_type());
                }

                self.expect(TokenType::GreaterThan, "\">\" Expected."); // Verifica se o próximo token é ">"
                data_type.push('>');
            }
            // Se o token atual não corresponder a nenhum tipo esperado
            _ => {
                self.error(
                    "Expected one of primitive types: Text, Integer, Decimal, Bool, Object<T>, Array<T> or _.",
                );
                // Gera um erro indicando o tipo esperado
            }
        }

        data_type // Retorna a string com o tipo de dado completo
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
        let line: &String = &self.lines.as_ref().unwrap()[lineno - 1]; // Obtém a linha do código fonte
        let column_repr: String = format!(
            "{}{}",
            " ".repeat(column.0 - 1),
            "^".repeat(token.value.len())
        ); // Representação visual da posição do erro na linha

        // Formata a mensagem de erro com informações relevantes
        let formatted_message: String = format!(
            "%%r{} %%b{}%%!:%%y{}%%!:%%y{}%%!:\n\t{}\n\t%%r{}%%!\n%%y{}%%!",
            "ERROR:",
            filename,
            lineno,
            column.0 - 1,
            escape(line),
            column_repr,
            message
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
        while self.not_eof() {
            program.body.push(self.parse_stmt()); // Adiciona o nó de declaração à AST do programa
        }

        program // Retorna a AST do programa
    }

    // Método para analisar uma declaração de instrução
    fn parse_stmt(&mut self) -> Stmt {
        let curtoken: &Token = &self.at(); // Obtém o token atual
        match curtoken.token_type {
            // Se o token atual for um Import, analisa uma declaração de importação
            TokenType::Import => self.parse_import_stmt(),
            // Se o token atual for um Export, analisa uma declaração de exportação
            TokenType::Export => self.parse_export_stmt(),
            // Se o token atual for um If, analisa uma declaração de condicional If
            TokenType::If => self.parse_if_stmt(),
            // Se o token atual for um asm, analisa um statement de código assembly arbitrário
            TokenType::Asm => self.parse_asm_stmt(),
            // Se o token atual for um mov, analisa os valores a serem movidos
            TokenType::Mov => self.parse_mov_stmt(),
            // Se o token atual for um loop, analisa o loop statement
            TokenType::Loop => self.parse_loop_stmt(),
            // Se o token atual for um for, analisa o for statement
            TokenType::For => self.parse_for_stmt(),
            // Se o token atual for uma unit, analisa a unit
            TokenType::Unit => self.parse_unit(),
            // Se o token atual for private ou public, analisa um statement unit
            TokenType::Private | TokenType::Public => self.parse_unit_statement_declaration(),
            // Analisa declaração de variaveis simples, tipadas ou não
            TokenType::Val => self.parse_val_var_declaration(),
            TokenType::Label => {
                Stmt {
                    kind: NodeType::FunctionDeclaration,
                    expr: Some(self.parse_function_declaration()), // Analisa a declaração da função
                    return_stmt: None,
                }
            }
            // Se o token atual for Resb, Resw, Resd, ou Resq, analisa uma declaração de variável
            TokenType::Resb | TokenType::Resw | TokenType::Resd | TokenType::Resq => {
                let data_size: String = self.eat().value; // Obtém o tamanho dos dados

                let mut size: Option<String> = Some("1".to_string());
                // Se houver colchetes, obtém o tamanho especificado
                if self.at().token_type == TokenType::OBracket {
                    size = Some(format!(
                        "{}[{}]",
                        data_size,
                        self.expect(TokenType::Number, "Integer Expected.").value
                    ));
                    self.expect(TokenType::CBracket, "\"]\" Expected.");
                }
                let data_type: String = self.parse_data_type(); // Analisa o tipo de dado
                Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(self.parse_var_declaration(size.unwrap(), data_type, false)), // Analisa a declaração da variável
                    return_stmt: None,
                }
            }
            // Se o token atual for Auto, Byte, Word, Dword, ou Qword, analisa uma declaração de variável
            TokenType::Auto
            | TokenType::Byte
            | TokenType::Word
            | TokenType::Dword
            | TokenType::Qword => {
                let data_size: Token = self.eat(); // Obtém o tamanho dos dados

                // Verifica se é uma declaração de variável automática que não pode ter colchetes
                if data_size.token_type == TokenType::Auto
                    && self.at().token_type == TokenType::OBracket
                {
                    self.error("Automatic size cannot have brackets.");
                }

                let tipo: String = self.parse_data_type(); // Analisa o tipo de dado

                Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(self.parse_var_declaration(data_size.value, tipo, true)), // Analisa a declaração da variável
                    return_stmt: None,
                }
            }
            // Se o token atual for Return, analisa uma declaração de retorno
            TokenType::Return => {
                let return_expr: ReturnStmt = self.parse_return_stmt(); // Analisa a expressão de retorno
                Stmt {
                    kind: NodeType::Stmt,
                    expr: None,
                    return_stmt: Some(return_expr),
                }
            }

            // Se o token atual for Identifier, String, ou Number, analisa uma expressão ou chamada de função
            TokenType::Identifier | TokenType::String | TokenType::Number => {
                // Verifica se o próximo token não é um dos tokens que indicam o final da expressão
                if !matches!(
                    self.next().token_type,
                    TokenType::Semicolon
                        | TokenType::Attribution
                        | TokenType::Colon
                        | TokenType::OBracket
                        | TokenType::Eof
                ) {
                    // Analisa se o retorno é um member expression ou função
                    if self.next().token_type == TokenType::Dot
                        || self.next().token_type == TokenType::OParen
                    {
                        let expr: Option<Expr> = Some(self.parse_call_member_expr());
                        let return_expr: ReturnStmt = ReturnStmt {
                            kind: NodeType::ReturnStmt,
                            argument: expr.clone(),
                        };

                        // Se conter um ";" faz o parsing como call expr normal
                        if self.at().token_type == TokenType::Semicolon {
                            self.eat();
                            return Stmt {
                                kind: NodeType::CallExpr,
                                expr,
                                return_stmt: None,
                            };
                        }

                        // Senão, retorna como return statement
                        Stmt {
                            kind: NodeType::Stmt,
                            expr: None,
                            return_stmt: Some(return_expr),
                        }
                    } else {
                        // Se não for um member expr, continua
                        let expr: Expr = self.parse_expr(); // Analisa a expressão
                        Stmt {
                            kind: expr.kind(),
                            expr: Some(expr),
                            return_stmt: None,
                        }
                    }
                } else {
                    // Placeholder para a variável expr.
                    let mut expr: Expr = Expr::NullLiteral(NullLiteral {
                        kind: NodeType::NullLiteral,
                        value: "Null",
                    });

                    // Loop para permitir chaining de acessos a array
                    loop {
                        match self.next().token_type {
                            TokenType::Attribution => {
                                expr = self.parse_assignment_expr();
                            }
                            TokenType::OParen => {
                                expr = self.parse_expr(); // Analisa a expressão
                                let args: Vec<Box<Expr>> = self.parse_arguments(); // Analisa os argumentos da chamada de função
                                expr = Expr::CallExpr(CallExpr {
                                    kind: NodeType::CallExpr,
                                    caller: Box::new(expr),
                                    args,
                                });
                            }
                            TokenType::OBracket => {
                                expr = self.parse_array_access_expr(); // Analisa a expressão de acesso a índice de array
                            }

                            TokenType::Semicolon => {
                                self.error("\";\" Unexpected.");
                                self.eat();
                                break;
                            }

                            TokenType::Colon => {
                                let identifier: String = self.eat().value;
                                self.eat(); // Consome o token de colon
                                let data_type: String = self.parse_data_type();
                                if self.at().token_type == TokenType::Semicolon {
                                    self.eat();

                                    return Stmt {
                                        kind: NodeType::VarDeclaration,
                                        expr: Some(Expr::VarDeclaration(VarDeclaration {
                                            kind: NodeType::VarDeclaration,
                                            constant: true,
                                            data_size: "auto".to_string(),
                                            data_type,
                                            identifier,
                                            value: Box::from(Expr::UndefinedLiteral(
                                                UndefinedLiteral {
                                                    kind: NodeType::UndefinedLiteral,
                                                    value: "undefined",
                                                },
                                            )),
                                        })),
                                        return_stmt: None,
                                    };
                                } else {
                                    self.expect(TokenType::Attribution, "\"=\" Expected.");

                                    // Parseia a expressão de valor
                                    let value: Box<Expr>;

                                    match self.at().token_type {
                                        TokenType::OBracket => {
                                            value = Box::new(self.parse_array_expr());
                                        }
                                        TokenType::OBrace => {
                                            value = Box::new(self.parse_object_expr())
                                        }
                                        _ => {
                                            value = Box::new(*self.parse_ternary_expr());
                                        }
                                    }

                                    // Espera pelo ponto e vírgula ";"
                                    self.expect(TokenType::Semicolon, "\";\" Expected");

                                    return Stmt {
                                        kind: NodeType::VarDeclaration,
                                        expr: Some(Expr::VarDeclaration(VarDeclaration {
                                            kind: NodeType::VarDeclaration,
                                            constant: true,
                                            data_size: "auto".to_string(),
                                            data_type: "undefined".to_string(),
                                            identifier,
                                            value,
                                        })),
                                        return_stmt: None,
                                    };
                                }
                            }

                            _ => break,
                        }
                    }
                    Stmt {
                        kind: expr.kind(),
                        expr: Some(expr),
                        return_stmt: None,
                    }
                }
            }

            // Outros casos para outros tipos de tokens...
            _ => {
                let expr: Expr = self.parse_expr(); // Analisa a expressão
                Stmt {
                    kind: expr.kind(),
                    expr: Some(expr),
                    return_stmt: None,
                }
            }
        }
    }

    fn parse_val_var_declaration(&mut self) -> Stmt {
        self.eat();
        let identifier: String = self.eat().value;

        // faz parsing de expressões como "val identifier;"
        if self.at().token_type == TokenType::Semicolon {
            self.eat();

            return Stmt {
                kind: NodeType::VarDeclaration,
                expr: Some(Expr::VarDeclaration(VarDeclaration {
                    kind: NodeType::VarDeclaration,
                    constant: false,
                    data_size: "auto".to_string(),
                    data_type: "undefined".to_string(),
                    identifier,
                    value: Box::from(Expr::UndefinedLiteral(UndefinedLiteral {
                        kind: NodeType::UndefinedLiteral,
                        value: "undefined",
                    })),
                })),
                return_stmt: None,
            };
        }

        // faz o parsing de "val identifier: type;" e "val identifier: type = value"
        if self.at().token_type == TokenType::Colon {
            self.eat();

            let data_type: String = self.parse_data_type();
            if self.at().token_type == TokenType::Semicolon {
                self.eat();

                return Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(Expr::VarDeclaration(VarDeclaration {
                        kind: NodeType::VarDeclaration,
                        constant: false,
                        data_size: "auto".to_string(),
                        data_type,
                        identifier,
                        value: Box::from(Expr::UndefinedLiteral(UndefinedLiteral {
                            kind: NodeType::UndefinedLiteral,
                            value: "undefined",
                        })),
                    })),
                    return_stmt: None,
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

                // Espera pelo ponto e vírgula ";"
                self.expect(TokenType::Semicolon, "\";\" Expected");

                return Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(Expr::VarDeclaration(VarDeclaration {
                        kind: NodeType::VarDeclaration,
                        constant: false,
                        data_size: "auto".to_string(),
                        data_type: "undefined".to_string(),
                        identifier,
                        value,
                    })),
                    return_stmt: None,
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

            // Espera pelo ponto e vírgula ";"
            self.expect(TokenType::Semicolon, "\";\" Expected");
            return Stmt {
                kind: NodeType::VarDeclaration,
                expr: Some(Expr::VarDeclaration(VarDeclaration {
                    kind: NodeType::VarDeclaration,
                    constant: false,
                    data_size: "auto".to_string(),
                    data_type: "undefined".to_string(),
                    identifier,
                    value,
                })),
                return_stmt: None,
            };
        }
    }

    fn parse_for_stmt(&mut self) -> Stmt {
        self.eat(); // consome o token de for

        // Espera o token de "(" para o início dos itens
        let mut items: Vec<String> = Vec::new();

        // Enquanto não for end of file ou "}", faz o parsing de statements
        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::Colon {
            items.push(
                self.expect(TokenType::Identifier, "Identifier Expected.")
                    .value,
            ); // adiciona um item aos itens

            // Se houver uma vírgula, consome-a
            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        self.expect(TokenType::Colon, "\":\" Expected.");

        let sequence: Box<Expr> = Box::new(self.parse_array_access_expr());

        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();

        // Enquanto não for end of file ou "}", faz o parsing de statements
        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        Stmt {
            kind: NodeType::ForStmt,
            expr: Some(Expr::ForStmt(ForStmt {
                kind: NodeType::ForStmt,
                items,
                sequence,
                body,
            })),
            return_stmt: None,
        }
    }

    fn parse_loop_stmt(&mut self) -> Stmt {
        self.eat(); // consome o token de loop
        self.expect(TokenType::OBrace, "\"{\" Expected.");

        let mut body: Vec<Stmt> = Vec::new();

        // Enquanto não for end of file ou "}", faz o parsing de statements
        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt());
        }

        self.expect(TokenType::CBrace, "\"}\" Expected.");

        // Retorna o loop statement
        Stmt {
            kind: NodeType::LoopStmt,
            expr: Some(Expr::LoopStmt(LoopStmt {
                kind: NodeType::LoopStmt,
                body,
            })),
            return_stmt: None,
        }
    }

    // Método para analisar uma expressão de acesso a array
    fn parse_array_access_expr(&mut self) -> Expr {
        let object_expr: Expr = self.parse_call_member_expr(); // Analisa a expressão do objeto/array

        // Verifica se há uma abertura de colchete
        if self.at().token_type == TokenType::OBracket {
            self.eat(); // Consome o token de abertura de colchete
            let index_expr: Expr = self.parse_expr(); // Analisa a expressão do índice do array

            // Verifica se há um fechamento de colchete
            self.expect(TokenType::CBracket, "\"]\" Expected.");

            // Verifica se há uma atribuição após o índice do array
            if self.at().token_type == TokenType::Attribution {
                self.eat(); // Consome o operador de atribuição
                let value_expr: Expr = self.parse_expr(); // Analisa a expressão do valor atribuído

                self.expect(TokenType::Semicolon, "\";\" Expected.");
                // Constrói uma expressão de atribuição
                Expr::AssignmentExpr(AssignmentExpr {
                    kind: NodeType::AssignmentExpr,
                    assigne: Box::new(Expr::ArrayAccess(ArrayAccess {
                        kind: NodeType::ArrayAccess,
                        array: Box::new(object_expr),
                        index: Box::new(index_expr),
                    })),
                    value: Box::new(value_expr),
                })
            } else {
                // Se não houver uma atribuição, retorna apenas a expressão de acesso a array
                Expr::ArrayAccess(ArrayAccess {
                    kind: NodeType::ArrayAccess,
                    array: Box::new(object_expr),
                    index: Box::new(index_expr),
                })
            }
        } else {
            // Se não houver abertura de colchete, retorna apenas a expressão do objeto/array
            object_expr
        }
    }

    // Método para analias mov statements
    fn parse_mov_stmt(&mut self) -> Stmt {
        self.eat(); // Consome o token de "mov"

        self.expect(TokenType::OParen, "\"(\" Expected.");
        let mut values: Vec<(String, String)> = Vec::new();

        let val1 = self.expect(TokenType::String, "String Expected.").value;
        self.expect(TokenType::Comma, "\",\" Expected.");
        let val2 = self.expect(TokenType::String, "String Expected.").value;

        values.push((val1, val2));

        self.expect(TokenType::CParen, "\")\" Expected.");

        self.expect(TokenType::Semicolon, "\";\" Expected.");

        let expr = Some(Expr::MovStmt(MovStmt {
            kind: NodeType::MovStmt,
            values,
        }));

        Stmt {
            kind: NodeType::MovStmt,
            expr,
            return_stmt: None,
        }
    }

    // Método para injeção de código assembly arbitrário
    fn parse_asm_stmt(&mut self) -> Stmt {
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
        self.expect(TokenType::Semicolon, "\";\" Expected.");

        Stmt {
            kind: NodeType::AsmStmt,
            expr: Some(Expr::AsmStmt(AsmStmt {
                kind: NodeType::AsmStmt,
                code: asm_code,
            })),
            return_stmt: None,
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

        // Analisa o bloco "else", se presente
        let mut alternate: Option<Vec<Stmt>> = None;
        if self.not_eof() && self.at().token_type == TokenType::Else {
            self.eat();

            // Verifica se há um "if" após o "else"
            if self.not_eof() && self.at().token_type == TokenType::If {
                alternate = Some(vec![self.parse_if_stmt()]);
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

        Stmt {
            kind: NodeType::IfStmt,
            expr: Some(Expr::IfStmt(Box::new(IfStmt {
                kind: NodeType::IfStmt,
                test: Box::new(test),
                consequent,
                alternate,
            }))),
            return_stmt: None,
        }
    }

    fn parse_logical_expr(&mut self) -> Expr {
        let mut expr: Expr = self.parse_equality_expr();

        while self.at().token_type == TokenType::And || self.at().token_type == TokenType::Or {
            let operator: String = self.eat().value.clone(); // Consome o operador lógico
            let right: Expr = self.parse_equality_expr();

            expr = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            });
        }

        expr
    }

    // Método para analisar expressões de igualdade
    fn parse_equality_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_relational_expr();

        while self.at().token_type == TokenType::Equals
            || self.at().token_type == TokenType::NotEquals
        {
            let operator: String = self.eat().value.clone();
            let right: Expr = self.parse_relational_expr();

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left
    }

    // Método para analisar expressões relacionais
    fn parse_relational_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_logical_not_expr();

        while self.at().token_type == TokenType::LessThan
            || self.at().token_type == TokenType::LessThanOrEqual
            || self.at().token_type == TokenType::GreaterThan
            || self.at().token_type == TokenType::GreaterThanOrEqual
        {
            let operator: String = self.eat().value.clone();
            let right: Expr = self.parse_logical_not_expr();

            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left
    }

    // Método para analisar uma expressão de negação lógica
    fn parse_logical_not_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::Exclamation {
            self.eat(); // Consome o operador de negação lógica
            let operand: Expr = self.parse_logical_not_expr(); // Analisa a expressão unária seguinte
            Expr::LogicalNotExpr(LogicalNotExpr {
                kind: NodeType::LogicalNotExpr,
                operand: Box::new(operand),
            })
        } else {
            self.parse_unary_expr() // Chama a análise da próxima expressão unária
        }
    }

    // Método para analisar uma declaração de importação
    fn parse_import_stmt(&mut self) -> Stmt {
        self.eat(); // Consome o token "import"
        self.expect(TokenType::OParen, "\"(\" Expected."); // Verifica e consome o token "("
        self.expect(TokenType::OBracket, "\"[\" Expected."); // Verifica e consome o token "["

        // Aqui ele espera um formato do tipo "path" as identifier, Hashmap<path, Option<identifier>>
        let mut paths: HashMap<String, Option<String>> = HashMap::new(); // Inicializa um hashmap para armazenar os caminhos de importação e seus aliases opcionais

        // Loop para analisar cada caminho de importação e seu alias opcional
        // Enquanto for diferente de "]"
        while self.at().token_type != TokenType::CBracket {
            let path = self
                .expect(TokenType::String, "String Expected.") // Analisa e armazena o caminho de importação
                .value
                .clone(); // Espera uma string para o path

            // Cria um alias possivelmente nulo para a possibilidade de "path" as Identifier
            let alias: Option<String> = match self.at().token_type {
                TokenType::As => {
                    self.eat(); // Consome o token "as"
                    Some(
                        self.expect(TokenType::Identifier, "Identifier Expected.") // Analisa e armazena o alias
                            .value
                            .clone(),
                    )
                }
                _ => None, // caso não tenha "as" ele retorna None
            };

            paths.insert(path, alias); // Insere o caminho de importação e seu alias no hashmap

            // Se tiver virgula, apenas consome ela e retorna ao loop
            if self.at().token_type == TokenType::Comma {
                self.eat();
            }
        }

        self.expect(TokenType::CBracket, "\"]\" Expected."); // Verifica e consome o token "]"
        self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o token ";"

        // Retorna uma estrutura Stmt representando a declaração de importação
        Stmt {
            kind: NodeType::ImportStmt,
            expr: Some(Expr::ImportStmt(ImportStmt {
                kind: NodeType::Stmt,
                paths,
            })),
            return_stmt: None,
        }
    }

    // Método para analisar uma declaração de exportação
    fn parse_export_stmt(&mut self) -> Stmt {
        self.eat(); // Consome o token "export"
        self.expect(TokenType::OParen, "\"(\" Expected."); // Verifica e consome o token "("
        self.expect(TokenType::OBracket, "\"[\" Expected."); // Verifica e consome o token "["

        let mut identifiers: Vec<Identifier> = Vec::new(); // Inicializa um vetor para armazenar os identificadores a serem exportados

        // Loop para analisar cada identificador a ser exportado
        while self.at().token_type != TokenType::CBracket {
            identifiers.push(Identifier {
                kind: NodeType::Identifier,
                symbol: self
                    .expect(TokenType::Identifier, "Identifier Expected.") // Analisa e armazena o identificador
                    .value
                    .clone(),
            });

            if self.at().token_type == TokenType::Comma {
                self.eat(); // Consome a vírgula entre os identificadores
            }
        }

        self.expect(TokenType::CBracket, "\"]\" Expected."); // Verifica e consome o token "]"
        self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o token ";"

        // Retorna uma estrutura Stmt representando a declaração de exportação
        Stmt {
            kind: NodeType::ExportStmt,
            expr: Some(Expr::ExportStmt(ExportStmt {
                kind: NodeType::Stmt,
                identifiers,
            })),
            return_stmt: None,
        }
    }

    // Método para analisar uma declaração de retorno
    fn parse_return_stmt(&mut self) -> ReturnStmt {
        self.expect(TokenType::Return, "\"return\" Expected."); // Consome o token "return"

        // Usa parse_call_member_expr para analisar a expressão de retorno completa
        let expr = self.parse_call_member_expr();

        if self.tokens.get(self.index - 1).unwrap().token_type == TokenType::Semicolon {
            return ReturnStmt {
                kind: NodeType::ReturnStmt,
                argument: Some(expr), // Retorna a expressão analisada
            };
        }
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o ponto e vírgula

        ReturnStmt {
            kind: NodeType::ReturnStmt,
            argument: Some(expr), // Retorna a expressão analisada
        }
    }

    // Método para analisar a declaração de uma função
    fn parse_function_declaration(&mut self) -> Expr {
        self.eat(); // Consome o token "label"
        let name: String = self
            .expect(TokenType::Identifier, "Function name expected") // Analisa e armazena o nome da função
            .value;
        //oi1
        let mut parameters: Vec<(String, String, String)> = Vec::new(); // Inicializa um vetor para armazenar os parâmetros da função

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
                    let identifier: String = self
                        .expect(TokenType::Identifier, "Parameter name expected") // Analisa e armazena o nome do parâmetro
                        .value;

                    let mut size: String = "undefined".to_string(); // Obtém o tamanho do parâmetro
                    let mut data_type: String = "undefined".to_string(); // Analisa e obtém o tipo de dados do parâmetro
                    if self.at().token_type == TokenType::Colon {
                        self.eat();
                        match self.at().token_type {
                            TokenType::Auto
                            | TokenType::Byte
                            | TokenType::Word
                            | TokenType::Dword
                            | TokenType::Qword => size = self.eat().value,
                            TokenType::UndefinedType
                            | TokenType::Text
                            | TokenType::Integer
                            | TokenType::Decimal
                            | TokenType::Array
                            | TokenType::Object => {
                                data_type = self.parse_data_type();
                            }
                            _ => self.error("Unexpected token "),
                        }

                        match self.at().token_type {
                            TokenType::UndefinedType
                            | TokenType::Text
                            | TokenType::Integer
                            | TokenType::Decimal
                            | TokenType::Array
                            | TokenType::Object => {
                                data_type = self.parse_data_type();
                            }
                            _ => self.error("Unexpected token "),
                        }
                    }

                    parameters.push((size, data_type, identifier)); // Adiciona o parâmetro ao vetor de parâmetros

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

        let mut return_size: String = "undefined".to_string();
        let mut return_type: String = "undefined".to_string();

        if self.at().token_type == TokenType::Colon {
            self.eat();
            match self.at().token_type {
                TokenType::Auto
                | TokenType::Byte
                | TokenType::Word
                | TokenType::Dword
                | TokenType::Qword => {
                    return_size = self.eat().value;
                    match self.at().token_type {
                        TokenType::UndefinedType
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
                TokenType::UndefinedType
                | TokenType::Text
                | TokenType::Integer
                | TokenType::Decimal
                | TokenType::Array
                | TokenType::Object => {
                    return_type = self.parse_data_type();
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

        // Retorna uma estrutura Expr representando a declaração da função
        Expr::FunctionDeclaration(Box::new(FunctionDeclaration {
            kind: NodeType::FunctionDeclaration,
            return_size,
            return_type,
            name,
            parameters,
            body,
        }))
    }

    fn parse_unit_statement_declaration(&mut self) -> Stmt {
        let access_modifier: String = match self.at().token_type {
            TokenType::Public | TokenType::Private => self.eat().value.clone(),
            _ => "private".to_string(),
        };

        if self.at().token_type == TokenType::Label {
            return Stmt {
                kind: NodeType::UnitFunctionDeclaration,
                expr: Some(Expr::UnitFunctionDeclaration(Box::new(
                    UnitFunctionDeclaration {
                        kind: NodeType::UnitFunctionDeclaration,
                        access_modifier,
                        function: self.parse_function_declaration(),
                    },
                ))),
                return_stmt: None,
            };
        }

        self.parse_stmt()
    }

    // Método para analisar uma unidade (unit)
    fn parse_unit(&mut self) -> Stmt {
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

        let expr: Option<Expr> = Some(Expr::UnitDeclaration(Box::new(UnitDeclaration {
            kind: NodeType::UnitDeclaration,
            name,
            super_units,
            body,
        })));

        Stmt {
            kind: NodeType::UnitDeclaration,
            expr,
            return_stmt: None,
        }
    }

    // Método para analisar uma declaração de variável
    fn parse_var_declaration(
        &mut self,
        data_size: String,
        data_type: String,
        is_constant: bool,
    ) -> Expr {
        // Se a variável é constante
        if is_constant {
            // Pega o identificador da variável
            let identifier: String = self
                .expect(TokenType::Identifier, "Identifier Expected.")
                .value;
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

            // Retorna uma declaração de variável constante
            Expr::VarDeclaration(VarDeclaration {
                kind: NodeType::VarDeclaration,
                constant: is_constant,
                data_size,
                data_type,
                identifier,
                value,
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

            // Retorna uma declaração de variável
            Expr::VarDeclaration(VarDeclaration {
                kind: NodeType::VarDeclaration,
                constant: is_constant,
                data_size,
                data_type,
                identifier,
                value,
            })
        }
    }

    // Método para analisar uma expressão ternária
    fn parse_ternary_expr(&mut self) -> Box<Expr> {
        // Verifica se o token atual é um identificador que pode ser seguido por um acesso a índice de array
        if self.at().token_type == TokenType::Identifier
            && self.next().token_type == TokenType::OBracket
        {
            return Box::new(self.parse_array_access_expr()); // Chamada de parse_array_access_expr sem argumentos
        }

        let condition = self.parse_logical_expr(); // Analisa a condição da expressão ternária

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

        // Retorna uma estrutura Box<Expr> representando a expressão ternária
        Box::new(Expr::TernaryExpr(TernaryExpr {
            kind: NodeType::TernaryExpr,
            condition: Box::new(condition),
            consequent,
            alternate,
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
            // Verifica se a expressão está entre chaves
            self.eat(); // Consome o token "{"
            let mut statements: Vec<Stmt> = Vec::new();
            // Loop para analisar cada instrução dentro do bloco
            while self.at().token_type != TokenType::CBrace {
                statements.push(self.parse_stmt()); // Analisa e adiciona a instrução ao vetor de instruções
            }
            self.expect(TokenType::CBrace, "\"}\" Expected."); // Verifica e consome o token "}"
                                                               // Retorna uma expressão representando um bloco de instruções
            Box::new(Expr::BlockExpr(Box::new(BlockExpr {
                kind: NodeType::BlockExpr,
                statements,
            })))
        } else {
            Box::new(self.parse_expr()) // Retorna a expressão analisada
        }
    }

    // Método para analisar uma expressão unária
    fn parse_unary_expr(&mut self) -> Expr {
        let expr = match self.at().token_type {
            TokenType::Minus => {
                self.eat(); // Consome o operador de menos unário
                let operand = self.parse_unary_expr(); // Analise a expressão unária seguinte
                Expr::UnaryMinusExpr(UnaryMinusExpr {
                    kind: NodeType::UnaryMinusExpr,
                    operand: Box::new(operand),
                })
            }
            TokenType::Exclamation => {
                self.eat(); // Consome o operador de negação lógica
                let operand = self.parse_unary_expr(); // Analise a expressão unária seguinte
                Expr::LogicalNotExpr(LogicalNotExpr {
                    kind: NodeType::LogicalNotExpr,
                    operand: Box::new(operand),
                })
            }
            TokenType::Increment => {
                self.eat(); // Consome o operador de pré-incremento
                let operand = self.parse_unary_expr(); // Analise a expressão unária seguinte
                Expr::PreIncrementExpr(PreIncrementExpr {
                    kind: NodeType::PreIncrementExpr,
                    operand: Box::new(operand),
                })
            }
            TokenType::Decrement => {
                self.eat(); // Consome o operador de pré-decremento
                let operand = self.parse_unary_expr(); // Analise a expressão unária seguinte
                Expr::PreDecrementExpr(PreDecrementExpr {
                    kind: NodeType::PreDecrementExpr,
                    operand: Box::new(operand),
                })
            }
            _ => return self.parse_postfix_expr(), // Analise a expressão primária e depois verifique por pós-fixação
        };

        // Se já não houver um semicolon, ele espera que haja
        if self.at().token_type != TokenType::Semicolon {
            self.expect(TokenType::Semicolon, "\";\" Expected.");
        } else {
            self.eat();
        }

        expr
    }

    /// Método para analisar uma expressão pós-fixada (incremento/decremento após a expressão)
    fn parse_postfix_expr(&mut self) -> Expr {
        let mut expr: Expr = self.parse_call_member_expr(); // Analise acesso de array

        loop {
            match self.at().token_type {
                TokenType::Increment => {
                    self.eat(); // Consome o operador de pós-incremento
                    expr = Expr::PostIncrementExpr(PostIncrementExpr {
                        kind: NodeType::PostIncrementExpr,
                        operand: Box::new(expr),
                    });
                }
                TokenType::Decrement => {
                    self.eat(); // Consome o operador de pós-decremento
                    expr = Expr::PostDecrementExpr(PostDecrementExpr {
                        kind: NodeType::PostDecrementExpr,
                        operand: Box::new(expr),
                    });
                }
                _ => break, // Sai do loop se não houver mais operadores pós-fixados
            }
        }

        expr // Retorna a expressão final
    }

    // Atualize o método parse_expr para chamar parse_unary_expr
    fn parse_expr(&mut self) -> Expr {
        if self.at().token_type == TokenType::Minus
            || self.at().token_type == TokenType::Increment
            || self.at().token_type == TokenType::Decrement
            || self.next().token_type == TokenType::Exclamation
            || self.next().token_type == TokenType::Increment
            || self.next().token_type == TokenType::Decrement
        {
            return self.parse_unary_expr();
        }

        self.parse_assignment_expr()
    }

    // Método para analisar uma expressão de atribuição
    fn parse_assignment_expr(&mut self) -> Expr {
        let mut left: Expr = *self.parse_ternary_expr();

        while self.at().token_type == TokenType::Attribution {
            self.eat();
            let right: Expr = *self.parse_ternary_expr();
            left = Expr::AssignmentExpr(AssignmentExpr {
                kind: NodeType::AssignmentExpr,
                assigne: Box::new(left),
                value: Box::new(right),
            });
            self.expect(TokenType::Semicolon, "\";\" Expected.");
        }

        left
    }

    // Método para analisar uma expressão de array
    fn parse_array_expr(&mut self) -> Expr {
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

        //
        Expr::ArrayExpr(ArrayExpr {
            kind: NodeType::ArrayExpr,
            elements: array,
        })
    }

    // Método para analisar uma expressão de objeto
    fn parse_object_expr(&mut self) -> Expr {
        // Verifica se o token atual é uma abertura de chave "{"
        if self.at().token_type != TokenType::OBrace {
            return self.parse_additive_expr();
        }

        // Consome a abertura de chave "{"
        self.eat();
        let mut properties: Vec<Property> = Vec::new();

        // Loop até encontrar o fechamento de chave "}" ou fim do arquivo
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
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
                })))
            } else {
                // Espera pelo token de dois pontos ":"
                self.expect(TokenType::Colon, "\":\" Expected.");
                // Parseia o valor associado à chave
                Some(Box::new(self.parse_expr()))
            };

            // Adiciona a propriedade ao vetor de propriedades
            properties.push(Property {
                kind: NodeType::Property,
                key,
                value,
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
        // Retorna um literal de objeto
        Expr::ObjectLiteral(ObjectLiteral {
            kind: NodeType::ObjectLiteral,
            properties,
        })
    }

    // Método para analisar uma expressão aditiva
    fn parse_additive_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_multiplicative_expr(); // Analisa a expressão multiplicativa à esquerda

        // Loop para lidar com operadores de adição e subtração
        while self.at().value == "+" || self.at().value == "-" {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_multiplicative_expr(); // Analisa a expressão multiplicativa à direita

            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão multiplicativa
    fn parse_multiplicative_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_exponential_expr(); // Analisa a expressão exponencial à esquerda

        // Loop para lidar com operadores de multiplicação, divisão e módulo
        while self.at().value == "*" || self.at().value == "/" || self.at().value == "%" {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_exponential_expr(); // Analisa a expressão exponencial à direita

            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão exponencial
    fn parse_exponential_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_call_member_expr(); // Analisa a expressão de chamada/membro à esquerda

        // Loop para lidar com operadores de exponenciação
        while self.at().value == "^" {
            let operator: String = self.eat().value; // Consome o operador
            let right: Expr = self.parse_call_member_expr(); // Analisa a expressão de chamada/membro à direita

            // Constrói uma expressão binária com o operador e as expressões esquerda e direita
            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left // Retorna a expressão analisada
    }

    // Método para analisar uma expressão de chamada ou membro
    fn parse_call_member_expr(&mut self) -> Expr {
        let mut expr: Expr = self.parse_primary_expr(); // Começa com uma expressão primária

        loop {
            match self.at().token_type {
                TokenType::Dot => {
                    self.eat(); // Consome o ponto
                    let property: Token =
                        self.expect(TokenType::Identifier, "Identifier Expected.");
                    expr = Expr::MemberExpr(MemberExpr {
                        kind: NodeType::MemberExpr,
                        object: Box::new(expr),
                        property: Box::new(Expr::Identifier(Identifier {
                            kind: NodeType::Identifier,
                            symbol: property.value,
                        })),
                    });
                }
                TokenType::OParen => {
                    expr = self.parse_call_expr(expr); // Analisa a expressão de chamada associada
                }
                TokenType::OBracket => {
                    expr = self.parse_array_access_expr(); // Chama a função para analisar a expressão de acesso a array
                }
                _ => break, // Sai do loop se não houver mais operações
            }
        }

        expr // Retorna a expressão final
    }

    // Método para analisar uma expressão de chamada
    fn parse_call_expr(&mut self, caller: Expr) -> Expr {
        self.expect(TokenType::OParen, "\"(\" Expected."); // Espera o token '('
        let args: Vec<Box<Expr>> = self.parse_args(); // Analisa os argumentos da chamada de função

        let expr: Expr = Expr::CallExpr(CallExpr {
            kind: NodeType::CallExpr,
            caller: Box::new(caller),
            args,
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

    // Método para analisar uma expressão primária
    fn parse_primary_expr(&mut self) -> Expr {
        // Verifica o tipo de token atual
        match self.at().token_type {
            TokenType::Identifier => {
                // Se for um identificador, cria uma expressão de identificador
                Expr::Identifier(Identifier {
                    kind: NodeType::Identifier,
                    symbol: self.eat().value, // Consome o token e obtém o valor do identificador
                })
            }
            TokenType::Number => {
                // Se for um número, cria uma expressão de literal numérico
                Expr::NumericLiteral(NumericLiteral {
                    kind: NodeType::NumericLiteral,
                    value: self.eat().value, // Consome o token e obtém o valor numérico
                })
            }
            TokenType::String => {
                // Se for uma string, cria uma expressão de literal de string
                Expr::StringLiteral(StringLiteral {
                    kind: NodeType::StringLiteral,
                    value: self.eat().value, // Consome o token e obtém o valor da string
                })
            }
            TokenType::Null => {
                // Se for nulo, consome o token e cria uma expressão de literal nulo
                self.eat();
                Expr::NullLiteral(NullLiteral {
                    kind: NodeType::NullLiteral,
                    value: "Null", // Define o valor como "Null"
                })
            }

            // Se for um undefined
            TokenType::Undefined => {
                self.eat();
                Expr::UndefinedLiteral(UndefinedLiteral {
                    kind: NodeType::UndefinedLiteral,
                    value: "undefined",
                })
            }

            // Se for um true
            TokenType::True => {
                self.eat();
                Expr::TrueLiteral(TrueLiteral {
                    kind: NodeType::TrueLiteral,
                })
            }

            // Se for um false
            TokenType::False => {
                self.eat();
                Expr::FalseLiteral(FalseLiteral {
                    kind: NodeType::FalseLiteral,
                })
            }

            // Se for um break;
            TokenType::Break => {
                self.eat();
                // Espera um semicolon
                self.expect(TokenType::Semicolon, "\";\" Expected.");
                Expr::BreakExpr(BreakExpr {
                    kind: NodeType::BreakExpr,
                })
            }

            TokenType::OParen => {
                // Se for um parêntese aberto, consome o token e analisa a expressão dentro dos parênteses
                self.eat();
                let expr = self.parse_expr(); // Analisa a expressão dentro dos parênteses
                self.expect(TokenType::CParen, "\")\" Expected."); // Espera um parêntese fechado
                expr // Retorna a expressão analisada
            }
            _ => {
                // Se for qualquer outro tipo de token
                self.error(&format!("Unexpected token: {:?}", self.at().token_type)); // Gera um erro indicando um token inesperado
                                                                                      // Retorna uma expressão nula como fallback
                Expr::NullLiteral(NullLiteral {
                    kind: NodeType::NullLiteral,
                    value: "Null", // Define o valor como "Null"
                })
            }
        }
    }
}
