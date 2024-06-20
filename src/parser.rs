use crate::ast::*;
use crate::colors::{escape, printc};
use crate::lexer::{Token, TokenType};

// Implementação de métodos para a enumeração Expr
impl Expr {
    // Método que retorna o tipo do nó da árvore de sintaxe abstrata (AST)
    fn kind(&self) -> NodeType {
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
            Expr::AsmStmt(_) => NodeType::AsmStmt, // Se for uma expressão de assembly
            Expr::ArrayExpr(_) => NodeType::ArrayExpr,
            Expr::ArrayAccess(_) => NodeType::ArrayAccess, // Se for um array
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>, // Um vetor de tokens que representa a entrada do parser
    errstate: bool,     // Um indicador de estado de erro para o parser
    lines: Option<Vec<String>>, // Uma opção de vetor de strings representando as linhas do código fonte
    index: usize,               // Um índice para rastrear a posição atual durante o parsing
}

impl Parser {
    // Método de criação de uma nova instância de Parser
    pub fn new() -> Parser {
        // Criação de uma nova instância de Parser com valores padrão
        Parser {
            tokens: Vec::new(), // Inicializa o vetor de tokens como vazio
            errstate: false,    // Define o estado de erro como falso
            lines: None,        // Inicializa as linhas como nenhuma (None)
            index: 0,           // Define o índice inicial como 0
        }
    }

    // Método para verificar se não é o final do arquivo (end of file - EOF)
    fn not_eof(&self) -> bool {
        self.at().token_type != TokenType::Eof
    }

    // Método para obter o token atual
    fn at(&self) -> Token {
        self.tokens[self.index].clone() // Retorna o token na posição atual do índice
    }

    // Método para consumir o token atual e avançar para o próximo
    fn eat(&mut self) -> Token {
        let tok: Token = self.at(); // Obtém o token atual
        self.index += 1; // Avança para o próximo token
        tok // Retorna o token atual consumido
    }

    // Método para analisar o tipo de dado
    fn parse_data_type(&mut self) -> String {
        let mut data_type = String::new();

        // Match para determinar o tipo de token atual
        match self.at().token_type {
            // Se o token atual for Text, Integer, Decimal ou Bool
            TokenType::Text | TokenType::Integer | TokenType::Decimal | TokenType::Bool => {
                data_type.push_str(&self.eat().value); // Consome o token e adiciona seu valor à string
            }
            // Se o token atual for um Array
            TokenType::Array | TokenType::Object => {
                data_type.push_str(&self.eat().value); // Consome o token e adiciona seu valor à string
                self.expect(TokenType::LessThan, "\"<\" Expected."); // Verifica se o próximo token é "<"

                // Parsear o tipo interno recursivamente
                data_type.push('<');
                data_type.push_str(&self.parse_data_type());

                while self.at().token_type == TokenType::Comma {
                    data_type.push(',');
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

                while self.at().token_type == TokenType::Comma {
                    data_type.push(',');
                    self.eat();
                    data_type.push_str(&self.parse_data_type());
                }

                self.expect(TokenType::GreaterThan, "\">\" Expected."); // Verifica se o próximo token é ">"
                data_type.push('>');
            }
            // Se o token atual não corresponder a nenhum tipo esperado
            _ => {
                self.error("Expected one of: Text, Integer, Decimal, Bool, Object<T>, Array<T>");
                // Gera um erro indicando o tipo esperado
            }
        }

        data_type // Retorna a string com o tipo de dado completo
    }

    // Método para esperar um tipo específico de token
    fn expect(&mut self, expected_type: TokenType, err: &str) -> Token {
        let prev = self.eat(); // Consome o token anterior

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
        let token: Token = self.at(); // Obtém o token atual
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
        let formatted_message = format!(
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
        let curtoken: Token = self.at(); // Obtém o token atual
        match curtoken.token_type {
            // Se o token atual for um Import, analisa uma declaração de importação
            TokenType::Import => self.parse_import_stmt(),
            // Se o token atual for um Export, analisa uma declaração de exportação
            TokenType::Export => self.parse_export_stmt(),
            // Se o token atual for um If, analisa uma declaração de condicional If
            TokenType::If => self.parse_if_stmt(),
            // Se o token atual for um asm, analisa um statement de código assembly arbitrário
            TokenType::Asm => self.parse_asm_stmt(),
            // Se o token atual for Auto, Resb, Resw, Resd, ou Resq, analisa uma declaração de variável ou função
            TokenType::Auto
            | TokenType::Resb
            | TokenType::Resw
            | TokenType::Resd
            | TokenType::Resq => {
                let data_size: String = self.eat().value; // Obtém o tamanho dos dados
                                                          // Verifica se é uma declaração de variável automática que não pode ter colchetes
                if curtoken.token_type == TokenType::Auto
                    && self.at().token_type == TokenType::OBracket
                {
                    self.error("Automatic size cannot have brackets.");
                }
                let mut size: Option<String> = None;
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
            // Se o token atual for Byte, Word, Dword, ou Qword, analisa uma declaração de variável ou função
            TokenType::Byte | TokenType::Word | TokenType::Dword | TokenType::Qword => {
                let data_size: String = self.eat().value; // Obtém o tamanho dos dados
                let tipo: String = self.parse_data_type(); // Analisa o tipo de dado
                self.expect(TokenType::Separator, "\"::\" Expected.");
                // Se o próximo token for um Label, analisa uma declaração de função
                if self.at().token_type == TokenType::Label {
                    self.eat();
                    return Stmt {
                        kind: NodeType::FunctionDeclaration,
                        expr: Some(self.parse_function_declaration(data_size, tipo)), // Analisa a declaração da função
                        return_stmt: None,
                    };
                }

                Stmt {
                    kind: NodeType::VarDeclaration,
                    expr: Some(self.parse_var_declaration(data_size, tipo, true)), // Analisa a declaração da variável
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
                if let Some(next_token) = self.tokens.get(self.index + 1) {
                    println!("{:?}", self.at().value);
                    println!("{:?}", next_token.value);
                    if next_token.token_type != TokenType::Semicolon
                        && next_token.token_type != TokenType::Attribution
                        && next_token.token_type != TokenType::Dot
                        && next_token.token_type != TokenType::OParen
                        && next_token.token_type != TokenType::Colon
                        && next_token.token_type != TokenType::OBracket
                    {
                        let ident: String = self.eat().value; // Obtém o identificador
                        let return_expr: ReturnStmt = ReturnStmt {
                            kind: NodeType::ReturnStmt,
                            argument: Some(*Box::new(Expr::Identifier(Identifier {
                                kind: NodeType::Identifier,
                                symbol: ident.clone(),
                            }))),
                        };
                        Stmt {
                            kind: NodeType::Stmt,
                            expr: None,
                            return_stmt: Some(return_expr),
                        }
                    } else {
                        let mut expr: Expr = self.parse_expr(); // Analisa a expressão
                                                                // Loop para permitir chaining de acessos a array
                        while let Some(next_token) = self.tokens.get(self.index) {
                            if next_token.token_type == TokenType::OParen {
                                let args: Vec<Box<Expr>> = self.parse_arguments(); // Analisa os argumentos da chamada de função
                                expr = Expr::CallExpr(CallExpr {
                                    kind: NodeType::CallExpr,
                                    caller: Box::new(expr),
                                    args,
                                });
                            } else if next_token.token_type == TokenType::OBracket {
                                expr = self.parse_array_access_expr(expr); // Analisa a expressão de acesso a índice de array
                            } else {
                                break;
                            }
                        }
                        Stmt {
                            kind: expr.kind(),
                            expr: Some(expr),
                            return_stmt: None,
                        }
                    }
                } else {
                    let expr: Expr = self.parse_expr(); // Analisa a expressão
                    Stmt {
                        kind: expr.kind(),
                        expr: Some(expr),
                        return_stmt: None,
                    }
                }
            }
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

    // Método para analisar uma expressão de acesso a índice de array
    fn parse_array_access_expr(&mut self, array_expr: Expr) -> Expr {
        let mut expr = array_expr;
        while self.at().token_type == TokenType::OBracket {
            self.eat(); // Consome o '['
            let index_expr = self.parse_expr(); // Analisa a expressão de índice
            self.expect(TokenType::CBracket, "\"]\" Expected."); // Espera um ']'
            expr = Expr::ArrayAccess(ArrayAccess {
                kind: NodeType::ArrayAccess,
                array: Box::new(expr),
                index: Box::new(index_expr),
            });
        }
        expr
    }

    // Método para injeção de código assembly arbitrário
    fn parse_asm_stmt(&mut self) -> Stmt {
        let mut asm_code: Vec<Expr> = Vec::new();

        self.eat();
        self.expect(TokenType::Colon, "\":\" Expected.");
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

    // Método para analisar uma declaração "if"
    fn parse_if_stmt(&mut self) -> Stmt {
        self.eat(); // Consome o token "if"
        self.expect(TokenType::OParen, "\"(\" Expected."); // Verifica e consome o token "("

        let test: Expr = self.parse_logical_expr(); // Analisa a expressão lógica dentro dos parênteses

        self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
        self.expect(TokenType::OBrace, "\"{\" Expected."); // Verifica e consome o token "{"

        let mut consequent: Vec<Stmt> = Vec::new(); // Inicializa um vetor para armazenar o corpo do "if"
                                                    // Loop para analisar cada instrução dentro do corpo do "if"
        while self.not_eof() && self.at().token_type != TokenType::CBrace {
            consequent.push(self.parse_stmt()); // Analisa e armazena as instruções no vetor
        }

        self.expect(TokenType::CBrace, "\"}\" Expected."); // Verifica e consome o token "}"

        let mut alternate: Option<Vec<Stmt>> = None; // Inicializa uma opção para o bloco "else"

        // Verifica se há um bloco "else"
        if self.not_eof() && self.at().token_type == TokenType::Else {
            self.eat(); // Consome o token "else"

            // Verifica se há um "if" após o "else"
            if self.not_eof() && self.at().token_type == TokenType::If {
                alternate = Some(vec![self.parse_if_stmt()]); // Analisa o "if" e armazena no bloco "else"
            } else {
                self.expect(TokenType::OBrace, "\"{\" Expected."); // Verifica e consome o token "{"

                let mut else_body: Vec<Stmt> = Vec::new(); // Inicializa um vetor para armazenar o corpo do "else"
                                                           // Loop para analisar cada instrução dentro do corpo do "else"
                while self.not_eof() && self.at().token_type != TokenType::CBrace {
                    else_body.push(self.parse_stmt()); // Analisa e armazena as instruções no vetor
                }

                self.expect(TokenType::CBrace, "\"}\" Expected."); // Verifica e consome o token "}"
                alternate = Some(else_body); // Armazena o corpo do "else" na opção
            }
        }

        // Retorna uma estrutura Stmt representando a declaração "if"
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

    // Método para analisar expressões lógicas
    fn parse_logical_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_equality_expr(); // Analisa a expressão de igualdade à esquerda

        // Loop para analisar operadores lógicos e expressões à direita
        while self.at().token_type == TokenType::And || self.at().token_type == TokenType::Or {
            let operator: String = self.eat().value.clone(); // Consome o operador lógico
            let right: Expr = self.parse_equality_expr(); // Analisa a expressão de igualdade à direita

            // Constrói uma expressão binária com o operador lógico e as expressões à esquerda e à direita
            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left // Retorna a expressão lógica construída
    }

    // Método para analisar expressões de igualdade
    fn parse_equality_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_relational_expr(); // Analisa a expressão relacional à esquerda

        // Loop para analisar operadores de igualdade e expressões à direita
        while self.at().token_type == TokenType::Equals
            || self.at().token_type == TokenType::NotEquals
        {
            let operator: String = self.eat().value.clone(); // Consome o operador de igualdade
            let right: Expr = self.parse_relational_expr(); // Analisa a expressão relacional à direita

            // Constrói uma expressão binária com o operador de igualdade e as expressões à esquerda e à direita
            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left // Retorna a expressão de igualdade construída
    }

    // Método para analisar expressões relacionais
    fn parse_relational_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_additive_expr(); // Analisa a expressão aditiva à esquerda

        // Loop para analisar operadores relacionais e expressões à direita
        while self.at().token_type == TokenType::LessThan
            || self.at().token_type == TokenType::LessThanOrEqual
            || self.at().token_type == TokenType::GreaterThan
            || self.at().token_type == TokenType::GreaterThanOrEqual
        {
            let operator: String = self.eat().value.clone(); // Consome o operador relacional
            let right: Expr = self.parse_additive_expr(); // Analisa a expressão aditiva à direita

            // Constrói uma expressão binária com o operador relacional e as expressões à esquerda e à direita
            left = Expr::BinaryExpr(BinaryExpr {
                kind: NodeType::BinaryExpr,
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }

        left // Retorna a expressão relacional construída
    }

    // Método para analisar uma declaração de importação
    fn parse_import_stmt(&mut self) -> Stmt {
        self.eat(); // Consome o token "import"
        self.expect(TokenType::Colon, "\":\" Expected."); // Verifica e consome o token ":"
        self.expect(TokenType::OParen, "\"(\" Expected."); // Verifica e consome o token "("

        let mut paths: Vec<String> = Vec::new(); // Inicializa um vetor para armazenar os caminhos de importação

        // Loop para analisar cada caminho de importação
        while self.at().token_type != TokenType::CParen {
            paths.push(
                self.expect(TokenType::String, "String Expected.") // Analisa e armazena o caminho de importação
                    .value
                    .clone(),
            );

            if self.at().token_type == TokenType::Comma {
                self.eat(); // Consome a vírgula entre os caminhos de importação
            }
        }

        self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o token ";"

        // Retorna uma estrutura Stmt representando a declaração de importação
        Stmt {
            kind: NodeType::Stmt,
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
        self.expect(TokenType::Colon, "\":\" Expected."); // Verifica e consome o token ":"
        self.expect(TokenType::OParen, "\"(\" Expected."); // Verifica e consome o token "("

        let mut identifiers: Vec<Identifier> = Vec::new(); // Inicializa um vetor para armazenar os identificadores a serem exportados

        // Loop para analisar cada identificador a ser exportado
        while self.at().token_type != TokenType::CParen {
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

        self.expect(TokenType::CParen, "\")\" Expected."); // Verifica e consome o token ")"
        self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o token ";"

        // Retorna uma estrutura Stmt representando a declaração de exportação
        Stmt {
            kind: NodeType::Stmt,
            expr: Some(Expr::ExportStmt(ExportStmt {
                kind: NodeType::Stmt,
                identifiers,
            })),
            return_stmt: None,
        }
    }

    // Método para analisar uma declaração de retorno
    fn parse_return_stmt(&mut self) -> ReturnStmt {
        self.expect(TokenType::Return, "\"return\" Expected."); // Consome a palavra-chave "return"
        let mut expr: Expr = self.parse_expr(); // Analisa a expressão de retorno

        // Loop para permitir chaining de acessos a array
        while let Some(next_token) = self.tokens.get(self.index) {
            if next_token.token_type == TokenType::OBracket {
                expr = self.parse_array_access_expr(expr); // Analisa a expressão de acesso a índice de array
            } else {
                break;
            }
        }

        ReturnStmt {
            kind: NodeType::ReturnStmt,
            argument: Some(*Box::new(expr)), // Retorna a expressão analisada
        }
    }

    // Método para analisar a declaração de uma função
    fn parse_function_declaration(&mut self, return_size: String, return_type: String) -> Expr {
        self.expect(TokenType::Colon, "\":\" Expected."); // Verifica e consome o token ":"
        let name: String = self
            .expect(TokenType::Identifier, "Function name expected") // Analisa e armazena o nome da função
            .value;
        self.expect(TokenType::Colon, "\":\" Expected."); // Verifica e consome o token ":"

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
                    let size = self.eat().value.clone(); // Obtém o tamanho do parâmetro
                    let data_type = self.parse_data_type(); // Analisa e obtém o tipo de dados do parâmetro
                    self.expect(TokenType::Separator, "\"::\" Expected."); // Verifica e consome o token "::"
                    let identifier = self
                        .expect(TokenType::Identifier, "Parameter name expected") // Analisa e armazena o nome do parâmetro
                        .value;

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

        self.expect(TokenType::Arrow, "\"=>\" Expected."); // Verifica e consome o token "=>"
        self.expect(TokenType::OBrace, "\"{\" Expected."); // Verifica e consome o token "{"

        let mut body: Vec<Stmt> = Vec::new(); // Inicializa um vetor para armazenar o corpo da função

        // Loop para analisar cada instrução no corpo da função
        while self.at().token_type != TokenType::Eof && self.at().token_type != TokenType::CBrace {
            body.push(self.parse_stmt()); // Analisa e adiciona a instrução ao vetor de corpo
        }

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
            // Espera pelo token de atribuição "<<"
            self.expect(TokenType::Attribution, "\"<<\" Expected.");

            // Parseia a expressão de valor
            let value: Box<Expr> = self.parse_ternary_expr();

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
            // Espera pelo token de separador "::"
            self.expect(TokenType::Separator, "\"::\" Expected.");
            // Pega o identificador da variável
            let identifier: String = self
                .expect(TokenType::Identifier, "Identifier Expected.")
                .value;
            // Espera pelo token de atribuição "<<"
            self.expect(TokenType::Attribution, "\"<<\" Expected.");

            // Verifica se a inicialização é um objeto ou uma expressão
            let value: Box<Expr> = if self.at().token_type == TokenType::OBrace {
                Box::new(self.parse_array_expr()) // Parseia como objeto
            } else {
                self.parse_ternary_expr() // Parseia como expressão
            };

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
        let condition = self.parse_logical_expr(); // Analisa a condição da expressão ternária

        // Verifica se há um operador ternário "?"
        if self.at().token_type != TokenType::QuestionMark {
            return Box::new(condition); // Retorna a condição se não houver operador ternário
        }
        self.eat(); // Consome o operador ternário "?"

        let consequent: Box<Expr> = self.parse_nested_ternary_expr(); // Analisa a expressão consequente

        // Verifica se há um separador "::"
        if self.at().token_type != TokenType::Separator {
            self.error("\"::\" Expected after '?' in ternary expression."); // Emite um erro se não houver separador "::"
            return Box::new(condition); // Retorna a condição em caso de erro
        }
        self.eat(); // Consome o separador "::"

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

    // Método para analisar uma expressão
    fn parse_expr(&mut self) -> Expr {
        self.parse_assignment_expr() // Chama o método para analisar uma expressão de atribuição
    }

    // Método para analisar uma expressão de atribuição
    fn parse_assignment_expr(&mut self) -> Expr {
        let mut left: Expr = self.parse_array_expr(); // Analisa a expressão do lado esquerdo da atribuição

        // Loop para lidar com múltiplas atribuições consecutivas
        while self.at().token_type == TokenType::Attribution {
            self.eat(); // Consome o token de atribuição
            let right: Expr = self.parse_expr(); // Analisa a expressão do lado direito da atribuição
            left = Expr::AssignmentExpr(AssignmentExpr {
                kind: NodeType::AssignmentExpr,
                assigne: Box::new(left),
                value: Box::new(right),
            }); // Constrói uma expressão de atribuição
            self.expect(TokenType::Semicolon, "\";\" Expected."); // Verifica e consome o token ";"
        }

        left // Retorna a expressão analisada
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
            array.push(self.parse_expr());

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

            // Espera pelo token de dois pontos ":"
            self.expect(TokenType::Colon, "\":\" Expected.");
            // Parseia o valor associado à chave
            let value: Box<Expr> = Box::new(self.parse_expr());

            // Adiciona a propriedade ao vetor de propriedades
            properties.push(Property {
                kind: NodeType::Property,
                key,
                value: Some(value),
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
        let member = self.parse_member_expr(); // Analisa a expressão de membro

        if self.at().token_type == TokenType::Colon {
            self.parse_call_expr(member) // Analisa a expressão de chamada
        } else {
            member // Retorna a expressão de membro se não for uma chamada de função
        }
    }

    // Método para analisar uma expressão de chamada
    fn parse_call_expr(&mut self, caller: Expr) -> Expr {
        self.expect(TokenType::Colon, "\":\" Expected."); // Espera o token ':'
        self.expect(TokenType::OParen, "\"(\" Expected."); // Espera o token '('
        let args = self.parse_args(); // Analisa os argumentos da chamada de função
        Expr::CallExpr(CallExpr {
            kind: NodeType::CallExpr,
            caller: Box::new(caller),
            args,
        })
    }

    // Método para analisar os argumentos de uma chamada de função
    fn parse_args(&mut self) -> Vec<Box<Expr>> {
        let mut args = Vec::new();
        if self.at().token_type != TokenType::CParen {
            args = self.parse_arguments_list(); // Analisa a lista de argumentos
        }
        self.expect(TokenType::CParen, "\")\" Expected."); // Espera o token ')'
        args
    }

    // Método para analisar uma lista de argumentos
    fn parse_arguments_list(&mut self) -> Vec<Box<Expr>> {
        let mut args = vec![Box::new(self.parse_assignment_expr())]; // Analisa o primeiro argumento

        while self.not_eof() && self.at().token_type == TokenType::Comma {
            self.eat(); // Consome a vírgula
            args.push(Box::new(self.parse_assignment_expr())); // Analisa o próximo argumento
        }

        args
    }

    // Método para analisar uma expressão de membro
    fn parse_member_expr(&mut self) -> Expr {
        let mut object = self.parse_primary_expr(); // Analisa a expressão primária inicialmente

        while self.at().token_type == TokenType::Dot || self.at().token_type == TokenType::Colon {
            match self.at().token_type {
                TokenType::Dot => {
                    self.eat(); // Consome o token '.'
                    let property = self.expect(TokenType::Identifier, "Identifier Expected.");
                    object = Expr::MemberExpr(MemberExpr {
                        kind: NodeType::MemberExpr,
                        object: Box::new(object),
                        property: Box::new(Expr::Identifier(Identifier {
                            kind: NodeType::Identifier,
                            symbol: property.value,
                        })),
                    });
                }
                TokenType::Colon => {
                    object = self.parse_call_expr(object); // Analisa a expressão de chamada associada
                }
                _ => break,
            }
        }

        object
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
            TokenType::OParen => {
                // Se for um parêntese aberto, consome o token e analisa a expressão dentro dos parênteses
                self.eat();
                let expr = self.parse_expr(); // Analisa a expressão dentro dos parênteses
                self.expect(TokenType::CParen, "\")\" Expected."); // Espera um parêntese fechado
                expr // Retorna a expressão analisada
            }

            // Se conter "{", faz o parsing de objetos
            TokenType::OBrace => self.parse_object_expr(),
            // Se conter "[", faz o parsing de arrays
            TokenType::OBracket => self.parse_array_expr(),

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
