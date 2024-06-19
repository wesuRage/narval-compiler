use std::collections::HashMap;

// Definição dos tipos de token suportados
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Tokens para delimitadores
    OParen,
    CParen,
    OBrace,
    CBrace,
    OBracket,
    CBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    // Tokens para operadores
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Minus,
    // Tokens para comentários
    Comment,
    OComment,
    CComment,
    // Outros tokens comuns
    As,
    Unit,
    Plus,
    Div,
    Mul,
    Power,
    Mod,
    Backslash,
    Pipe,
    Exclamation,
    QuestionMark,
    And,
    // Tokens para tipos de dados e palavras-chave
    Bool,
    Integer,
    Decimal,
    Text,
    Object,
    UndefinedType,
    Undefined,
    Null,
    Auto,
    // Tokens para diretivas
    Resb,
    Resw,
    Resd,
    Resq,
    Byte,
    Word,
    Dword,
    Qword,
    Label,
    // Tokens para literais e identificadores
    Number,
    Identifier,
    String,
    // Tokens para construções de controle
    Arrow,
    Or,
    Separator,
    Attribution,
    Return,
    Import,
    Export,
    If,
    Else,
    Switch,
    Break,
    True,
    False,
    Continue,
    // Tokens para modificadores de acesso
    Private,
    Public,
    // Token especial para marcação de final de arquivo
    Eof,
    // Token especial para representar um caractere inválido
    _Invalid,
}

impl TokenType {
    // Verifica se o tipo de token atual corresponde a um dos tipos fornecidos
    pub fn matches(&self, candidates: &[TokenType]) -> bool {
        candidates.contains(self)
    }
}

// Estrutura que contém as definições de tokens
pub struct TokenDefinitions {
    pub literals: HashMap<&'static str, TokenType>, // Mapeamento de literais para tipos de token
    pub keywords: HashMap<&'static str, TokenType>, // Mapeamento de palavras-chave para tipos de token
}

impl TokenDefinitions {
    // Construtor da estrutura TokenDefinitions
    pub fn new() -> Self {
        let mut literals = HashMap::new();
        let mut keywords = HashMap::new();

        // Preenchimento dos mapas com literais e palavras-chave associados aos tipos de token correspondentes
        // Delimitadores
        literals.insert("(", TokenType::OParen);
        literals.insert(")", TokenType::CParen);
        literals.insert("{", TokenType::OBrace);
        literals.insert("}", TokenType::CBrace);
        literals.insert("[", TokenType::OBracket);
        literals.insert("]", TokenType::CBracket);
        literals.insert(",", TokenType::Comma);
        literals.insert(".", TokenType::Dot);
        literals.insert(":", TokenType::Colon);
        literals.insert(";", TokenType::Semicolon);
        // Operadores
        literals.insert("==", TokenType::Equals);
        literals.insert("!=", TokenType::NotEquals);
        literals.insert(">", TokenType::GreaterThan);
        literals.insert(">=", TokenType::GreaterThanOrEqual);
        literals.insert("<", TokenType::LessThan);
        literals.insert("<=", TokenType::LessThanOrEqual);
        literals.insert("-", TokenType::Minus);
        literals.insert("//", TokenType::Comment);
        literals.insert("/*", TokenType::OComment);
        literals.insert("*/", TokenType::CComment);
        literals.insert("+", TokenType::Plus);
        literals.insert("/", TokenType::Div);
        literals.insert("*", TokenType::Mul);
        literals.insert("^", TokenType::Power);
        literals.insert("%", TokenType::Mod);
        literals.insert("\\", TokenType::Backslash);
        literals.insert("|", TokenType::Pipe);
        literals.insert("||", TokenType::Or);
        literals.insert("!", TokenType::Exclamation);
        literals.insert("?", TokenType::QuestionMark);
        literals.insert("&&", TokenType::And);
        literals.insert("::", TokenType::Separator);
        literals.insert("<<", TokenType::Attribution);
        literals.insert("=>", TokenType::Arrow);
        // Tipos de dados e palavras-chave
        keywords.insert("unit", TokenType::Unit);
        keywords.insert("as", TokenType::As);
        keywords.insert("private", TokenType::Private);
        keywords.insert("public", TokenType::Public);
        keywords.insert("undefined", TokenType::Undefined);
        keywords.insert("import", TokenType::Import);
        keywords.insert("export", TokenType::Export);
        keywords.insert("if", TokenType::If);
        keywords.insert("else", TokenType::Else);
        keywords.insert("switch", TokenType::Switch);
        keywords.insert("return", TokenType::Return);
        keywords.insert("break", TokenType::Break);
        keywords.insert("continue", TokenType::Continue);
        keywords.insert("true", TokenType::True);
        keywords.insert("false", TokenType::False);
        keywords.insert("Null", TokenType::Null);
        keywords.insert("Bool", TokenType::Bool);
        keywords.insert("Integer", TokenType::Integer);
        keywords.insert("Decimal", TokenType::Decimal);
        keywords.insert("Object", TokenType::Object);
        keywords.insert("auto", TokenType::Auto);
        keywords.insert("Text", TokenType::Text);
        keywords.insert("resb", TokenType::Resb);
        keywords.insert("resw", TokenType::Resw);
        keywords.insert("resd", TokenType::Resd);
        keywords.insert("resq", TokenType::Resq);
        keywords.insert("byte", TokenType::Byte);
        keywords.insert("word", TokenType::Word);
        keywords.insert("dword", TokenType::Dword);
        keywords.insert("qword", TokenType::Qword);
        keywords.insert("label", TokenType::Label);

        TokenDefinitions { literals, keywords }
    }
}

// Estrutura que representa um token individual
#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,            // Valor do token
    pub token_type: TokenType,    // Tipo do token
    pub lineno: usize,            // Número da linha no código fonte
    pub column: (usize, usize),   // Coluna de início e fim do token
    pub position: (usize, usize), // Posição de início e fim do token no código fonte
    pub filename: String,         // Nome do arquivo fonte
    pub message: Option<String>,  // Mensagem de erro associada ao token (se houver)
}

// Estrutura principal responsável pela análise léxica do código fonte
pub struct Lexer<'a> {
    token_definitions: TokenDefinitions, // Definições de tokens
    filename: &'a str,                   // Nome do arquivo fonte
    code: Option<String>,                // Código fonte a ser analisado
    ignoreable_chars: Vec<char>,         // Caracteres a serem ignorados durante a análise
    index: usize,                        // Índice atual no código fonte
    endindex: usize,                     // Índice final no código fonte
    col: usize,                          // Coluna atual no código fonte
    endcol: usize,                       // Coluna final no código fonte
    lineno: usize,                       // Número da linha atual no código fonte
    tokens: Vec<Token>,                  // Tokens encontrados durante a análise
}

impl<'a> Lexer<'a> {
    // Construtor da estrutura Lexer
    pub fn new(filename: &str) -> Lexer {
        let token_definitions = TokenDefinitions::new();
        Lexer {
            token_definitions,
            filename,
            code: None,
            ignoreable_chars: vec![' ', '\t'],
            index: 0,
            endindex: 0,
            col: 1,
            endcol: 1,
            lineno: 1,
            tokens: Vec::new(),
        }
    }

    // Método para tokenizar o código fonte
    pub fn tokenize(&mut self, code: &str) -> Vec<Token> {
        // Inicializa o código fonte e a lista de tokens
        self.code = Some(code.to_string());
        self.tokens.clear();

        // Loop principal para percorrer o código fonte e tokenizá-lo
        while !self.is_eof() {
            self.index = self.endindex;
            self.col = self.endcol;
            self.scan_token();
        }

        // Adiciona um token de final de arquivo à lista de tokens
        self.add_token(TokenType::Eof, "EOF".to_string());

        // Retorna a lista de tokens encontrados
        self.tokens.clone()
    }

    // Verifica se o final do arquivo foi alcançado
    fn is_eof(&self) -> bool {
        self.code
            .as_ref()
            .map_or(true, |code| self.endindex + 1 >= code.len())
    }

    // Escaneia o próximo token no código fonte
    fn scan_token(&mut self) {
        // Obtém o próximo caractere no código fonte
        let char = self.eat_char();
        // Verifica se o caractere deve ser ignorado
        if self.ignoreable_chars.contains(&char) {
            return; // Se sim, ignora o caractere e retorna
        } else if char == '\n' {
            // Se o caractere for uma quebra de linha, atualiza o número da linha e as colunas
            self.lineno += 1;
            self.col = 1;
            self.endcol = 1;
        } else if char == '"' {
            // Se o caractere for uma aspas dupla, escaneia uma cadeia de caracteres delimitada por aspas duplas
            self.scan_double_quote_string();
        } else if char == '\'' {
            // Se o caractere for uma aspas simples, escaneia uma cadeia de caracteres delimitada por aspas simples
            self.scan_single_quote_string();
        } else if let Some(pick_char) = self.pick_char() {
            // Se o próximo caractere estiver disponível, verifica se há um token composto
            let two_char_literal: String = format!("{}{}", char, pick_char);
            // Verifica se o token composto existe nas definições de literais
            if let Some(token_type) = self
                .token_definitions
                .literals
                .get(two_char_literal.as_str())
            {
                // Se o token composto for encontrado, processa-o
                if two_char_literal == "//" {
                    // Se o token composto for um comentário de linha, pula o comentário
                    self.eat_char();
                    self.skip_comment(false);
                } else if two_char_literal == "/*" {
                    // Se o token composto for o início de um comentário de bloco, pula o bloco de comentário
                    self.eat_char();
                    self.skip_comment(true);
                } else {
                    // Adiciona o token composto à lista de tokens e avança para o próximo caractere
                    self.add_token(token_type.clone(), two_char_literal);
                    self.eat_char();
                }
            } else if let Some(token_type) = self
                .token_definitions
                .literals
                .get(char.to_string().as_str())
            {
                // Se o token composto não for encontrado, verifica se o caractere simples é um literal
                self.add_token(token_type.clone(), char.to_string());
            } else {
                // Se o caractere não for um literal, verifica se é um número, um identificador ou um erro
                if char.is_digit(10) {
                    // Se o caractere for um dígito, escaneia um número
                    self.scan_number();
                } else if char.is_alphabetic() || char == '_' {
                    // Se o caractere for alfabético ou um sublinhado, escaneia um identificador
                    self.scan_identifier();
                } else {
                    // Se o caractere for desconhecido, gera um erro
                    self.error(format!("Unknown char: {}", char));
                }
            }
        }
    }

    // Gera um erro com uma mensagem fornecida
    fn error(&mut self, message: String) {
        // Obtém o chunk atual do código fonte e adiciona um token de caractere inválido à lista de tokens
        let chunk = self.get_current_chunk(None, None);
        self.add_token(TokenType::_Invalid, chunk);
        // Se houver um último token na lista de tokens, adiciona a mensagem de erro a ele
        if let Some(last_token) = self.tokens.last_mut() {
            last_token.message = Some(message);
        }
    }

    // Obtém o chunk atual do código fonte com base nos índices fornecidos
    fn get_current_chunk(&self, start: Option<usize>, end: Option<usize>) -> String {
        let start = start.unwrap_or(self.index);
        let end = end.unwrap_or(self.endindex);
        self.code
            .as_ref()
            .map_or(String::new(), |code| code[start..end].to_string())
    }

    // Consome o próximo caractere no código fonte e atualiza os índices e colunas correspondentes
    fn eat_char(&mut self) -> char {
        let char = self.pick_char().unwrap_or('\0');
        self.endindex += 1;
        self.endcol += 1;
        char
    }

    // Obtém o próximo caractere no código fonte
    fn pick_char(&self) -> Option<char> {
        self.code
            .as_ref()
            .and_then(|code| code.chars().nth(self.endindex))
    }

    // Escaneia uma cadeia de caracteres delimitada por aspas duplas no código fonte
    fn scan_double_quote_string(&mut self) {
        // Loop para escanear a cadeia de caracteres até encontrar a próxima aspas dupla
        while self.pick_char().unwrap_or('\0') != '"' {
            self.eat_char();
        }
        // Consome a última aspas dupla
        self.eat_char();

        // Obtém o texto da cadeia de caracteres e adiciona um token correspondente à lista de tokens
        let text = self.get_current_chunk(Some(self.index + 1), Some(self.endindex - 1));
        self.add_token(TokenType::String, text);
    }

    // Escaneia uma cadeia de caracteres delimitada por aspas simples no código fonte
    fn scan_single_quote_string(&mut self) {
        // Loop para escanear a cadeia de caracteres até encontrar a próxima aspas simples
        while self.pick_char().unwrap_or('\0') != '\'' {
            self.eat_char();
        }
        // Consome a última aspas simples
        self.eat_char();

        // Obtém o texto da cadeia de caracteres e adiciona um token correspondente à lista de tokens
        let text = self.get_current_chunk(Some(self.index + 1), Some(self.endindex - 1));
        self.add_token(TokenType::String, text);
    }

    // Pula um comentário (de linha ou bloco) no código fonte
    fn skip_comment(&mut self, long: bool) {
        if let Some(ref code) = self.code {
            if long {
                // Pula até encontrar o final do comentário de bloco
                let end_comment: &str = "*/";
                if let Some(pos) = code[self.endindex..].find(end_comment) {
                    self.endindex += pos + end_comment.len();
                }
            } else {
                // Pula até encontrar uma quebra de linha
                if let Some(pos) = code[self.endindex..].find('\n') {
                    self.endindex += pos;
                }
            }
            // Atualiza o número da linha e as colunas
            self.lineno += 1;
            self.col = 1;
            self.endcol = 1;
        }
    }

    // Escaneia um número no código fonte
    fn scan_number(&mut self) {
        // Loop para escanear os dígitos do número
        while let Some(char) = self.pick_char() {
            if !char.is_digit(10) {
                break;
            }
            self.eat_char();
        }
        // Verifica se há um ponto decimal após os dígitos (para números decimais)
        if self.pick_char() == Some('.') {
            self.eat_char();
            // Loop para escanear os dígitos após o ponto decimal
            while let Some(char) = self.pick_char() {
                if !char.is_digit(10) {
                    break;
                }
                self.eat_char();
            }
        }
        // Obtém a representação em string do número e adiciona um token correspondente à lista de tokens
        let num_str = self.get_current_chunk(None, None);
        self.add_token(TokenType::Number, num_str);
    }

    // Escaneia um identificador no código fonte
    fn scan_identifier(&mut self) {
        // Loop para escanear os caracteres alfanuméricos e o sublinhado
        while let Some(char) = self.pick_char() {
            if !char.is_alphanumeric() && char != '_' {
                break;
            }
            self.eat_char();
        }
        // Obtém o identificador e verifica se é uma palavra-chave
        let text = self.get_current_chunk(None, None);
        let token_type = self
            .token_definitions
            .keywords
            .get(text.as_str())
            .cloned()
            .unwrap_or(TokenType::Identifier);
        // Adiciona um token correspondente à lista de tokens
        self.add_token(token_type, text);
    }

    // Adiciona um token à lista de tokens
    fn add_token(&mut self, token_type: TokenType, value: String) {
        // Cria um novo token com as informações fornecidas e o adiciona à lista de tokens
        let token = Token {
            value,
            token_type,
            lineno: self.lineno,
            column: (self.col, self.endcol),
            position: (self.index, self.endindex),
            filename: self.filename.to_string(),
            message: None,
        };
        self.tokens.push(token);
    }
}
