use std::collections::HashMap;

// Definição dos tipos de token suportados
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    And,
    Array,
    As,
    Asm,
    Attribution,
    Auto,
    BitwiseAnd,
    BitwiseAndEq,
    BitwiseNot,
    BitwiseOr,
    BitwiseOrEq,
    BitwiseXor,
    BitwiseXorEq,
    Bool,
    Break,
    Byte,
    CBrace,
    CBracket,
    CComment,
    Colon,
    Comma,
    Comment,
    Continue,
    CParen,
    Decimal,
    Decrement,
    Div,
    DivEq,
    Dot,
    Dword,
    Elif,
    Else,
    Enum,
    Eof,
    Equals,
    Export,
    False,
    For,
    GreaterThan,
    GreaterThanOrEqual,
    Identifier,
    If,
    Import,
    Increment,
    Integer,
    IntegerDiv,
    IntegerDivEq,
    Label,
    LessThan,
    LessThanOrEqual,
    Loop,
    Minus,
    MinusEq,
    Match,
    Mod,
    ModEq,
    Mov,
    Mul,
    MulEq,
    Not,
    NotEquals,
    Null,
    Number,
    OBrace,
    OBracket,
    OComment,
    Object,
    OParen,
    Or,
    Plus,
    PlusEq,
    Power,
    PowerEq,
    Private,
    Public,
    QuestionMark,
    Qword,
    Range,
    RangeInclusive,
    Resb,
    Resd,
    Resq,
    Resw,
    Return,
    Semicolon,
    ShiftLeft,
    ShiftLeftEq,
    ShiftRight,
    ShiftRightEq,
    String,
    Text,
    True,
    Unit,
    Var,
    Void,
    While,
    Word,
    _Invalid,
}

// Estrutura que contém as definições de tokens
pub struct TokenDefinitions {
    pub literals: HashMap<&'static str, TokenType>, // Mapeamento de literais para tipos de token
    pub keywords: HashMap<&'static str, TokenType>, // Mapeamento de palavras-chave para tipos de token
}

impl TokenDefinitions {
    // Construtor da estrutura TokenDefinitions
    pub fn new() -> Self {
        let mut literals: HashMap<&str, TokenType> = HashMap::new();
        let mut keywords: HashMap<&str, TokenType> = HashMap::new();

        // Preenchimento dos mapas com literais e palavras-chave associados aos tipos de token correspondentes
        // Delimitadores
        literals.insert("=", TokenType::Attribution);
        literals.insert("&", TokenType::BitwiseAnd);
        literals.insert("&=", TokenType::BitwiseAndEq);
        literals.insert("|", TokenType::BitwiseOr);
        literals.insert("|=", TokenType::BitwiseOrEq);
        literals.insert("~", TokenType::BitwiseNot);
        literals.insert("^", TokenType::BitwiseXor);
        literals.insert("^=", TokenType::BitwiseXorEq);
        literals.insert("//", TokenType::Comment);
        literals.insert(",", TokenType::Comma);
        literals.insert(":", TokenType::Colon);
        literals.insert("*/", TokenType::CComment);
        literals.insert("}", TokenType::CBrace);
        literals.insert("]", TokenType::CBracket);
        literals.insert(")", TokenType::CParen);
        literals.insert("/", TokenType::Div);
        literals.insert("/=", TokenType::DivEq);
        literals.insert(".", TokenType::Dot);
        literals.insert("==", TokenType::Equals);
        literals.insert("not", TokenType::Not);
        literals.insert(">", TokenType::GreaterThan);
        literals.insert(">=", TokenType::GreaterThanOrEqual);
        literals.insert("\\", TokenType::IntegerDiv);
        literals.insert("\\=", TokenType::IntegerDivEq);
        literals.insert("++", TokenType::Increment);
        literals.insert("--", TokenType::Decrement);
        literals.insert("..", TokenType::Range);
        literals.insert("..=", TokenType::RangeInclusive);
        literals.insert("{", TokenType::OBrace);
        literals.insert("[", TokenType::OBracket);
        literals.insert("/*", TokenType::OComment);
        literals.insert("(", TokenType::OParen);
        literals.insert("or", TokenType::Or);
        literals.insert("+", TokenType::Plus);
        literals.insert("+=", TokenType::PlusEq);
        literals.insert("-", TokenType::Minus);
        literals.insert("-=", TokenType::MinusEq);
        literals.insert("%", TokenType::Mod);
        literals.insert("%=", TokenType::ModEq);
        literals.insert("*", TokenType::Mul);
        literals.insert("*=", TokenType::MulEq);
        literals.insert("!=", TokenType::NotEquals);
        literals.insert("?", TokenType::QuestionMark);
        literals.insert(";", TokenType::Semicolon);
        literals.insert("<<", TokenType::ShiftLeft);
        literals.insert("<<=", TokenType::ShiftLeftEq);
        literals.insert(">>", TokenType::ShiftRight);
        literals.insert(">>=", TokenType::ShiftRightEq);
        literals.insert("<", TokenType::LessThan);
        literals.insert("<=", TokenType::LessThanOrEqual);
        literals.insert("**", TokenType::Power);
        literals.insert("**=", TokenType::PowerEq);
        literals.insert("and", TokenType::And);

        keywords.insert("Array", TokenType::Array);
        keywords.insert("as", TokenType::As);
        keywords.insert("asm", TokenType::Asm);
        keywords.insert("auto", TokenType::Auto);
        keywords.insert("boolean", TokenType::Bool);
        keywords.insert("break", TokenType::Break);
        keywords.insert("byte", TokenType::Byte);
        keywords.insert("continue", TokenType::Continue);
        keywords.insert("decimal", TokenType::Decimal);
        keywords.insert("dword", TokenType::Dword);
        keywords.insert("elif", TokenType::Elif);
        keywords.insert("else", TokenType::Else);
        keywords.insert("enum", TokenType::Enum);
        keywords.insert("export", TokenType::Export);
        keywords.insert("false", TokenType::False);
        keywords.insert("for", TokenType::For);
        keywords.insert("if", TokenType::If);
        keywords.insert("import", TokenType::Import);
        keywords.insert("integer", TokenType::Integer);
        keywords.insert("label", TokenType::Label);
        keywords.insert("loop", TokenType::Loop);
        keywords.insert("match", TokenType::Match);
        keywords.insert("mov", TokenType::Mov);
        keywords.insert("null", TokenType::Null);
        keywords.insert("Object", TokenType::Object);
        keywords.insert("private", TokenType::Private);
        keywords.insert("public", TokenType::Public);
        keywords.insert("qword", TokenType::Qword);
        keywords.insert("resb", TokenType::Resb);
        keywords.insert("resd", TokenType::Resd);
        keywords.insert("resq", TokenType::Resq);
        keywords.insert("resw", TokenType::Resw);
        keywords.insert("return", TokenType::Return);
        keywords.insert("text", TokenType::Text);
        keywords.insert("true", TokenType::True);
        keywords.insert("unit", TokenType::Unit);
        keywords.insert("var", TokenType::Var);
        keywords.insert("void", TokenType::Void);
        keywords.insert("while", TokenType::While);
        keywords.insert("word", TokenType::Word);

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
    filename: &'a String,                // Nome do arquivo fonte
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
    pub fn new(filename: &String) -> Lexer {
        let token_definitions: TokenDefinitions = TokenDefinitions::new();
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

    // Escaneia o atual token no código fonte
    fn scan_token(&mut self) {
        // Obtém o atual caractere no código fonte
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
            // Se o atual caractere estiver disponível, verifica se há um token composto
            let two_char_literal: String = format!("{}{}", char, pick_char);
            let three_char_literal: String = if let Some(next_pick_char) = self.pick_next() {
                format!("{}{}{}", char, pick_char, next_pick_char)
            } else {
                two_char_literal.clone()
            };

            // Verifica se há um token composto de três caracteres
            if let Some(token_type) = self
                .token_definitions
                .literals
                .get(three_char_literal.as_str())
            {
                self.add_token(token_type.clone(), three_char_literal); // Adiciona o token composto de três caracteres
                self.eat_char(); // Avança para o segundo caractere
                self.eat_char(); // Avança para o terceiro caractere
            }
            // Verifica se há um token composto de dois caracteres
            else if let Some(token_type) = self
                .token_definitions
                .literals
                .get(two_char_literal.as_str())
            {
                if two_char_literal == "//" {
                    // Comentário de linha
                    self.eat_char(); // Avança para o segundo caractere
                    self.skip_comment(false); // Pula o comentário de linha
                } else if two_char_literal == "/*" {
                    // Comentário de bloco
                    self.eat_char(); // Avança para o segundo caractere
                    self.skip_comment(true); // Pula o bloco de comentário
                } else {
                    self.add_token(token_type.clone(), two_char_literal); // Adiciona o token composto de dois caracteres
                    self.eat_char(); // Avança para o segundo caractere
                }
            }
            // Verifica se há um token de um único caractere
            else if let Some(token_type) = self
                .token_definitions
                .literals
                .get(char.to_string().as_str())
            {
                self.add_token(token_type.clone(), char.to_string()); // Adiciona o token de um único caractere
            } else {
                // Se não for um literal, verifica se é um número, identificador ou um caractere desconhecido
                if char.is_digit(10) {
                    self.scan_number(char); // Escaneia número
                } else if char.is_alphabetic() || char == '_' {
                    self.scan_identifier(); // Escaneia identificador
                } else {
                    self.error(format!("Unknown char: {}", char)); // Gera um erro para caractere desconhecido
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

    // Consome o atual caractere no código fonte e atualiza os índices e colunas correspondentes
    fn eat_char(&mut self) -> char {
        let char = self.pick_char().unwrap_or('\0');
        self.endindex += 1;
        self.endcol += 1;
        char
    }

    // Obtém o atual caractere no código fonte
    fn pick_char(&self) -> Option<char> {
        self.code
            .as_ref()
            .and_then(|code: &String| code.chars().nth(self.endindex))
    }

    // Obtém o atual caractere no código fonte
    fn pick_next(&self) -> Option<char> {
        self.code
            .as_ref()
            .and_then(|code: &String| code.chars().nth(self.endindex + 1))
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
                while let (Some(c1), Some(c2)) = (self.pick_char(), self.pick_next()) {
                    if !(c1 == '*' && c2 == '/') {
                        self.eat_char();
                    }
                    self.eat_char();
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

    fn scan_number(&mut self, firstchar: char) {
        let mut num_str: String = String::new();
        num_str.push(firstchar);
        let mut has_dot: bool = false;
        let mut has_exponent: bool = false;
        let mut base: u32 = 10; // Base padrão é decimal

        // Checar se o número tem um prefixo que indica uma base diferente (binário, octal, hexadecimal)
        if firstchar == '0' {
            if let Some(next_char) = self.pick_char() {
                match next_char {
                    'x' | 'X' => {
                        base = 16;
                        num_str.push(self.eat_char());
                    }
                    'o' | 'O' => {
                        base = 8;
                        num_str.push(self.eat_char());
                    }
                    'b' | 'B' => {
                        base = 2;
                        num_str.push(self.eat_char());
                    }
                    _ => {}
                }
            }
        }

        // Escanear o restante do número
        while let Some(char) = self.pick_char() {
            match char {
                '0'..='9' => {
                    num_str.push(self.eat_char());
                }
                'a'..='f' | 'A'..='F' if base == 16 => {
                    num_str.push(self.eat_char());
                }
                '.' if base == 10 => {
                    if has_dot || has_exponent {
                        break;
                    }
                    if self.pick_next() == Some('.') {
                        break;
                    }
                    has_dot = true;
                    num_str.push(self.eat_char());
                }
                'e' | 'E' if base == 10 => {
                    if has_exponent {
                        break;
                    }
                    has_exponent = true;
                    num_str.push(self.eat_char());
                    if let Some(next_char) = self.pick_char() {
                        if next_char == '+' || next_char == '-' {
                            num_str.push(self.eat_char());
                        }
                    }
                }
                _ => break,
            }
        }

        // Validação adicional para binários, octais e hexadecimais
        match base {
            2 => {
                if num_str.chars().skip(2).any(|c| !matches!(c, '0' | '1')) {
                    self.error(format!("Invalid binary number: {}", num_str));
                }
            }
            8 => {
                if num_str.chars().skip(2).any(|c| !matches!(c, '0'..='7')) {
                    self.error(format!("Invalid octal number: {}", num_str));
                }
            }
            16 => {
                if num_str
                    .chars()
                    .skip(2)
                    .any(|c| !matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'))
                {
                    self.error(format!("Invalid hexadecimal number: {}", num_str));
                }
            }
            _ => {}
        }

        // Adicionar o token do número identificado
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
        let text: String = self.get_current_chunk(None, None);
        let token_type: TokenType = self
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
