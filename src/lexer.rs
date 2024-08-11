use std::collections::HashMap;

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
    Class,
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
    In,
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
    Var,
    Void,
    While,
    Word,
    _Invalid,
}

pub struct TokenDefinitions {
    pub literals: HashMap<&'static str, TokenType>,
    pub keywords: HashMap<&'static str, TokenType>,
}

impl TokenDefinitions {
    pub fn new() -> Self {
        let mut literals: HashMap<&str, TokenType> = HashMap::new();
        let mut keywords: HashMap<&str, TokenType> = HashMap::new();

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
        keywords.insert("in", TokenType::In);
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
        keywords.insert("class", TokenType::Class);
        keywords.insert("var", TokenType::Var);
        keywords.insert("void", TokenType::Void);
        keywords.insert("while", TokenType::While);
        keywords.insert("word", TokenType::Word);

        TokenDefinitions { literals, keywords }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
    pub lineno: usize,
    pub column: (usize, usize),
    pub position: (usize, usize),
    pub filename: String,
    pub message: Option<String>,
}

pub struct Lexer<'a> {
    token_definitions: TokenDefinitions,
    filename: &'a String,
    code: Option<String>,
    ignoreable_chars: Vec<char>,
    index: usize,
    endindex: usize,
    col: usize,
    endcol: usize,
    lineno: usize,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
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

    pub fn tokenize(&mut self, code: &str) -> Vec<Token> {
        self.code = Some(code.to_string());
        self.tokens.clear();

        while !self.is_eof() {
            self.index = self.endindex;
            self.col = self.endcol;
            self.scan_token();
        }

        self.add_token(TokenType::Eof, "EOF".to_string());

        self.tokens.clone()
    }

    fn is_eof(&self) -> bool {
        self.code
            .as_ref()
            .map_or(true, |code| self.endindex + 1 >= code.len())
    }

    fn scan_token(&mut self) {
        let char = self.eat_char();

        if self.ignoreable_chars.contains(&char) {
            return;
        } else if char == '\n' {
            self.lineno += 1;
            self.col = 1;
            self.endcol = 1;
        } else if char == '"' {
            self.scan_double_quote_string();
        } else if char == '\'' {
            self.scan_single_quote_string();
        } else if let Some(pick_char) = self.pick_char() {
            let two_char_literal: String = format!("{}{}", char, pick_char);
            let three_char_literal: String = if let Some(next_pick_char) = self.pick_next() {
                format!("{}{}{}", char, pick_char, next_pick_char)
            } else {
                two_char_literal.clone()
            };

            if let Some(token_type) = self
                .token_definitions
                .literals
                .get(three_char_literal.as_str())
            {
                self.add_token(token_type.clone(), three_char_literal);
                self.eat_char();
                self.eat_char();
            } else if let Some(token_type) = self
                .token_definitions
                .literals
                .get(two_char_literal.as_str())
            {
                if two_char_literal == "//" {
                    self.eat_char();
                    self.skip_comment(false);
                } else if two_char_literal == "/*" {
                    self.eat_char();
                    self.skip_comment(true);
                } else {
                    self.add_token(token_type.clone(), two_char_literal);
                    self.eat_char();
                }
            } else if let Some(token_type) = self
                .token_definitions
                .literals
                .get(char.to_string().as_str())
            {
                self.add_token(token_type.clone(), char.to_string());
            } else {
                if char.is_digit(10) {
                    self.scan_number(char);
                } else if char.is_alphabetic() || char == '_' {
                    self.scan_identifier();
                } else {
                    self.error(format!("Unknown char: {}", char));
                }
            }
        }
    }

    fn error(&mut self, message: String) {
        let chunk = self.get_current_chunk(None, None);
        self.add_token(TokenType::_Invalid, chunk);

        if let Some(last_token) = self.tokens.last_mut() {
            last_token.message = Some(message);
        }
    }

    fn get_current_chunk(&self, start: Option<usize>, end: Option<usize>) -> String {
        let start = start.unwrap_or(self.index);
        let end = end.unwrap_or(self.endindex);
        self.code
            .as_ref()
            .map_or(String::new(), |code| code[start..end].to_string())
    }

    fn eat_char(&mut self) -> char {
        let char = self.pick_char().unwrap_or('\0');
        self.endindex += 1;
        self.endcol += 1;
        char
    }

    fn pick_char(&self) -> Option<char> {
        self.code
            .as_ref()
            .and_then(|code: &String| code.chars().nth(self.endindex))
    }

    fn pick_next(&self) -> Option<char> {
        self.code
            .as_ref()
            .and_then(|code: &String| code.chars().nth(self.endindex + 1))
    }

    fn scan_double_quote_string(&mut self) {
        while self.pick_char().unwrap_or('\0') != '"' {
            self.eat_char();
        }

        self.eat_char();

        let text = self.get_current_chunk(Some(self.index + 1), Some(self.endindex - 1));
        self.add_token(TokenType::String, text);
    }

    fn scan_single_quote_string(&mut self) {
        while self.pick_char().unwrap_or('\0') != '\'' {
            self.eat_char();
        }

        self.eat_char();

        let text = self.get_current_chunk(Some(self.index + 1), Some(self.endindex - 1));
        self.add_token(TokenType::String, text);
    }

    fn skip_comment(&mut self, long: bool) {
        if let Some(ref code) = self.code {
            if long {
                while let (Some(c1), Some(c2)) = (self.pick_char(), self.pick_next()) {
                    if !(c1 == '*' && c2 == '/') {
                        self.eat_char();
                    }
                    self.eat_char();
                }
            } else {
                if let Some(pos) = code[self.endindex..].find('\n') {
                    self.endindex += pos;
                }
            }

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
        let mut base: u32 = 10;

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

        self.add_token(TokenType::Number, num_str);
    }

    fn scan_identifier(&mut self) {
        while let Some(char) = self.pick_char() {
            if !char.is_alphanumeric() && char != '_' {
                break;
            }
            self.eat_char();
        }

        let text: String = self.get_current_chunk(None, None);
        let token_type: TokenType = self
            .token_definitions
            .keywords
            .get(text.as_str())
            .cloned()
            .unwrap_or(TokenType::Identifier);

        self.add_token(token_type, text);
    }

    fn add_token(&mut self, token_type: TokenType, value: String) {
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
