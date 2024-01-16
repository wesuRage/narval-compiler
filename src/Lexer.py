import re

class Lexer:
  def __init__(self):
    self.code = None
    self.token_definitions = {
      "literals": {
        "(": "OPAREN",
        ")": "CPAREN",
        "{": "OBRACE",
        "}": "CBRACE",
        "[": "OBRACKET",
        "]": "CBRACKET",
        ",": "COMMA",
        ".": "DOT",
        ":": "COLON",
        ";": "SEMICOLON",
        "=": "EQUALS",
        "=>": "ARROW",
        ">": "GT",
        "<": "LT",
        "-": "MINUS",
        "+": "PLUS",
        "/": "DIV",
        "*": "MUL",
        "%": "MOD",
        "\\": "BACKSLASH",
        "|": "BAR",
        "!": "EXCLAMATION",
        "?": "INTERROGATION",
        "&": "AND"
      },
      "keywords": {
        "resb": "RESB",
        "resw": "RESW",
        "resd": "RESD",
        "resq": "RESQ",
        "byte": "BYTE"
      }
    }

    self.ignoreableChars = [' ', '\t'] 
    #rastrear o inicio e fim de cada token
    self.index = 0
    self.endindex = 0
    self.lineno = 1
    self.tokens = None

  def tokenize(self, code):
    self.code = code
    self.tokens = []

    while not self.isEof():
      self.index = self.endindex
      self.scanToken()

    self.addToken("EOF", "EOF")

    return self.tokens

  def isEof(self):
    return self.endindex >= len(self.code)
  
  def scanToken(self):
    char = self.eatChar()
      #lida com literais de dois caracteres
    if char in self.ignoreableChars:
      return
    elif char == '\n':
      self.lineno += 1
    elif char == '"':
      self.scanDoubleQuoteString()
    elif char == "'":
      self.scanSingleQuoteString()
    elif char + self.peekChar() in self.token_definitions["literals"]:
      self.addToken(self.token_definitions["literals"][char + self.eatChar()], char)
    elif char in self.token_definitions["literals"]: 
      self.addToken(self.token_definitions["literals"][char], char)
    else:
      if re.match('\\d', char):
        self.scanNumber()
      elif re.match('[a-zA-Z_]', char):
        self.scanIdentifier()
      else:
        raise SyntaxError(f"Unknown char: {char}")

  def eatChar(self):
    char = self.peekChar()
    self.endindex += 1
    return char
  
  def peekChar(self):
    return self.code[self.endindex] if not self.isEof() else '\0'
  
  def scanDoubleQuoteString(self):
    while self.peekChar() != '"':
      self.eatChar()
    self.eatChar()
    text = self.getCurrentChunk(self.index + 1)
    self.addToken("STRING", text)

  def getCurrentChunk(self, start=None, end=None):
    start = start if start is not None else self.index 
    end = end if end is not None else self.endindex
    return self.code[self.index:self.endindex]
  
  def scanSingleQuoteString(self):
    while self.peekChar() != "'":
      self.eatChar()
    self.eatChar()

    text = self.getCurrentChunk(self.index + 2, self.endindex)

    self.addToken("STRING", text)
  
  def scanNumber(self):
    while re.match('\\d', self.peekChar()):
      self.eatChar()

    if self.peekChar() == ".":
      self.eatChar()
      if not re.match('\\d', self.peekChar()):
        raise SyntaxError("Malformed float literal")
      while re.match('\\d', self.peekChar()):
        self.eatChar()
    
    num = float(self.getCurrentChunk())

    self.addToken("NUMBER", num)

  

  def addToken(self, type, lexeme=None):
    self.tokens.append({"value": lexeme, "type": type})

  def scanIdentifier(self):
    while re.match("\\w", self.peekChar()):
      self.eatChar()

    text = self.getCurrentChunk()

    toktype = self.token_definitions["keywords"].get(text)

    if not toktype:
      toktype = "IDENTIFIER"

    self.addToken(toktype, text)
