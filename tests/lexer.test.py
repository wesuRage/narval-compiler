from src.Lexer import Lexer

expectedtypes = [
  'OPAREN',
  'CPAREN',
  'OBRACE',
  'CBRACE',
  'OBRACKET',
  'CBRACKET',
  'IDENTIFIER',
  'NUMBER',
  'NUMBER',
  'DOT',
  'COMMA',
  'SEMICOLON',
  'COLON',
  'PLUS',
  'MINUS',
  "MUL",
  "DIV",
  "BACKSLASH",
  "ARROW",
  "GT",
  "LT",
  "MOD",
  "BAR",
  "EXCLAMATION",
  "AND",
  "INTERROGATION",
  "BYTE",
  "WORD",
  "DWORD",
  "QWORD",
  "RESB",
  "RESQ",
  "RESD",
  "RESW",
  "LABEL",
  "EOF"
]
def test():
  lexer = Lexer()
  code = "( ) { } [ ] lex 35 4.6 . , ; : + - * / \\ => > < % | ! & ? byte word dword qword resb resq resd resw label"

  info = {'sucess': False, 'data': None}

  try:
    tokens = lexer.tokenize(code)
    tokentypes = list(map(lambda tok: tok["type"], tokens))
    if tokentypes == expectedtypes:
      info['sucess'] = True
    info['data'] = tokentypes
  except Exception as e:
    info['data'] = e

  return info