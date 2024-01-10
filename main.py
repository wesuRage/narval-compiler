import json
from src.Lexer import Lexer
from src.Parser import Parser
from src.Checker import Checker

lex = Lexer()
yacc = Parser()
checker = Checker()

with open("main.px", "r") as f:
  code = "".join(f.readlines())
  
tokens = lex.Tokenize(code)
parsing = yacc.produceAST(tokens)
check = checker.eval(parsing)
print(json.dumps(check, indent=2))
