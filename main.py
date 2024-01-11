import argparse
from src.Lexer import Lexer
from src.Parser import Parser
from src.Checker import Checker
from src.Builder import Builder
from src.Compiler import Compiler

lexer = Lexer()
parser = Parser()
checker = Checker()
builder = Builder()
compiler = Compiler()

argparser = argparse.ArgumentParser(description="Poseidon - compiler for the Prox language.")
argparser.add_argument("-i", "--input", help="Input file to compile")
argparser.add_argument("-o", "--output", help="Output path")
args = argparser.parse_args()

if __name__ == "__main__":
  if not args.input:
    print("ERROR: no input files")
    exit(1)

  with open(args.input, "r") as f:
    code = f.read()
    
  tokens = lexer.tokenize(code)
  parsing = parser.produceAST(tokens)
  checker.eval(parsing)

  if not args.output:
    args.output = args.input

  builder.build(parsing, args.output)
  # compiler.compile(args.output)
