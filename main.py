#!/usr/bin/env python3
import argparse
import json
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

argparser = argparse.ArgumentParser(description="Compiler for the Narval language.")
argparser.add_argument("-i", "--input", help="Input file to compile")
argparser.add_argument("-o", "--output", help="Output path")
args = argparser.parse_args()

if __name__ == "__main__":
  if not args.input:
    print("ERROR: no input files")
    exit(1)

  with open(args.input, "r") as f:
  # with open("main.nv", "r") as f:
    code = f.read()
    
  tokens = lexer.tokenize(code)
  parsing = parser.produceAST(tokens)

  # print(json.dumps(parsing, indent=2))
  checker.eval(parsing)

  if not args.output:
    args.output = args.input

  builder.build(parsing, args.output)
  # builder.build(parsing, "main.nv")
  compiler.compile(args.output)
