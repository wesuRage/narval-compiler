class Parser: 
  def __init__(self):
    self.tokens = []

  def not_eof(self):
    return self.tokens[0]["type"] != "EOF" 
  
  def at(self):
    return self.tokens[0]
  
  def eat(self):
    return self.tokens.pop(0)
  
  def expect(self, tokenType, error):
    prev = self.eat()

    if prev is None or prev["type"] != tokenType:
      raise SyntaxError(error)
    
    return prev

  def produceAST(self, tokens):
    self.tokens = tokens
    program = {
      "NodeType": "Program",
      "body": []
    }
    while self.not_eof():
      program["body"].append(self.parse_stmt())

    return program
  
  def parse_stmt(self):
    match self.at()["type"]:
      case "BYTE" | "RESB":
        return self.parse_var_declaration()

      case _:
        return self.parse_expr()
  
  def parse_var_declaration(self):
    isBytes = self.eat()["type"] == "BYTE"
    Identifier = self.expect("IDENTIFIER", "Identifier expected.")
    
    if isBytes:
      self.expect("EQUALS", "Expected equals in variable declaration.")
      
      declaration = {
        "NodeType": "VarDeclaration",
        "Identifier": Identifier,
        "value": self.parse_expr(),
        "type": "constant" if isBytes else "reserved",
        "directive": "db" if isBytes else "resb"
      }
      self.expect("SEMICOLON", "Expected ';' at the end of statement.")
      return declaration


    self.expect("OBRACKET", "Expected '[' for reserved length.")
    
    length = self.parse_expr()

    self.expect("CBRACKET", "Expected ']' for reserved length.")
    if self.at()["type"] == "SEMICOLON":
      self.eat()
      declaration = {
        "NodeType": "VarDeclaration",
        "Identifier": Identifier,
        "length": length["value"],
        "type": "reserved",
        "directive": "resb"
      }
      return declaration
 
    else:  
      self.expect("EQUALS", "Expected equals in variable declaration.")
      value = self.parse_expr()
      declaration = {
        "NodeType": "VarDeclaration",
        "Identifier": Identifier,
        "length": length,
        "value": value,
        "type": "reserved",
        "directive": "resb"

      }
      self.expect("SEMICOLON", "Expected ';' at the end of statement.")

      return declaration

  def parse_expr(self):
    return self.parse_assignment_expr()

  def parse_assignment_expr(self):
    left = self.parse_additive_expr()

    if self.at()["type"] == "EQUALS":
      self.eat()
      value = self.parse_assignment_expr()
      self.expect("SEMICOLON", "Expected ';' at the end of statement.")
      return {"NodeType": "AssignmentExpr", "assigne": left, "value": value}
    
    return left
  
  def parse_additive_expr(self):
    left = self.parse_multiplicative_expr()

    while self.at()["value"] == "+" or self.at()["value"] == "-":
      operator = self.eat()["value"]
      right = self.parse_multiplicative_expr()
      left = {
        "NodeType": "BinaryExpr",
        "left": left,
        "right": right,
        "operator": operator
      }

    return left

  def parse_multiplicative_expr(self):
    left = self.parse_primary_expr()

    while self.at()["value"] == "*" or self.at()["value"] == "/":
      operator = self.eat()["value"]
      right = self.parse_primary_expr()
      left = {
        "NodeType": "BinaryExpr",
        "left": left,
        "right": right,
        "operator": operator
      }

    return left
  
  def parse_primary_expr(self):
    tk = self.at()["type"]

    match tk:
      case "IDENTIFIER":
        return {"NodeType": "Identifier", "value": self.eat()["value"]}
      
      case "NUMBER":
        return {"NodeType": "NumericLiteral", "value": float(self.eat()["value"])}
            
      case "OPAREN":
        self.eat()
        value = self.parse_expr()
        self.expect("CPAREN", "Unexpected token inside parenthesised expression. Expected closing parenthesis.")
        return value
      
      case "EOF":
        raise SyntaxError("Expression expected")

      case _:
        raise SyntaxError(f"Failed to parse: \"{self.eat()['value']}\"")
