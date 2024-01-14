class Parser: 
  def __init__(self):
    self.tokens = []
    self.token_directive = {}

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
    tk_type =  self.at()["type"]
    if tk_type in ["BYTE", "WORD", "DWORD", "QWORD", "RESB", "RESW", "RESD", "RESQ"]:
      return self.parse_var_declaration()

    else:
      return self.parse_expr()
  
  def parse_var_declaration(self):
    type = self.at()["value"]
    isConstant = self.eat()["type"] in ["BYTE", "WORD", "DWORD", "QWORD"]
    Identifier = self.expect("IDENTIFIER", "Identifier expected.")
    
    if isConstant:
      self.expect("EQUALS", "Expected equals in variable declaration.")
      
      value = self.parse_expr()

      declaration = {
        "NodeType": "VarDeclaration",
        "Identifier": Identifier,
        "value": value,
        "type": "constant" if isConstant else "reserved",
        "directive": type
      }
      self.token_directive[Identifier["value"]] = type

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
        "directive": type
      }

      self.token_directive[Identifier["value"]] = type


      return declaration
 
    else:  
      self.expect("EQUALS", "Expected equals in variable declaration.")
      value = self.parse_expr()
      declaration = {
        "NodeType": "VarDeclaration",
        "Identifier": Identifier,
        "length": length["value"],
        "value": value,
        "type": "reserved",
        "directive": type

      }
      self.token_directive[Identifier["value"]] = type


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
      return {"NodeType": "AssignmentExpr", "assigne": left, "value": value, "directive": self.token_directive[left["value"]]}
    
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
      
      case "DQUOTES":
        self.eat()
        string = []
        while self.at()["type"] != "DQUOTES":
          string.append(self.at()["value"])
          self.eat()

        self.expect("DQUOTES", "Expected '\"' at statement.")

        return {"NodeType": "String", "value": " ".join(string)}

      case "EOF":
        raise SyntaxError("Expression expected")

      case _:
        raise SyntaxError(f"Failed to parse: \'{self.eat()['value']}\'")
