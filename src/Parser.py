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
        "length": int(length["value"]),
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
        "length": int(length["value"]),
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
    left = self.parse_object_expr()

    if self.at()["type"] == "EQUALS":
      self.eat()
      value = self.parse_assignment_expr()
      self.expect("SEMICOLON", "Expected ';' at the end of statement.")
      return {"NodeType": "AssignmentExpr", "assigne": left, "value": value, "directive": self.token_directive[left["value"]]}
    
    return left
  
  def parse_object_expr(self):
    if self.at()["type"] != "OBRACE":
      return self.parse_additive_expr()
    
    self.eat()

    properties = []
    
    while self.not_eof() and self.at()["type"] != "CBRACE":
      key = self.expect("IDENTIFIER", "Identifier key expected on object literal")["value"]
      self.expect("COLON", "Missing colon following object expression.")
      value = self.parse_expr()

      properties.append({"key": key, "value": value})

      if self.at()["type"] != "CBRACE":
        self.expect("COMMA", "Expected ',' or '}' on object literal.")

    self.expect("CBRACE", "Expected '}' on object literal")
    return {"NodeType": "ObjectLiteral", "properties": properties}
  
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
    left = self.parse_call_member_expr()

    while self.at()["value"] == "*" or self.at()["value"] == "/":
      operator = self.eat()["value"]
      right = self.parse_call_member_expr()
      left = {
        "NodeType": "BinaryExpr",
        "left": left,
        "right": right,
        "operator": operator
      }

    return left
  
  
  def parse_call_member_expr(self):
    member = self.parse_member_expr()

    if self.at()["type"] == "OPAREN":
      return self.parse_call_expr(member)
    
    return member


  def parse_call_expr(self, caller):
    call_expr = {"NodeType": "CallExpr", "caller": caller, "args": self.parse_args()}

    if self.at()["type"] == "OPAREN":
      call_expr = self.parse_call_expr(call_expr)

    return call_expr


  def parse_args(self):
    self.expect("OPAREN", "Expected '(' at expression.")
    
    args = [] if self.at()["type"] == "CPAREN" else self.parse_arguments_list()

    self.expect("CPAREN", "'(' was not closed")

    return args


  def parse_arguments_list(self):
    args = [self.parse_assignment_expr()]

    while self.at()["type"] == "COMMA" and self.eat():
      args.append(self.parse_assignment_expr())

    return args

  
  def parse_member_expr(self):
    object = self.parse_primary_expr()

    while self.at()["type"] == "DOT" or self.at()["type"] == "OBRACKET":
      operator = self.eat()
      property = None
      computed = False

      if operator["type"] == "DOT":
        property = self.parse_primary_expr()

        if property["NodeType"] != "Identifier":
          raise SyntaxError("Cannot use dot operator without an identifier.")
      else:
        computed = True
        property = self.parse_expr()
        self.expect("CBRACKET", "'[' was not closed.")

      object = {
        "NodeType": "MemberExpr",
        "object": object,
        "property": property,
        "computed": computed
      }

    return object


  def parse_primary_expr(self):
    tk = self.at()["type"]

    match tk:
      case "IDENTIFIER":
        return {"NodeType": "Identifier", "value": self.eat()["value"]}
      
      case "NUMBER":
        return {"NodeType": "NumericLiteral", "value": float(self.eat()["value"])}
            
      case "STRING":
        return {"NodeType": "String", "value": self.eat()["value"]}
      
      case "OPAREN": 
        self.eat()
        value = self.parse_expr()
        self.expect("CPAREN", "Unexpected token inside parenthesised expression. Expected closing parenthesis.")
        return value
    
      case "EOF":
        raise SyntaxError("Expression expected")

      case _:
        raise SyntaxError(f"Failed to parse: \'{self.eat()['value']}\'")
