from src.Env import Env

class Checker:
  def __init__(self) -> None:
    self.envs = [Env(ispersistant=True)]
    self.env = self.envs[0]

    self.objectKeys = []

    self.typedefs = {
      'number': {
        'type': 'native',
        'has+': True,
        'has-': True,
        'has/': True,
        'has*': True,
        'has%': True
      },
      'string': {
        'type': 'native',
        'has+': False,
        'has-': False,
        'has/': False,
        'has*': False,
        'has%': False
      }
    }


  def getNodeHandler(self, nodetype):
    return Checker.__dict__["eval_" + nodetype]


  def eval(self, node):
    handler = self.getNodeHandler(node["NodeType"])
    return handler(self, node)
  

  def eval_Program(self, node):
    for stmt in node["body"]:
      self.eval(stmt)


  def eval_VarDeclaration(self, node):
    if node.get("value"):
      typevalue = self.eval(node["value"])
      definition = {
        'value': node["value"],
        'typeValue': typevalue,
        'type': node["type"]
      }

      id = node["Identifier"]["value"]
      value = node["value"]
      directive = node["directive"]

      if node["type"] == "constant":
        return self.env.newName(id, value, directive, True)
      else:
        return self.env.newName(id, value, directive)
    else:
      typevalue = None
      definition = {
        'value': None,
        'typeValue': typevalue,
        'type': node["type"]
      }

      id = node["Identifier"]["value"]
      value = None
      directive = node["directive"]

      return self.env.newName(id, value, directive)


  def eval_AssignmentExpr(self, node):
    if node["assigne"]["NodeType"] != "Identifier":
      raise SyntaxError("Invalid expression in assignment")
    
    varname = node["assigne"]["value"]
    value = node["value"]
    directive = node["directive"]

    self.env.setName(varname, value, directive)


  def eval_ObjectLiteral(self, node):
    keys = []
    for prop in node["properties"]:
      if prop["key"] not in keys:
        keys.append(prop["key"])
      else:
        raise SyntaxError(f"Key '{prop['key']}' already defined in scope")
      
      self.eval(prop["value"])

  def eval_NumericLiteral(self, node):
    return "number"
  

  def eval_String(self, node):
    return "string"
  

  def eval_Identifier(self, node):
    return self.env.getName(node["value"])

  def eval_MemberExpr(self, node):
    if isinstance(self.get(node["object"]), list):
      target = self.get(node["object"])[0]
    else:
      target = self.get(node["object"])

    #print("OOOOOOOOOOOOOOOOO MEU PAI QUE ABENÇOE ESSE PRINT DE DEBUG AMEM", target)
    prop = node["property"]
    if node["computed"]:
      self.eval(prop)
    propindex = 0
    for pair in target["properties"]:
      if pair["key"] == prop["value"]:
        return target["properties"][propindex]["value"]
      propindex += 1
    else:
      raise AttributeError("Cannot find property in object")

  def get(self, node):
    if node["NodeType"] == "Identifier":
      return self.env.getName(node["value"])
    return self.eval(node)
    
  
  def eval_BinaryExpr(self, node):
    left = node["left"]
    right = node["right"]

    ltype = self.eval(left)
    rtype = self.eval(right)

    if not self.supportsOp(ltype, node["operator"]):
      raise TypeError(f"'{ltype}' doesnt supports operation '{node['operator']}'")
    
    if ltype == "null" or rtype == "null":
      raise TypeError("Operations with null values are not allowed.")
    return "number"
  
  def supportsOp(self, type, op):
    typedef = self.typedefs[type]

    return typedef["has" + op]

