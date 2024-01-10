from src.Env import Env

class Checker:
  def __init__(self) -> None:
    self.envs = [Env(ispersistant=True)]
    self.env = self.envs[0]

    self.typedefs = {
      'number': {
        'type': 'native',
        'has+': True,
        'has-': True,
        'has/': True,
        'has*': True,
        'has%': True,
      },
      'null': {
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

      if node["type"] == "constant":
        return self.env.newName(id, value, True)
      else:
        return self.env.newName(id, value)
    else:
      typevalue = None
      definition = {
        'value': None,
        'typeValue': typevalue,
        'type': node["type"]
      }

      id = node["Identifier"]["value"]
      value = None

      return self.env.newName(id, value)

    
  def eval_AssignmentExpr(self, node):
    if node["assigne"]["NodeType"] != "Identifier":
      raise SyntaxError("Invalid expression in assignment")
    
    varname = node["assigne"]["value"]
    value = node["value"]    

    self.env.setName(varname, value)
    

  def eval_NumericLiteral(self, node):
    return "number"
  
  def eval_Identifier(self, node):
    return self.env.getName(node["value"])

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

