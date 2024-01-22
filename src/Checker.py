from src.Env import Env

class Checker:
  def __init__(self) -> None:
    self.envs = [Env(ispersistant=True)]
    self.env = self.envs[0]

    self.objectKeys = {}
    self.adds = []

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


  def push_env(self):
    self.envs.append(Env(parent=self.envs[-1]))
    self.env = self.envs[-1]

  def pop_env(self):
    self.envs.pop()
    self.env = self.envs[-1]

  def eval_FunctionDeclaration(self, node):
    previousObjects = self.objectKeys
    self.push_env()
    self.objectKeys = {}

    for param in node['parameters']:
      self.env.newName(param, {"argumentobj": 1}, "byte")
    name = node["name"]
    body = node["body"]
    for stmt in body:
      self.eval(stmt)
    self.env.newName(name, body, None)
    
    self.pop_env()
    self.objectKeys = previousObjects

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
    if node["assigne"]["NodeType"] != "Identifier" and node["assigne"]["NodeType"] != "MemberExpr":
      raise SyntaxError("Invalid expression in assignment. Identifier or member expression only.")

    target = node["assigne"]
    targetnodetype = target["NodeType"]
    if targetnodetype == "Identifier":
      target = target["value"]
    
    value = node["value"]
    directive = node["directive"]

    if targetnodetype == 'Identifier':
      self.env.setName(target, value, directive)
    else:
      targ = self.get(target["object"])
      if "properties" in targ:
        for prop in targ["properties"]:
          if prop["key"] == target["property"]["value"]:
            break
        else:
          raise AttributeError(f"Attempt to set a non-defined property '{target['property']['value']}'.")
        


  def eval_ObjectLiteralProperties(self, node, keys):
    for prop in node["properties"]:
      if prop["key"] not in keys:
        keys.append(prop["key"])

        if prop["value"].get("value"):
          self.objectKeys[prop["key"]] = prop["value"]["value"]
        else: 
          newkeys = []
          self.objectKeys[prop["key"]] = newkeys
          self.eval_ObjectLiteralProperties(prop["value"], newkeys) 
      else:
        raise AttributeError(f"Key '{prop['key']}' already defined in this object.")
      self.eval(prop["value"])


  def eval_ObjectLiteral(self, node):
    keys = []
    self.eval_ObjectLiteralProperties(node, keys)
      

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

    prop = node["property"]
    if node["computed"]:
      self.eval(prop)
    propindex = 0
    for pair in target["properties"]:
      if pair["key"] == prop["value"]:
        return target["properties"][propindex]["value"]
      propindex += 1
    else:
      raise AttributeError(f"Cannot find property '{prop['value']}' in object.")


  def get(self, node):
    if node["NodeType"] == "Identifier":
      return self.env.getName(node["value"])
    return self.eval(node)
  

  def eval_Add(self, node):
    if node["file"] not in self.adds:
      self.adds.append(node["file"])
    else:
      raise NameError(f"File '{node['file']}' already imported.")

  def eval_Print(self, node):
    for arg in node["args"]:
      self.eval(arg)


  def eval_CallExpr(self, node):
    id = node["caller"]["value"]

    for arg in node["args"]:
      self.eval(arg)

    return self.env.getName(id)

  def eval_BinaryExpr(self, node):
    left = node["left"]
    right = node["right"]

    ltype = self.eval(left)
    rtype = self.eval(right)

    if not self.supportsOp(ltype, node["operator"]):
      raise TypeError(f"'{ltype}' doesnt supports operation '{node['operator']}'.")
    
    if ltype == "null" or rtype == "null":
      raise TypeError("Operations with null values are not allowed.")
    return "number"
  
  def supportsOp(self, type, op):
    typedef = self.typedefs[type]

    return typedef["has" + op]

