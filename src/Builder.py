import re
import math

class Builder:
  def __init__(self):

    self.adds = []
    
    self.builtin_constants = [
      "NEWLINE equ 0xA\n"
      "NULL equ 0x0\n"
      "TRUE equ 0x1\n"
      "FALSE equ 0x0\n"
    ]

    self.builtin_functions = {
      "print": {
        "input": "esi",
      },
      "strlen": {
        "input": "rdi",
        "output": "rdx"
      },
      "int2str": {
        "input": "rax",
        "output": "INT2STR_BUFFER"
      }
    }

    self.section_data = [
      "section .data\n", 
    ]
    
    self.section_bss = [
      "section .bss\n"
    ]

    self.before_main = [

    ]

    self.section_text = [
      "section .text\n", 
      "\tglobal main\n\n",
      "main:\n"
    ]

    self.values = {}

    self.constants = {
      "NULL":"NULL", 
      "TRUE":"TRUE", 
      "FALSE": "FALSE"
    }


  def returnSizeNum(self, direct):
    directives = {
      "resb": 1,
      "resw": 2,
      "resd": 4,
      "resq": 4,
    }

    return directives.get(direct)

  
  def returnSize(self, direct):
    directives = {
      "resb": "byte",
      "resw": "word",
      "resd": "dword",
      "resq": "qword",
    }

    return directives.get(direct)


  def returnDirective(self, direct):
    directives = {
      "byte": "db",
      "word": "dw",
      "dword": "dd",
      "qword": "dq",
    }

    return directives.get(direct)

  def generate_mov_expr(self, identifier, value, directive):
    if directive == None:
      if isinstance(value, float):
        value = math.floor(value)
      self.section_text.append(f"\tmov {identifier}, {value}\n")
      return
    if re.match(r'^[+-]?\d+(\.\d+)?$', str(value)):
      value = int(value)
      self.section_text.append("\n")
      self.section_text.append(f"\tmov {self.returnSize(directive)} [{identifier}], {value}\n")
    elif self.values.get(value):
      value = self.values[value].replace('"', '').split(",")
      value = value[0]
      self.section_text.append("\n")
      last = 0
      for index in range(0, len(value), self.returnSizeNum(directive)):
        last = index + self.returnSizeNum(directive)
        val = value[index:last]
        self.section_text.append(f"\tmov {self.returnSize(directive)} [{identifier}+{index}], \"{val}\" \n")
      self.section_text.append(f"\tmov {self.returnSize(directive)} [{identifier}+{index+1}], 0x0 \n")
      
    elif value in self.constants:
      self.section_text.append("\n")
      self.section_text.append(f"\tmov {self.returnSize(directive)} [{identifier}], {value} \n")
    else:
      value = value.replace('"', '')

      self.section_text.append("\n")
      last = 0
      for index in range(0, len(value), self.returnSizeNum(directive)):
        last = index + self.returnSizeNum(directive)
        val = value[index:last]
        self.section_text.append(f"\tmov {self.returnSize(directive)} [{identifier}+{index}], \"{val}\" \n")

      self.section_text.append(f"\tmov {self.returnSize(directive)} [{identifier}+{last}], 0x0 \n")

  
  def getNodeHandler(self, nodetype):
    return Builder.__dict__["build_" + nodetype]


  def build(self, nodes, output):
    self.getNodeHandler(nodes["NodeType"])(self, nodes, output)


  def build_Program(self, node, output):
    for stmt in node["body"]:
      self.build(stmt, output)

    self.build_Code(output)


  def build_NumericLiteral(self, node, _):
    return "number"
  

  def build_ObjectProperties(self, props, prefix, directive, ident=1):
    self.section_data.append("\t"*ident+f"{prefix}:\n")
    
    for prop in props:
      key = prop["key"]
      value = prop["value"]


      if value["NodeType"] == "ObjectLiteral":
        pre = f"{prefix}_{key}"
        self.values[pre] = None
        self.build_ObjectProperties(value["properties"], pre, directive, ident+1)
      elif value["NodeType"] == "Identifier":
        val = self.values[value["value"]]
        if "value" in val:
          val = val["value"]

        self.section_data.append("\t"*(ident+1)+f"{prefix}_{key} {self.returnDirective(directive)} {val}\n")
        self.values[f"{prefix}_{key}"] = val

      else:
        val = value["value"]
        if isinstance(val, str):
          if not any(val in value for value in self.builtin_constants):
            val = val + ',0x0'
        self.section_data.append("\t"*(ident+1)+f"{prefix}_{key} {self.returnDirective(directive)} {val}\n")
        self.values[f"{prefix}_{key}"] = val

    return f"{prefix}"


  def build_FunctionDeclaration(self, node, output):
    name = f'{node["name"]}:\n'
    body = node["body"]

    if name in self.before_main:
      self.before_main.append(name)

    stmts = []

    for stmt in body:
      stmts.append(self.build(stmt, output))

    self.section_text.extend(self.before_main)


  def build_CallExpr(self, node, _):
    print(node)

  
  def build_Add(self, node, _):
    self.adds.append(f'%include "{node["file"]}"\n')


  def build_Print(self, node, _):
    args = node["args"]
    for arg in args:
      self.build(arg, _)

      self.section_text.append("\tmov esi, eax\n\tcall print\n")
                   
  def build_BinaryExpr(self, node, _):
    expr: str | None = None
    if 'singular' in node['right']:
      value = node['right']['value']

      if isinstance(value, float):
        value = int(value)
      expr = f"\t{self.getOperator(node['operator'])} eax, {value}\n"
    else:
      self.build(node["right"], _)
      self.generate_mov_expr('edx', 'eax', None)
      expr = f"\t{self.getOperator(node['operator'])} eax, edx\n"

    if 'singular' in node['left']:
      self.generate_mov_expr('eax', node["left"]["value"], None)
    else:
      self.build(node["left"], _)

    self.section_text.append(expr)

    
    
  def getOperator(self, op):
    cases = {
      '+': 'add',
      '-': 'sub',
      '*': 'imul',
      '/': 'div'
    }

    return cases.get(op)
  
  def build_VarDeclaration(self, node, _):
    length = None

    id = node["Identifier"]["value"]
    directive = node["directive"]

    if "length" in node:
      length = int(node["length"])
      if node.get("value"):
        value = node["value"]["value"]
        expr = f"\t{id} {directive} {length}\n"
        self.section_bss.append(expr)
        self.generate_mov_expr(id, value, directive)
        self.values[id] = value

    else:
      if node["value"]["NodeType"] == "String":
        value = f"{node['value']['value']},0x0"
        expr = f"\t{id} {self.returnDirective(directive)} {value}\n"
        self.section_data.append(expr)
        self.values[id] = value

      elif node["value"]["NodeType"] == "ObjectLiteral":
        self.values[id] = self.build_ObjectProperties(node["value"]["properties"], id, directive)
        
        
      else:
        value = node["value"]
        if value["NodeType"] == "Identifier":
          value = self.values[value["value"]]
          if "value" in value:
            expr = f"\t{id} {self.returnDirective(directive)} {value['value']}\n"
            self.section_data.append(expr)
          else:
            expr = f"\t{id} {self.returnDirective(directive)} {value}\n"
            self.section_data.append(expr)
        else:
          if "value" in value:
            expr = f"\t{id} {self.returnDirective(directive)} {value['value']}\n"
            self.section_data.append(expr)
          else:
            expr = f"\t{id} {self.returnDirective(directive)} {value}\n"
            self.section_data.append(expr)
        self.values[id] = value


  def generate_object_name(self, object):
    obj = []

    def percorrer(object, prefix=""):
      if isinstance(object, dict):
        for key, value in object.items():
          if key == 'NodeType' and value == 'Identifier':
            obj.append(prefix + object['value'])
          else:
            percorrer(value, prefix)
      elif isinstance(object, list):
        for item in object:
          percorrer(item, prefix)

    percorrer(object)

    return "_".join(obj)


  def build_AssignmentExpr(self, node, _):
    if "value" in node["assigne"]:
      id = node["assigne"]["value"]
      value = node["value"]["value"]
      directive = node["directive"]

      self.generate_mov_expr(id, value, directive)
    else:
      id = self.generate_object_name(node["assigne"])
      
      directive = node["directive"]
      value = node["value"]

      if "value" in node["value"]:
        value = node["value"]["value"]
      else:
        value = self.generate_object_name(node["value"])

      self.generate_mov_expr(id, value, directive)

      
      

  def build_Code(self, output):
    out = output.split(".")[0]

    code = []

    code.extend(self.adds)
    code.append("\n")

    code.extend(self.builtin_constants)
    code.append("\n")

    if len(self.section_data) > 1:
      code.extend(self.section_data)
      code.append("\n")

    if len(self.section_bss) > 1:
      code.extend(self.section_bss)
      code.append("\n")

    code.extend(self.section_text)
    code.append("\nexit_program:\n\tmov eax, 1\n\txor ebx, ebx\n\tint 0x80\n")

    code = "".join(code)
    
    with open(f"{out}.asm", "w") as f:
      f.write(code)
    
    f.close()

  