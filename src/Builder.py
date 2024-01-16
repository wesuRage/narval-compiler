import re

class Builder:
  def __init__(self):
    self.section_data = ["section .data\n", "\tBUILTIN_NEWLINE db 0xA\n"]
    self.section_bss = ["section .bss\n"]
    self.section_text = ["section .text\n", "\tglobal main\n\n", "main:\n"]
    self.values = {} 


  def generate_mov_expr(self, id, value, directive):
    if re.match(r'^[+-]?\d+(\.\d+)?$', str(value)):
      value = int(value)
      self.section_text.append(f"\tmov {self.returnSize(directive)} [{id}], {value}\n")
    else:
      self.section_text.append("\n")
      last = 0
      for index in range(0, len(value), self.returnSizeNum(directive)):
        last = index + self.returnSizeNum(directive)
        val = value[index:last]
        self.section_text.append(f"\tmov {self.returnSize(directive)} [{id}+{index}], \"{val}\" \n")
      self.section_text.append(f"\tmov {self.returnSize(directive)} [{id}+{last}], 0 \n")



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


  def getNodeHandler(self, nodetype):
    return Builder.__dict__["build_" + nodetype]


  def build(self, nodes, output):
    self.getNodeHandler(nodes["NodeType"])(self, nodes, output)


  def build_Program(self, node, output):
    for stmt in node["body"]:
      self.build(stmt, output)

    self.build_Code(output)


  def build_VarDeclaration(self, node, _):
    length = None

    id = node["Identifier"]["value"]
    directive = node["directive"]
    type = node["type"]

    if node.get("length"):
      length = int(node["length"])
      if node.get("value"):
        value = node["value"]["value"]
        self.generate_mov_expr(id, value, directive)

    else:
      if node["value"]["NodeType"] == "String":
        value = f"\"{node['value']['value']}\",0"
      else:
        value = int(node["value"]["value"])

    if type == "constant":
      expr = f"\t{id} {self.returnDirective(directive)} {value}\n"
      self.section_data.append(expr)
    elif length:
      expr = f"\t{id} {directive} {length}\n"
      self.section_bss.append(expr)
    self.values[id] = value


  def build_AssignmentExpr(self, node, _):
    id = node["assigne"]["value"]
    value = node["value"]["value"]
    directive = node["directive"]

    self.generate_mov_expr(id, value, directive)


  def build_Code(self, output):
    out = output.split(".")[0]

    code = []
    code.extend(self.section_data)
    code.append("\n")
    code.extend(self.section_bss)
    code.append("\n")
    code.extend(self.section_text)
    code.append("\nexit_program:\n\tmov eax, 1\n\txor ebx, ebx\n\tint 0x80\n")

    code = "".join(code)
    
    with open(f"{out}.asm", "w") as f:
      f.write(code)
    
    f.close()

  