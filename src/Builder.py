import re

class Builder:
  def __init__(self):
    self.section_data = ["section .data\n", "\tBUILTIN_NEWLINE db 0xA\n"]
    self.section_rodata = ["section .rodata\n"]
    self.section_bss = ["section .bss\n"]
    self.section_text = ["section .text\n", "\tglobal main\n\n", "main:\n"]
    self.values = {}


  def generate_mov_expr(self, id, value):
    if re.match(r'^[+-]?\d+(\.\d+)?$', str(value)):
      value = int(value)
      self.section_text.append(f"\tmov qword [{id}], {value}\n")
    else:
      self.section_text.append("\n")
      for index in range(0, len(value), 4):
        val = value[index:index + 4]
        self.section_text.append(f"\tmov qword [{id}+{index}], \"{val}\" \n")


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
        self.generate_mov_expr(id, value)

    else:
      if node["value"]["NodeType"] == "String":
        value = f"\"{node['value']['value']}\",0"
      else:
        value = int(node["value"]["value"])

    if type == "constant":
      expr = f"\t{id} {self.returnDirective(directive)} {value}\n"
      self.section_rodata.append(expr)
    elif length:
      expr = f"\t{id} {directive} {length}\n"
      self.section_bss.append(expr)
    else:
      expr = f"\t{id} {directive} {value}\n"
      self.section_data.append(expr)
    self.values[id] = value


  def build_AssignmentExpr(self, node, _):
    id = node["assigne"]["value"]
    value = node["value"]["value"]

    self.generate_mov_expr(id, value)


  def build_Print(self, node, _):
    printsyscode = '4'
    arg1 = "eax"
    descriptor = "1"
    arg2 = "ebx"
    
    arg3 = "ecx"
    if node["value"]["NodeType"] == "String":
      value = f'"{node["value"]["value"]}",0'
      size = len(value[1:-3]) + 1
    else:
      value = str(self.values[node["value"]["value"]])
      size = 0
      for _ in value:
        size+=1

    arg4 = "edx"
    self.section_text.append(f"\n\tmov {arg1}, {printsyscode}\n")
    self.section_text.append(f"\tmov {arg2}, {descriptor}\n")
    self.section_text.append(f"\tmov {arg3}, {node['value']['value']}\n")
    self.section_text.append(f"\tmov {arg4}, {size}\n")
    self.section_text.append("\tint 0x80\n")

    self.section_text.append(f"\n\tmov {arg1}, {printsyscode}\n")
    self.section_text.append(f"\tmov {arg2}, {descriptor}\n")
    self.section_text.append(f"\tmov {arg3}, BUILTIN_NEWLINE\n")
    self.section_text.append(f"\tmov {arg4}, 1\n")
    self.section_text.append("\tint 0x80\n")


  def build_Code(self, output):
    out = output.split(".")[0]

    code = []
    code.extend(self.section_data)
    code.append("\n")
    code.extend(self.section_rodata)
    code.append("\n")
    code.extend(self.section_bss)
    code.append("\n")
    code.extend(self.section_text)
    code.append("\nexit_program:\n\tmov eax, 1\n\txor ebx, ebx\n\tint 0x80\n")

    code = "".join(code)
    
    with open(f"{out}.asm", "w") as f:
      f.write(code)
    
    f.close()

  