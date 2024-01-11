import re


class Builder:
  def __init__(self):
    self.section_data = ["section .data\n"]
    self.section_rodata = ["section .rodata\n"]
    self.section_bss = ["section .bss\n"]
    self.section_text = ["section .text\n", "\tglobal main\n\n", "main:\n"]

  def getNodeHandler(self, nodetype):
    return Builder.__dict__["build_" + nodetype]

  def build(self, nodes, output):
    self.getNodeHandler(nodes["NodeType"])(self, nodes, output)

  def build_Program(self, node, output):
    for stmt in node["body"]:
      self.build(stmt, output)

    self.build_Code(output)

  def build_VarDeclaration(self, node, output):
    length = None

    id = node["Identifier"]["value"]
    directive = node["directive"]
    type = node["type"]

    if node.get("length"):
      length = int(node["length"]["value"])
      if node.get("value"):
        value = node["value"]["value"]
        if re.match(r'^[+-]?\d+(\.\d+)?$', f"{value}"):
          value = int(value)
        self.section_text.append(f"\tmov [{id}], {value}\n")

    else:
      value = node["value"]["value"]


    if type == "constant":
      expr = f"\t{id} {directive} {value}\n"
      self.section_rodata.append(expr)
    elif length:
      expr = f"\t{id} {directive} {length}\n"
      self.section_bss.append(expr)
    else:
      expr = f"\t{id} {directive} {value}\n"
      self.section_data.append(expr)
  
  def build_AssignmentExpr(self, node, output):
    print("ERROR: Not implemented")
    exit(1)

  def build_Code(self, output):
    out = output.split(".")[0]

    code = "".join(self.section_data)
    code += "\n"
    code += "".join(self.section_rodata)
    code += "\n"
    code += "".join(self.section_bss)
    code += "\n"
    code += "".join(self.section_text)

    code += "\n\tmov eax, 1\n\txor ebx, ebx\n\tint 0x80\n"
    
    with open(f"{out}.asm", "w") as f:
      f.write(code)
    
    f.close()