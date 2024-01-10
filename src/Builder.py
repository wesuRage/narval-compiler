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
    if node.get("value"):
      value = int(node["value"]["value"])
    else:
      value = int(node["length"])

    id = node["Identifier"]["value"]
    directive = node["directive"]

    expr = f"\t{id} {directive} {value}\n"

    type = node["type"]

    if type == "constant":
      self.section_rodata.append(expr)
    elif f"{value}".isdigit():
      self.section_bss.append(expr)
    else:
      self.section_data.append(expr)
  
  def build_AssignmentExpr(self, node):
    pass

  def build_Code(self, output):
    out = output.split(".")[0]

    code = "".join(self.section_data)
    code += "\n"
    code += "".join(self.section_rodata)
    code += "\n"
    code += "".join(self.section_bss)
    code += "\n"
    code += "".join(self.section_text)

    code += "\tmov eax, 1\n\txor ebx, ebx\n\tint 0x80\n"
    
    with open(f"{out}.asm", "w") as f:
      f.write(code)
    
    f.close()