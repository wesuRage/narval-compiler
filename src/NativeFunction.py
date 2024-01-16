class NativeFunction:
  def __init__(self, name, code, params) -> None:
    self.name = name
    self.code = code
    self.params = params

  def getAssemblyDef(self):
    return f"{self.name}:\n{self.code}"

  def getAssemblyCall(self, values):
    #"prealocando" um tamanho pra lista
    string = [None] * (len(values) + 1) 
    i = 0
    for reg, arg in zip(self.params, values):
      string[i] = f"mov {reg}, {arg}\n"
      i += 1

    string[-1] = f"call {self.name}\n"

    return "".join(string)

