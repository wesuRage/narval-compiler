class Env:
  def __init__(self, parent=None, ispersistant=False) -> None:
    self.parent = parent
    self.names = {}
    self.consts = {}
    self.external_names = {}
    self.ispersistant = ispersistant


  def getName(self, name):
    if name in self.names:
      return self.names[name]
    
    if self.parent:
      result = self.parent.getName(name)
      self.external_names[name] = result
      return result

    raise NameError(f"Cannot found name '{name}'")


  def newName(self, name, value, directive, isconst=False):
    if name in self.names:
      raise NameError(f"Attempt to re-define name '{name}'")

    self.names[name] = [value, directive]
    if isconst:
      self.consts[name] = None
    

  def setName(self, name, value, directive):
    can_change = False

    if name in self.names:
      can_change = True
    else:
      if self.parent and self.parent.ispersistant:
        if name in self.parent.names:
          can_change = True

    if name in self.consts:
      raise NameError(f"Attempt to change a constant name '{name}'")
    elif not can_change:
      raise NameError(f"Attempt to set a non-defined name '{name}'")
    elif name in self.names:
      self.names[name] = [value, directive]
    else:
      if self.parent.ispersistant:
        self.parent.setName(name, value, directive)
