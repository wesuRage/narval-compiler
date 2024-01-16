def MK_BOOL(bool=True):
  return {"type": "boolean", "value": bool}

def MK_NULL():
  return {"type": "null", "value": "null"}


def setupGlobalScope(scope):
  scope.newName("true", MK_BOOL(True), None, True)
  scope.newName("false", MK_BOOL(False), None, True)
  scope.newName("null", MK_NULL, None, True)

class Env:
  def __init__(self, parent=None, ispersistant=False) -> None:
    globalScope = True if parent else False
    self.parent = parent
    self.names = {}
    self.consts = {}
    self.external_names = {}
    self.ispersistant = ispersistant

    if globalScope:
      setupGlobalScope(self)

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
