from os import system

class Compiler:
  def compile(self, output):
    out = output.split(".")[0]
    system(f"nasm -felf64 {out}.asm -o {out}.o")
    system(f"gcc -no-pie -o {out} {out}.o")
    system(f"del {out}.asm")
    system(f"del {out}.o")