from os import system

class Compiler:
  def compile(self, output):
    out = output.split(".")[0]
    system(f"nasm -felf64 {out}.asm -o {out}.o")
    system(f"gcc -no-pie -o {out} -z noexecutestack {out}.o")
    # system(f"rm {out}.asm")
    system(f"rm {out}.o")