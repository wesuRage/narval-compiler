import os

class Compiler:
  def compile(self, output):
    out = output.split(".")[0] # ele pega o primeiro ponto que acha, aí da bug

    os.system(f"nasm -felf64 {out}.asm -o {out}.o")
    os.system(f"gcc -no-pie {out}.o -o {out} -z noexecstack")
    # os.system(f"rm {out}.asm")
    os.system(f"rm {out}.o")