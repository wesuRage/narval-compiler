import os

class Compiler:
  def compile(self, output):
    out = output.split(".")[0]

    os.system(f"nasm -felf64 {out}.asm -o {out}.o")
    os.system(f"gcc -no-pie -o {out} {out}.o -z noexecstack")
    # os.system(f"rm {out}.asm")
    os.system(f"rm {out}.o")