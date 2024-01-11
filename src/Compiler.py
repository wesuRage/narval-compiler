import os

class Compiler:
  def compile(self, output):
    out = output.split(".")[0]

    os.system(f"nasm -felf64 {out}.asm -o {out}.o")

    if os.name == "nt":
      os.system(f"gcc -no-pie -o {out} {out}.o")
      # os.system(f"del {out}.asm")
      os.system(f"del {out}.o")
    else:
      os.system(f"gcc -no-pie -o {out} -z noexecstack{out}.o")
      # os.system(f"rm {out}.asm")
      os.system(f"rm {out}.o")