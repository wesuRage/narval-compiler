import os
from shutil import which

class Compiler:
  def exists(self, name):
    return which(name) is not None

  def compile(self, output):
    out = output.split(".")[0] # ele pega o primeiro ponto que acha, aí da bug

    if not self.exists("nasm"):
      install = input("ERROR: NASM not installed. install? (y/n) ").lower()
      if install == "y" or install == "yes":
        os.system("sudo apt install nasm -y")
      else:
        print("ABORTING...")
        exit(1)

    if not self.exists("gcc"):
      install = input("ERROR: gcc not installed. install? (y/n) ").lower()
      if install == "y" or install == "yes":
        os.system("sudo apt install gcc -y")
      else:
        print("ABORTING...")
        exit(1)

    os.system(f"nasm -felf64 {out}.asm -o {out}.o -i src/assembly/")
    os.system(f"gcc -no-pie {out}.o -o {out} -z noexecstack")
    # os.system(f"rm {out}.asm")
    os.system(f"rm {out}.o")