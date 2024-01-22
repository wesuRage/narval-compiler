import os

def main():
  results = []
  for file in os.listdir("tests"):
    test(os.path.join("tests", file), results)
  for result in results:
    print(result)

def test(path, output):
  env = {}
  with open(path, 'r') as f:
    content = f.read()
    code = compile(content, path, "exec")

  exec(code, env)

  output.append(env['test']())


if __name__ == "__main__":
  main()

