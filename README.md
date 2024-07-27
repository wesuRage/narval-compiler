# Compilador da linguagem de programação Narval

Narval é uma linguagem de programação de propósito geral baseada em Assembly com a sintaxe semelhante a Python e Rust. Apesar de seu propósito geral, contém bibliotecas builtin prontas para Data Science, Big Data, Machine Learning e Deep Learning. A linguagem atualmente possui suporte para as arquiteturas x86_64 e ARM64 (em desenvolvimento).

### Instalação da fonte

```sh
git clone https://github.com/wesuRage/narval-compiler.git
cd narval-compiler
make
make install
```

# Sintaxe

### Hello, World!
Para imprimir na tela um simples **hello world**, você utiliza a função `write`.

```narval
write("Hello, World!");
```

### Declaração de constantes
Se você não vai precisar alterar seu valor após declarado, apenas escreva o nome da constante e atribua à ela seu valor:

```narval
minhaConstante = "Olá, Mundo!";
```
### Declaração de variáveis
Mas caso precise alterar o seu valor precise ser alterado, use a keyword `val` antes do nome da sua variável:

```narval
val minhaVariavel = [];
minhaVariavel[0] = "Olá!"
```

### Tipos & Tamanhos de dados
Narval é uma linguagem com tipagem inferida, mas você pode deixar a tipagem explícita em cada declaração. Os tipos de dados primitivos em Narval são: `text`, `integer`, `decimal`, `boolean`, `void`, `array`, `tuple` e `object`.
```narval
nome = "Alexandre";
idade = 14;
peso = 850.6;
vivo = true;
```
A sintaxe acima corresponde à:
```narval
nome: text = "Alexandre";
idade: integer = 14;
peso: decimal = 850.6;
vivo: boolean = true;
```
Confira as seguintes tipagens:
```narval
habilidades: Array<text> = ["programar", "ler mangás", "jogar"]

informacoes: (text, integer, decimal, boolean, Array<text>) = (nome, idade, peso, vivo, habilidades);

indefinido: void = (); // não confundir com uma tupla

complexo: Object<Array<(integer, text)>> = {
  key1: [(1, "a"), (2, "b")]
  key2: [(3, "c"), (4, "d")]
};
```
Em objetos, se a chave possui o mesmo nome que o valor, pode se omitir o símbolo ":" e colocar apenas a nome.
```narval
num: integer = 3;
habilidades: Array<text> = ["programar", "ler mangás", "jogar"]

meuObj: Object<void> = {
  nome: "João",
  num, // o mesmo que num: num,
  habilidades, // o mesmo que habilidades: habilidades,
  ultima_vez_online: "15:30",
};

```
Você também pode especificar o tamanho 

### Condicionais & When Case
As condicionais em Narval são formadas por `if`, `elif` e `else`:

```narval
nome = "Alexandre";

if nome == "João" {
  write("Olá, João!);
} elif nome == "Alexandre {
  write("Olá, Alexandre");
} else {
  write("Olá, Estranho!);
}
```

Já o `when case` compara um único valor com outros valores:

```narval
nome = "Alexandre";

when nome {
  "João": write("Olá, João!"),
  "Alexandre": write("Olá, Alexandre!"),
  "Marcelo": write("Olá, Marcelo!"),
  "Thiago": write("Olá, Thiago!"),
  _: {
    write("Olá, Estranho!");
    write("Infelizmente não te conheço.")
  }
}

idade = 50;

when idade {
  0..=12: write("Você é criança!"),
  13..=17: write("Você é adolescente!"),
  18..=59: write("Você é adulto"!),
  _: write("Você é idoso"),
}
```
### Funções
Uma função em narval usa a keyword `label`
```

```