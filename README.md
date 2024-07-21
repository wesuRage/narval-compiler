# Compilador da linguagem de programação Narval

Narval é uma linguagem de programação de propósito geral baseada em Assembly com a sintaxe semelhante a Python e Rust. Apesar de seu propósito geral, contém bibliotecas builtin prontas para Data Science, Big Data, Machine Learning e Deep Learning. A linguagem atualmente possui suporte para as arquiteturas x86_64 (AMD64) e ARM64 (AArch64)

## Instalação da fonte

```sh
git clone https://github.com/wesuRage/narval-compiler.git
chmod +x install.sh
./install.sh
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

### Tipos de dados
Narval é uma linguagem com tipagem inferida, mas voce pode deixar a tipagem explícita a cada declaração. Os tipos de dados primitivos em Narval são: `text`, `integer`, `decimal`, `boolean`, `any`, `array`, `tuple` e `object`.

```narval
nome: text = "Alexandre";
idade: integer = 14;
peso: decimal = 850.6;

habilidades: Array<text> = ["programar", "ler mangás", "jogar"]

informacoes: (text, integer, decimal, Array<text>) = (nome, idade, peso, habilidades);

complexo: Object<any> = {
  key1: 3.14
  key2: {
    myArray: [(1, 2), (3, 4), (5, 6)],
    anotherKey: true
  },
  key3: "bye bye"
};
```

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
  0..=12 { // de zero a 12 com o 12 incluso
    write("Você é criança!");
  },
  13..=17 {
    write("Você é adolescente!");
  },


}
```