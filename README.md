# Narval
Narval é uma linguagem de programação transpilada para Assembly x86_64 NASM Linux sintaxe Intel.
# Sintaxe
Declarar constantes:
```
byte meuByte = 3;
word minhaWord = "olá mundo!";
dword ... ;
qword ... ;
```
Declarar variáveis:
```
resb caracter[1]= "A";
resw word[10] = "Narval";
resd ... ;
resq ... ;
```
Assinar valores à variáveis:
```
resb numero[3] = 1;
palavra = 2; //permitido

byte constante = "bla bla bla";
constante = "olá"; // não permitido

```


