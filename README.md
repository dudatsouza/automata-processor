<h1 align='center'>
  Simulador e Conversor de Autômatos — LFA
</h1>

<div align='center'>
 
[![Pascal][pascal-badge]][pascal-url]
[![Build][make-badge]][make-url]
[![Ubuntu][ubuntu-badge]][Ubuntu-url]
[![Windows][windows-badge]][windows-url]
[![macOS][macos-badge]][macos-url]



<b>
  Maria Eduarda Teixeira Souza<br>
  João Francisco Teles da Silva<br>
</b>
  
<br>
Linguagens Formais e Autômatos  <br>
Engenharia de Computação <br>
CEFET-MG Campus V <br>
2025/2 


</div>

## 📚 O Projeto

Este projeto implementa um sistema completo para **manipulação, conversão e simulação de autômatos**, desenvolvido como trabalho da disciplina de **Linguagens Formais e Autômatos (LFA)** do CEFET-MG.  
O software realiza conversões entre diferentes tipos de autômatos (AFN, AFN-ε, AFD, AFD minimizado, multi-inicial) e permite testar palavras seguindo as regras formais de cada modelo.


## ✨ Funcionalidades Principais

- 🔹 **Conversão AFN → AFD** (Método do Subconjunto)  
- 🔹 **Conversão AFN-ε → AFN** (remoção sistemática de ε-transições)  
- 🔹 **Conversão para AFD Multi-Inicial**  
- 🔹 **Minimização de AFD** (usando particionamento)  
- 🔹 **Simulação de palavras** (em qualquer autômato) 


## 📂 Estrutura do Projeto

A seguir está a estrutura geral do sistema, organizada por módulos:

```
src/
 ├── core/
 │    ├── automaton.pas         # Estrutura base do autômato
 │    ├── io.pas                # Entrada e saída (JSON)
 │    └── utils.pas             # Funções auxiliares
 │
 ├── conversion/
 │    ├── afn_afd_conversion.pas      # Conversão AFN → AFD
 │    ├── afne_afn_conversion.pas     # Conversão AFN-ε → AFN
 │    ├── afne_conversion.pas         # Interpretação de AFN-ε
 │    └── afd_minimization.pas        # Minimização de AFD
 │
 ├── simulation/
 │    └── words_test.pas        # Algoritmo de simulação de palavras
 │
 └── main.pas                   # Execução principal
```


## 🔎 Resumo das Conversões

### 🟦 AFN → AFD (Método do Subconjunto)
O método constrói um AFD onde **cada estado representa um conjunto de estados do AFN**.  
Para cada conjunto e símbolo do alfabeto calcula-se:

```
δ(S, a) = união das transições de cada estado de S com símbolo a
```

Resulta em um autômato determinístico **equivalente**.


### 🟩 AFN-ε → AFN (Remoção de ε-fechos)
O algoritmo remove todas as ε-transições usando:

```
ε-fecho(q) = todos os estados alcançáveis a partir de q usando ε
```

As transições são reconstruídas sem depender de ε-movimentos.


### 🟧 AFN Multi-Inicial → AFD
Quando o autômato possui múltiplos estados iniciais, cria-se um **novo estado inicial artificial**, conectado via ε para todos os iniciais originais.  
Após isso, aplica-se o método de subconjuntos.


### 🟥 Minimização de AFD
A minimização segue o algoritmo clássico de particionamento:

1. Separa estados finais e não finais  
2. Refina grupos até estabilizar  
3. Constrói novo AFD mínimo equivalente


### 🟨 Simulação de Palavras
O sistema lê a palavra símbolo a símbolo, navegando pelas transições:

- Se a leitura termina em um estado final → **aceita**
- Caso contrário → **rejeita**


## 📄 Estrutura do JSON

Exemplo de arquivo aceito pelo programa:

```json
{
  "states": ["q0", "q1", "q2"],
  "initialState": ["q0"],
  "finalStates": ["q2"],
  "alphabet": ["a", "b"],
  "transitions": [
    { "source": "q0", "symbol": "a", "target": "q1" }
  ]
}
```


## 🚀 Execução e Compilação

### 📥 Clonando o repositório

Primeiro, clone o repositório do projeto:

```bash
git clone https://github.com/dudatsouza/automata-processor.git
cd automata-processor
```

## 🧩 Instalação do Free Pascal

O projeto utiliza o **Free Pascal Compiler (FPC)**.
Siga as instruções de acordo com o seu sistema operacional.

### 🔹 **Windows**

1. Baixe o instalador oficial em:
   [https://www.freepascal.org/download.html](https://www.freepascal.org/download.html)
2. Durante a instalação, certifique-se de marcar a opção para adicionar o FPC ao `PATH`.
3. Verifique a instalação:

```powershell
fpc -h
```


### 🔹 **Linux (Debian / Ubuntu)**

```bash
sudo apt update
sudo apt install fpc
```

Verifique:

```bash
fpc -h
```


### 🔹 **macOS**

Usando o Homebrew:

```bash
brew install fpc
```

Verifique:

```bash
fpc -h
```


## 🛠️ Compilação e Execução

O processo de compilação e execução varia conforme o sistema operacional.


### 🔹 **Linux e macOS**

O projeto utiliza um **Makefile** para automatizar todo o processo.

#### Compilar e executar

```bash
make
```

Esse comando:

1. Remove arquivos de compilação anteriores
2. Compila o projeto
3. Executa o programa

#### Limpar arquivos de compilação

```bash
make clean
```


### 🔹 **Windows (PowerShell)**

No Windows, a compilação e execução são feitas via **PowerShell**, utilizando o script `exec.ps1`.

```powershell
powershell -ExecutionPolicy Bypass -File .\exec.ps1
```

Esse comando:

1. Limpa arquivos de compilação anteriores
2. Compila o projeto
3. Executa o programa


> [!IMPORTANT] 
> Usuários de **Windows** que possuam **WSL** ou **Git Bash** podem optar por utilizar o `Makefile`, seguindo os mesmos comandos do Linux/macOS.


## 👨‍💻 Autores

Trabalho desenvolvido pelos seguintes alunos:

<div align="center">

**Maria Eduarda Teixeira Souza**  
*Graduando - 6º Período de Engenharia de Computação @ CEFET-MG*  
<br>  
[![Gmail][gmail-badge]][gmail-duda]
[![Linkedin][linkedin-badge]][linkedin-duda]
[![Telegram][telegram-badge]][telegram-duda]

<br>

**João Francisco Teles da Silva**  
*Graduando - 6º Período de Engenharia de Computação @ CEFET-MG* <br>  
[![Gmail][gmail-badge]][gmail-joao]

</div>


[gmail-badge]: https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=gmail&logoColor=white
[linkedin-badge]: https://img.shields.io/badge/-LinkedIn-0077B5?style=for-the-badge&logo=Linkedin&logoColor=white
[telegram-badge]: https://img.shields.io/badge/Telegram-2CA5E0?style=for-the-badge&logo=telegram&logoColor=white


[gmail-joao]: mailto:joaoteles0505@gmail.com

[gmail-duda]: mailto:dudateixeirasouza@gmail.com
[telegram-duda]: https://t.me/dudat_18
[linkedin-duda]: https://www.linkedin.com/in/dudatsouza/


[pascal-badge]: https://img.shields.io/badge/Pascal-FreePascal-red?style=for-the-badge
[pascal-url]: https://www.freepascal.org/

[vscode-badge]: https://img.shields.io/badge/Visual%20Studio%20Code-0078d7.svg?style=for-the-badge&logo=visual-studio-code&logoColor=white
[vscode-url]: https://code.visualstudio.com/

[make-badge]: https://img.shields.io/badge/_-MAKEFILE-427819.svg?style=for-the-badge
[make-url]: https://www.gnu.org/software/make/manual/make.html

[ubuntu-badge]: https://img.shields.io/badge/Ubuntu-E95420?style=for-the-badge&logo=ubuntu&logoColor=white
[Ubuntu-url]: https://ubuntu.com/

[windows-badge]: https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white
[windows-url]: https://www.microsoft.com/windows/

[macos-badge]: https://img.shields.io/badge/macOS-000000?style=for-the-badge&logo=apple&logoColor=white
[macos-url]: https://www.apple.com/macos/
