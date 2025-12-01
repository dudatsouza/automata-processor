# Simulador e Conversor de Autômatos — LFA

Este projeto implementa um sistema completo para **manipulação, conversão e simulação de autômatos**, desenvolvido como trabalho da disciplina de **Linguagens Formais e Autômatos (LFA)** do CEFET-MG.  
O software realiza conversões entre diferentes tipos de autômatos (AFN, AFN-ε, AFD, AFD minimizado, multi-inicial) e permite testar palavras seguindo as regras formais de cada modelo.

---

## ✨ Funcionalidades Principais

- 🔹 **Conversão AFN → AFD** (Método do Subconjunto)  
- 🔹 **Conversão AFN-ε → AFN** (remoção sistemática de ε-transições)  
- 🔹 **Conversão para AFD Multi-Inicial**  
- 🔹 **Minimização de AFD** (usando particionamento)  
- 🔹 **Simulação de palavras** (em qualquer autômato) 

---

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

---

## 🔎 Resumo das Conversões

### 🟦 AFN → AFD (Método do Subconjunto)
O método constrói um AFD onde **cada estado representa um conjunto de estados do AFN**.  
Para cada conjunto e símbolo do alfabeto calcula-se:

```
δ(S, a) = união das transições de cada estado de S com símbolo a
```

Resulta em um autômato determinístico **equivalente**.

---

### 🟩 AFN-ε → AFN (Remoção de ε-fechos)
O algoritmo remove todas as ε-transições usando:

```
ε-fecho(q) = todos os estados alcançáveis a partir de q usando ε
```

As transições são reconstruídas sem depender de ε-movimentos.

---

### 🟧 AFN Multi-Inicial → AFD
Quando o autômato possui múltiplos estados iniciais, cria-se um **novo estado inicial artificial**, conectado via ε para todos os iniciais originais.  
Após isso, aplica-se o método de subconjuntos.

---

### 🟥 Minimização de AFD
A minimização segue o algoritmo clássico de particionamento:

1. Separa estados finais e não finais  
2. Refina grupos até estabilizar  
3. Constrói novo AFD mínimo equivalente

---

### 🟨 Simulação de Palavras
O sistema lê a palavra símbolo a símbolo, navegando pelas transições:

- Se a leitura termina em um estado final → **aceita**
- Caso contrário → **rejeita**

---

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

---

## 🛠 Compilação

### 🔹 **Windows**
Instale o FreePascal e rode:

```bash
fpc main.pas
```

### 🔹 **Linux (Debian/Ubuntu)**

```bash
sudo apt install fpc
fpc main.pas
```

O executável será:

- `main.exe` (Windows)  
- `main` (Linux)

---

## ▶️ Execução

```bash
./main data/automato.json
```

---

## 👨‍💻 Autores

Trabalho desenvolvido em dupla pelos seguintes alunos:

<div align="center">

**Maria Eduarda Teixeira Souza**  
*Estudante de Engenharia de Computação @ CEFET-MG*  
<br>  
[![Gmail][gmail-badge]][gmail-duda]

<br><br>

**João Francisco Teles da Silva**  
*Estudante de Engenharia de Computação @ CEFET-MG*  
<br>  
[![Gmail][gmail-badge]][gmail-joao]

</div>

---

[gmail-badge]: https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=gmail&logoColor=white
[gmail-duda]: mailto:dudateixeirasouza@gmail.com
[gmail-joao]: mailto:joaoteles0505@gmail.com

[telegram-autor3]: https://t.me/dudat_18
[gmail-autor3]: mailto:dudateixeirasouza@gmail.com

[linkedin-autor4]: https://
[telegram-autor4]: https://
[gmail-autor4]: mailto:

[linkedin-badge]: https://img.shields.io/badge/-LinkedIn-0077B5?style=for-the-badge&logo=Linkedin&logoColor=white
[telegram-badge]: https://img.shields.io/badge/Telegram-2CA5E0?style=for-the-badge&logo=telegram&logoColor=white
[gmail-badge]: https://img.shields.io/badge/-Gmail-D14836?style=for-the-badge&logo=Gmail&logoColor=white
