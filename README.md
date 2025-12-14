<h1 align='center'>
  Simulador e Conversor de Autômatos — LFA
</h1>

<div align='center'>
 
[![Pascal][pascal-badge]][pascal-url]
[![Build][make-badge]][make-url]

[![Linux][linux-badge]][Linux-url]
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

##
<details> 
  <summary>
    <b style='font-size: 20px'> ✨ Funcionalidades Principais  </b>
  </summary>

- 🔹 **Conversão AFN → AFD** (Método do Subconjunto)  
- 🔹 **Conversão AFN-ε → AFN** (remoção sistemática de ε-transições)  
- 🔹 **Conversão para AFD Multi-Inicial**  
- 🔹 **Minimização de AFD** (usando particionamento)  
- 🔹 **Simulação de palavras** (em qualquer autômato) 

</details>

##
<details> 
  <summary>
    <b style='font-size: 20px'> 📂 Estrutura do Projeto  </b>
  </summary> 

A seguir está a estrutura geral do sistema, organizada por módulos:

```
src/
├── core/
│   ├── automaton.pas      # Estruturas e tipos do autômato
│   ├── io.pas             # Leitura e escrita de arquivos JSON
│   └── utils.pas          # Funções auxiliares
│
├── conversions/
│   ├── afne_conversion.pas        # Tratamento de AFN-ε
│   ├── afne_afn_conversion.pas    # Conversão AFN-ε → AFN
│   ├── afn_afd_conversion.pas     # Conversão AFN → AFD
│   └── afd_minimization.pas       # Minimização de AFD
│
├── tests/
│   └── words_test.pas      # Simulação e teste de palavras no autômato
│
└── main/
    └── main.pas            # Programa principal (menus e execução)
```

</details>


##
<details> 
  <summary>
    <b style='font-size: 20px'> 🗂️ Análise do projeto  </b>
  </summary> 


### 
<details> 
  <summary>
    <b style='font-size: 18px'> 📂 Main </b>
  </summary> 

### 📌 main.pas
<!-- <details> 
  <summary>
    <b style='font-size: 16px'> 📌 main.pas </b>
  </summary>  -->

Este módulo implementa o **controle do fluxo principal do programa**, sendo responsável pela interação com o usuário e pela orquestração das conversões entre diferentes tipos de autômatos.

Nenhum algoritmo de Teoria de Linguagens Formais é implementado diretamente neste arquivo. Todas as operações teóricas são delegadas aos módulos especializados.

#### Função no projeto

O [`main.pas`](src/main/main.pas) atua como:

* ponto de entrada da aplicação;
* gerenciador do ciclo de execução;
* intermediário entre entrada/saída e os algoritmos de conversão.

Ele coordena a leitura do autômato, sua classificação e a execução sequencial das conversões permitidas.


#### Fluxo de execução

O fluxo geral do programa segue os seguintes passos:

1. **Leitura do autômato**

   * O autômato é carregado a partir de um arquivo JSON.
   * A descrição é convertida para a representação formal interna.

2. **Classificação do autômato**

   * O autômato é classificado como:

     * multi-inicial,
     * AFN-ε,
     * AFN,
     * AFD,
     * ou AFD mínimo.

3. **Seleção dinâmica de operações**

   * O menu apresentado ao usuário depende do tipo atual do autômato.
   * Apenas conversões teoricamente válidas são disponibilizadas.

4. **Encadeamento de conversões**

   * Quando uma conversão direta não é possível, o programa executa automaticamente a cadeia de conversões intermediárias correta, como:

     * AFN-ε → AFN → AFD
     * AFN → AFD → AFD mínimo

Esse encadeamento reflete diretamente as equivalências formais demonstradas na teoria de autômatos.

</details>

### 
<details> 
  <summary>
    <b style='font-size: 18px'> 📂 Core </b>
  </summary> 

###
<details> 
  <summary>
    <b style='font-size: 16px'> 📎 automaton.pas </b>
  </summary> 

Este módulo define a **representação formal interna de um autômato finito**, servindo como base comum para todos os algoritmos de conversão e análise implementados no projeto.

O autômato é modelado por meio de um registro (`record`) que corresponde diretamente à definição matemática clássica de um autômato finito:

$$
A = (Q, \Sigma, \delta, I, F)
$$

onde:

* ($Q$) é o conjunto de estados,
* ($\Sigma$) é o alfabeto,
* ($\delta$) é a função de transição,
* ($I$) é o conjunto de estados iniciais,
* ($F$) é o conjunto de estados finais.

#### Estrutura de dados

A estrutura [`TAutomaton`](src/core/automaton.pas#L22) utiliza **arrays estáticos com contadores explícitos**, evitando alocação dinâmica de memória e garantindo previsibilidade no uso de recursos.

Os principais componentes são:

* **Estados (`states`)**
  Representa o conjunto (Q), armazenado como um vetor de identificadores de estados.

* **Alfabeto (`alphabet`)**
  Representa o conjunto de símbolos (\Sigma).

* **Estados iniciais (`initialState`)**
  Representa o conjunto (I), permitindo múltiplos estados iniciais. Essa generalização é fundamental para suportar AFNs multi-iniciais e AFNs-ε.

* **Estados finais (`finalStates`)**
  Representa o conjunto (F).

* **Transições (`transitions`)**
  Cada transição é representada por um registro contendo estado de origem, símbolo e estado de destino, permitindo múltiplas transições para um mesmo par (estado, símbolo), conforme a definição de autômatos não determinísticos.

* **Classificação (`classification`)**
  Campo auxiliar que indica o tipo atual do autômato (AFD, AFN, AFN-ε, multi-inicial, AFD mínimo), permitindo que o fluxo do programa selecione corretamente as operações disponíveis.

#### Correspondência com a literatura

A modelagem adotada segue diretamente a abordagem apresentada em obras clássicas, como:

* Hopcroft & Ullman, *Introduction to Automata Theory, Languages, and Computation*
* Sipser, *Introduction to the Theory of Computation*

A representação explícita dos conjuntos e da função de transição facilita a implementação dos algoritmos de conversão, como a construção dos subconjuntos, remoção de ε-transições e minimização de autômatos determinísticos.

</details>

###
<details> 
  <summary>
    <b style='font-size: 16px'> 📎 io.pas </b>
  </summary> 


Este módulo é responsável pela **leitura e escrita de autômatos em formato JSON**, atuando como uma **camada de entrada/saída** entre a representação externa do autômato e a representação formal interna utilizada pelo programa.

Nenhum algoritmo de Teoria de Linguagens Formais é implementado neste módulo. Ele se limita a converter dados entre formatos, preservando integralmente a estrutura e a semântica do autômato descrito.

#### Função no projeto

O [`io.pas`](src/core/io.pas) atua como:

* leitor de autômatos descritos em JSON;
* conversor entre uma estrutura de dados dinâmica e a representação formal interna;
* escritor de autômatos resultantes em formato JSON.

Esse módulo permite que os algoritmos de conversão operem exclusivamente sobre estruturas formais, desacopladas do formato de entrada.


#### Estrutura intermediária (`TAutomatonData`)

A leitura do JSON é feita inicialmente para uma estrutura intermediária, que utiliza **arrays dinâmicos**, facilitando a interpretação flexível dos dados de entrada.

Essa estrutura representa diretamente os elementos da definição formal de um autômato finito:

* **Estados (`States`)** → conjunto ( Q )
* **Alfabeto (`Alphabet`)** → conjunto ( \Sigma )
* **Estados iniciais (`InitialState`)** → conjunto ( I )
* **Estados finais (`FinalStates`)** → conjunto ( F )
* **Transições (`Transitions`)** → função de transição ( \delta )

O campo `InitialState` é tratado como um **array**, permitindo representar tanto autômatos com um único estado inicial quanto autômatos **multi-iniciais**, o que é fundamental para suportar AFNs e AFNs-ε.

#### Conversão para a representação formal

Após a leitura, os dados são convertidos para a estrutura [`TAutomaton`](src/core/automaton.pas) por meio de uma função específica de conversão.

Nessa etapa:

* os conjuntos são copiados para **arrays estáticos com contadores explícitos**;
* a função de transição é representada por uma lista explícita de transições;
* nenhuma inferência, normalização ou conversão teórica é realizada.

A responsabilidade desse módulo é apenas **instanciar concretamente** o autômato descrito, deixando qualquer transformação formal para os módulos de conversão.


#### Escrita do autômato em JSON

O módulo também permite salvar o autômato atual em formato JSON, refletindo:

* o alfabeto,
* o conjunto de estados,
* o conjunto de estados iniciais,
* o conjunto de estados finais,
* e a lista de transições.

O estado inicial é sempre escrito como um **conjunto**, mesmo quando unitário, mantendo consistência com a representação geral adotada no projeto.
</details>


###
<details> 
  <summary>
    <b style='font-size: 16px'> 📎 utils.pas </b>
  </summary> 

Este módulo implementa **funções auxiliares de análise formal de autômatos**, sendo responsável por **classificar o tipo do autômato** e por verificar propriedades fundamentais utilizadas no fluxo de conversão do projeto.

Os algoritmos aqui implementados não realizam transformações estruturais no autômato, mas avaliam suas características formais de acordo com definições clássicas da Teoria de Linguagens Formais.


#### Função no projeto

O [`utils.pas`](src/core/utils.pas) atua como:

* classificador do tipo do autômato (AFN, AFN-ε, AFD, AFD mínimo);
* verificador de propriedades formais;
* módulo de apoio à tomada de decisão no fluxo principal do programa;
* ferramenta de inspeção e depuração do autômato atual.

As informações produzidas por este módulo determinam **quais conversões são teoricamente válidas** em cada etapa da execução.


#### Propriedades verificadas

O módulo implementa as seguintes verificações:

##### **Presença de ε-transições**

A função [`ContainsEpsilon`](src/core/utils.pas#L20) identifica transições rotuladas com ε (incluindo representações como `'ε'`, `'λ'` ou string vazia), caracterizando um **AFN-ε**.

📚 Fundamentação: definição clássica de autômatos com transições vazias.


##### **Determinismo**

A função [`IsDeterministic`](src/core/utils.pas#L40) verifica se o autômato satisfaz as condições de um **AFD**, exigindo:

* exatamente um estado inicial;
* ausência de transições ε;
* no máximo uma transição para cada par (estado, símbolo).

Essa verificação corresponde diretamente à definição formal da função de transição:

$$
\delta : Q \times \Sigma \rightarrow Q
$$

##### **Minimalidade de AFD**

A função [`IsMinimizedAFD`](src/core/utils.pas#L86) verifica se um AFD é **mínimo**, utilizando dois critérios clássicos:

1. **Inexistência de estados inalcançáveis**, verificada por meio de uma busca em largura (BFS);
2. **Inexistência de estados equivalentes**, verificada pelo **algoritmo da tabela de distinguibilidade** (*table-filling algorithm*).

O algoritmo marca pares de estados distinguíveis com base em:

* diferença entre estados finais e não-finais;
* comportamento distinto sob os símbolos do alfabeto;
* propagação das distinções até a estabilização da tabela.

Esse método é canônico na literatura e corresponde ao procedimento clássico de minimização de autômatos determinísticos.

> Observação: o algoritmo considera AFDs possivelmente incompletos, ou seja, sem estado poço explícito.

#### **Identificação e remoção de Estados Inalcançáveis**

A procedure [`RemoveUnreachableStates`](https://www.google.com/search?q=src/core/utils.pas) realiza uma **limpeza estrutural** (sanitização) no autômato. Utilizando o algoritmo de **Busca em Largura (BFS)** a partir do(s) estado(s) inicial(is), o sistema:

1.  Mapeia todos os estados acessíveis através de caminhos válidos (grafo conexo);
2.  Identifica estados isolados ("código morto") que nunca seriam utilizados no processamento de cadeias;
3.  **Remove fisicamente** esses estados e suas respectivas transições da estrutura de dados.

Essa etapa é pré-requisito para a verificação de minimalidade, garantindo que o autômato não contenha "gordura" estrutural antes de ser processado.

#### Classificação do autômato

A função [`ClassifyAutomaton`](https://www.google.com/search?q=src/core/utils.pas%23L338) orquestra a análise do autômato. Antes de verificar os tipos, ela **executa automaticamente a remoção de estados inalcançáveis**, garantindo a integridade da estrutura. Em seguida, determina a classificação na hierarquia:

1.  multi-inicial;
2.  AFN-ε;
3.  AFD ou AFD mínimo;
4.  AFN.

Essa classificação reflete diretamente as **relações de generalização e conversão** estudadas na teoria de autômatos.


Essa classificação reflete diretamente as **relações de generalização e conversão** estudadas na teoria de autômatos.

#### Mostrar Autômato Atual
A função [`ShowAutomatonDetails`](src/core/utils.pas#L290) é uma função auxiliar do projeto para mostrar ao usuário o autômato atual.


</details>

</details>


### 
<details> 
  <summary>
    <b style='font-size: 18px'> 📂 Conversions </b>
  </summary> 


###
<details> 
  <summary>
    <b style='font-size: 16px'> 🖇️ afne_conversion.pas </b>
  </summary> 

Este módulo implementa a **conversão de autômatos multi-iniciais para autômatos com transições ε (AFN-ε)**, normalizando a estrutura do autômato para que ele possua **exatamente um estado inicial**.

A conversão preserva a linguagem reconhecida pelo autômato e constitui um passo fundamental no encadeamento clássico de conversões estudado na Teoria de Linguagens Formais.


#### Função no projeto

O [`afne_conversion.pas`](src/conversions/afne_conversion.pas) atua como:

* normalizador estrutural de autômatos multi-iniciais;
* etapa preparatória para conversões posteriores;
* garantidor da existência de um único estado inicial;
* facilitador da aplicação de algoritmos clássicos sobre AFNs.

Esse módulo é acionado automaticamente sempre que um autômato multi-inicial precisa ser convertido para um modelo canônico.


#### Método teórico aplicado

Dado um autômato com múltiplos estados iniciais:

$$
I = {q_1, q_2, \dots, q_n}
$$

o algoritmo aplica o procedimento clássico:

1. Cria um novo estado inicial ( $q_0$ );
2. Adiciona transições ε de ( $q_0$ ) para cada ( $q_i \in I $);
3. Define o conjunto de estados iniciais como ( $I = {q_0}$ ).

Formalmente, a nova função de transição satisfaz:

$$
\delta(q_0, \varepsilon) = I
$$

Esse procedimento garante que o novo autômato reconheça **exatamente a mesma linguagem** do autômato original.


#### Detalhes de implementação

A implementação cuida de aspectos práticos relevantes:

* prevenção de conflitos de nomes ao criar o novo estado inicial;
* renomeação consistente de estados em todas as estruturas internas;
* verificação de limites de memória (arrays estáticos);
* reclassificação automática do autômato após a conversão.

Esses cuidados garantem a integridade estrutural do autômato ao longo de todo o fluxo do programa.

</details>



###
<details> 
  <summary>
    <b style='font-size: 16px'> 🖇️ afne_afn_conversion.pas </b>
  </summary> 



</details>



###
<details> 
  <summary>
    <b style='font-size: 16px'> 🖇️ afn_afd_conversion.pas </b>
  </summary> 



</details>


###
<details> 
  <summary>
    <b style='font-size: 16px'> 🖇️ afd_minimization.pas </b>
  </summary> 



</details>



</details>


### 
<details> 
  <summary>
    <b style='font-size: 18px'> 📂 Test </b>
  </summary> 

###
<details> 
  <summary>
    <b style='font-size: 16px'> 🔎 words_test.pas </b>
  </summary> 



</details>


</details>


</details>









## 📖 Resumo das Conversões

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


[pascal-badge]: https://img.shields.io/badge/pascal-376aa8.svg?style=for-the-badge&logo=javafx&logoColor=white
[pascal-url]: https://www.freepascal.org/

[vscode-badge]: https://img.shields.io/badge/Visual%20Studio%20Code-0078d7.svg?style=for-the-badge&logo=visual-studio-code&logoColor=white
[vscode-url]: https://code.visualstudio.com/

[make-badge]: https://img.shields.io/badge/_-MAKEFILE-427819.svg?style=for-the-badge
[make-url]: https://www.gnu.org/software/make/manual/make.html

[linux-badge]: https://img.shields.io/badge/Linux-E34F26?logo=linux&logoColor=black&style=for-the-badge
[Linux-url]: https://www.kernel.org/

[windows-badge]: https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white
[Windows-url]:  https://www.microsoft.com/windows

[macos-badge]: https://img.shields.io/badge/macOS-000000?style=for-the-badge&logo=apple&logoColor=white
[macos-url]: https://www.apple.com/macos/
