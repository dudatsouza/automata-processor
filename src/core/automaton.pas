unit automaton;

interface

const
  // Definimos limites máximos fixos (alocação estática)
  MAX_STATES = 500;
  MAX_ALPHABET = 100;
  MAX_TRANSITIONS = 5000;
  MAX_FINAL_STATES = 500;
  MAX_INITIAL_STATES = 50;

type
  // Registro de Transição
  TTransition = record
    source: String; 
    target: String;
    symbol: String;
  end;

  // Estrutura do Autômato usando Arrays Estáticos e Contadores, definição básica A=(Q,Σ,F,q0​,δ)
  TAutomaton = record
    // Alfabeto
    alphabet: array[0..MAX_ALPHABET] of String;
    countAlphabet: Integer;

    // Estados
    states: array[0..MAX_STATES] of String;
    countStates: Integer;

    // Estados Finais
    finalStates: array[0..MAX_FINAL_STATES] of String;
    countFinal: Integer;

    // Estado(s) Inicial(is)
    initialState: array[0..MAX_INITIAL_STATES] of String;
    countInitial: Integer;

    // Transições
    transitions: array[0..MAX_TRANSITIONS] of TTransition;
    countTransitions: Integer;

    // Classificação
    classification: String;
  end;

implementation

end.