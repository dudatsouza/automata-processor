unit automaton;

interface

type
  // As transições usam AnsiString explicitamente
  TTransition = record
    source: AnsiString;
    target: AnsiString;
    symbol: AnsiString;
  end;

  // Todas as listas também precisam ser array of AnsiString
  TAutomaton = record
    alphabet: array of AnsiString;
    states: array of AnsiString;
    finalStates: array of AnsiString;
    initialState: array of AnsiString;
    transitions: array of TTransition;
    classification: AnsiString;
  end;

implementation

end.
