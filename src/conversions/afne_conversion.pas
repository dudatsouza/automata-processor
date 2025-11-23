unit afne_conversion;

interface

uses
  automaton, io;

procedure ConvertMultiInitialToAFNE(var A: TAutomaton);

implementation

procedure ConvertMultiInitialToAFNE(var A: TAutomaton);
var
  i, pos: Integer;
  newInitial: AnsiString;
  newTransitions: array of TTransition;
begin
  // Apenas converte se houver mais de 1 estado inicial
  if Length(A.initialState) <= 1 then
    Exit;

  writeln('>> Convertendo automato Multi-Inicial para AFN-E...');

  // Nome fixo para novo estado inicial unificado
  newInitial := 'q0';

  // Criar novo vetor de transições:
  // todas as transições antigas + ε-transições para cada estado inicial antigo
  SetLength(newTransitions, Length(A.transitions) + Length(A.initialState));

  // Copiar transições anteriores
  for i := 0 to High(A.transitions) do
    newTransitions[i] := A.transitions[i];

  // Criar transições ε do novo estado inicial para cada antigo
  pos := Length(A.transitions);
  for i := 0 to High(A.initialState) do
  begin
    newTransitions[pos].source := newInitial;
    newTransitions[pos].target := A.initialState[i];
    newTransitions[pos].symbol := 'ε';
    Inc(pos);
  end;

  // Substituir transições
  A.transitions := newTransitions;

  // Substituir estados iniciais por apenas o novo estado
  SetLength(A.initialState, 1);
  A.initialState[0] := newInitial;

  // Adicionar novo estado no conjunto
  SetLength(A.states, Length(A.states) + 1);
  A.states[High(A.states)] := newInitial;

  // Atualizar classificação
  A.classification := 'AFN-E';

  writeln('>> Conversao Multi-Inicial para AFN-E executada com sucesso!');
end;

end.
