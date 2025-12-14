unit utils;

interface

uses
  automaton;

procedure ClassifyAutomaton(var A: TAutomaton);
function ContainsEpsilon(var A: TAutomaton): Boolean;
function IsDeterministic(var A: TAutomaton): Boolean;

implementation

function ContainsEpsilon(var A: TAutomaton): Boolean;
var
  i: Integer;
begin
  ContainsEpsilon := False; // Pascal Puro: atribui ao nome da função

  // Loop usa o contador manual, não Length()
  for i := 0 to A.countTransitions - 1 do
    if A.transitions[i].symbol = 'ε' then
    begin
      ContainsEpsilon := True;
      Exit;
    end;
end;

function IsDeterministic(var A: TAutomaton): Boolean;
var
  i, j, count: Integer;
begin
  // Pré-condição: Verifica contador de estados iniciais
  if A.countInitial <> 1 then
  begin
    IsDeterministic := False;
    Exit;
  end;

  // Percorre todas as transições para validar regras de AFD
  for i := 0 to A.countTransitions - 1 do
  begin
    // 1. AFD não admite epsilon
    if A.transitions[i].symbol = 'ε' then
    begin
      IsDeterministic := False;
      Exit;
    end;

    // 2. Verifica se há ambiguidade (mesma origem, mesmo símbolo, destinos diferentes)
    count := 0;
    for j := 0 to A.countTransitions - 1 do
    begin
      if (A.transitions[j].source = A.transitions[i].source) and
         (A.transitions[j].symbol = A.transitions[i].symbol) then
      begin
        Inc(count);
      end;
    end;

    // Se houver mais de 1 transição com mesmo símbolo saindo do mesmo estado
    if count > 1 then
    begin
      IsDeterministic := False;
      Exit;
    end;
  end;

  IsDeterministic := True;
end;

procedure ClassifyAutomaton(var A: TAutomaton);
begin
  // Verifica contador de iniciais
  if A.countInitial > 1 then
  begin
    A.classification := 'MULTI-INICIAL';
    Exit;
  end;

  if ContainsEpsilon(A) then
  begin
    A.classification := 'AFN-E';
    Exit;
  end;

  if IsDeterministic(A) then
    A.classification := 'AFD'
  else
    A.classification := 'AFN';
end;

end.