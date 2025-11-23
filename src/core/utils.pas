unit utils;

interface

uses
  SysUtils, automaton;

procedure ClassifyAutomaton(var A: TAutomaton);
function ContainsEpsilon(var A: TAutomaton): Boolean;
function IsDeterministic(var A: TAutomaton): Boolean;

implementation

function ContainsEpsilon(var A: TAutomaton): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(A.transitions) do
    if (A.transitions[i].symbol = 'ε') or (A.transitions[i].symbol = '') then
    begin
      ContainsEpsilon := True;
      Exit;
    end;
  ContainsEpsilon := False;
end;

function IsDeterministic(var A: TAutomaton): Boolean;
var
  i, j, count: Integer;
begin
  for i := 0 to High(A.transitions) do
  begin
    if A.transitions[i].symbol = 'ε' then
      Continue;

    count := 0;

    for j := 0 to High(A.transitions) do
      if (A.transitions[j].source = A.transitions[i].source) and
         (A.transitions[j].symbol = A.transitions[i].symbol) then
        Inc(count);

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
  if Length(A.initialState) > 1 then
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
