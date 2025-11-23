unit words_test;

interface

uses
  SysUtils, Classes, automaton;

procedure TestWords(var A: TAutomaton);

implementation

type
  TStringArray = array of AnsiString;

function ContainsString(const Arr: TStringArray; const S: AnsiString): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(Arr) do
    if Arr[i] = S then
    begin
      ContainsString := True;
      Exit;
    end;
  ContainsString := False;
end;

function IndexInArray(const Arr: TStringArray; const S: AnsiString): Integer;
var
  i: Integer;
begin
  for i := 0 to High(Arr) do
    if Arr[i] = S then
    begin
      IndexInArray := i;
      Exit;
    end;
  IndexInArray := -1;
end;

function UniqueAdd(var Arr: TStringArray; const S: AnsiString): Integer;
var
  idx: Integer;
begin
  idx := IndexInArray(Arr, S);
  if idx >= 0 then
  begin
    UniqueAdd := idx;
    Exit;
  end;

  idx := Length(Arr);
  SetLength(Arr, idx + 1);
  Arr[idx] := S;

  UniqueAdd := idx;
end;

function IsWordAccepted(var A: TAutomaton; const word: AnsiString; var reason: AnsiString): Boolean;
var
  currentStates, nextStates: TStringArray;
  i, j, t: Integer;
  ch: AnsiString;
begin
  reason := '';

  SetLength(currentStates, Length(A.initialState));
  for i := 0 to High(A.initialState) do
    currentStates[i] := A.initialState[i];

  if word = '' then
  begin
    for i := 0 to High(currentStates) do
      if ContainsString(A.finalStates, currentStates[i]) then
      begin
        IsWordAccepted := True;
        Exit;
      end;

    reason := 'Palavra vazia nao aceita: estado inicial nao eh final';
    IsWordAccepted := False;
    Exit;
  end;

  for i := 1 to Length(word) do
  begin
    ch := word[i];
    SetLength(nextStates, 0);

    for j := 0 to High(currentStates) do
    begin
      for t := 0 to High(A.transitions) do
      begin
        if (A.transitions[t].source = currentStates[j]) and
           (A.transitions[t].symbol = ch) then
        begin
          UniqueAdd(nextStates, A.transitions[t].target);
        end;
      end;
    end;

    if Length(nextStates) = 0 then
    begin
      reason := 'Nenhuma transicao encontrada para [' + ch + ']';
      IsWordAccepted := False;
      Exit;
    end;

    currentStates := nextStates;
  end;

  for i := 0 to High(currentStates) do
    if ContainsString(A.finalStates, currentStates[i]) then
    begin
      IsWordAccepted := True;
      Exit;
    end;

  reason := 'Palavra terminou em estado nao-final';
  IsWordAccepted := False;
end;

procedure TestWords(var A: TAutomaton);
var
  word: AnsiString;
  accepted: Boolean;
  reason: AnsiString;
  i: Integer;
begin
  writeln;
  writeln('========== TESTADOR DE PALAVRAS ==========');

  write('Alfabeto: { ');
  for i := 0 to High(A.alphabet) do
  begin
    if i > 0 then write(', ');
    write(A.alphabet[i]);
  end;
  writeln(' }');

  write('Estados: { ');
  for i := 0 to High(A.states) do
  begin
    if i > 0 then write(', ');
    write(A.states[i]);
  end;
  writeln(' }');

  write('Estado(s) Inicial(is): { ');
  for i := 0 to High(A.initialState) do
  begin
    if i > 0 then write(', ');
    write(A.initialState[i]);
  end;
  writeln(' }');

  write('Estado(s) Final(is): { ');

    for i := 0 to High(A.finalStates) do
  begin
    if i > 0 then write(', ');
    write(A.finalStates[i]);
  end;
  writeln(' }');

  writeln('Transicoes:');
  for i := 0 to High(A.transitions) do
    writeln('  ', A.transitions[i].source, ' --[', A.transitions[i].symbol, ']--> ', A.transitions[i].target);

  writeln;
  writeln('Digite "sair" para retornar ao menu.');

  while True do
  begin
    write('Digite a palavra: ');
    readln(word);
    if word = 'sair' then Exit;

    accepted := IsWordAccepted(A, word, reason);

    if accepted then
      writeln('>> Palavra ACEITA')
    else
      writeln('>> Palavra REJEITADA: ', reason);

    writeln;
  end;
end;

end.

