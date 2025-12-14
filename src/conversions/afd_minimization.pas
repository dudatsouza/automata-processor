unit afd_minimization;

interface

uses
  automaton, io, utils;

procedure MinimizeAFD(var A: TAutomaton);

implementation

type
  TDistinctionTable = array of array of Boolean;



// -------------------------------------------------------------
// Função auxiliar: vai verificar se tem transições, pois para minimizar tem que ser um afd completo
// -------------------------------------------------------------
function HasTransition(const A: TAutomaton; const state, symbol: AnsiString): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(A.transitions) do
    if (A.transitions[i].source = state) and
       (A.transitions[i].symbol = symbol) then
    begin
      HasTransition := True;
      Exit;
    end;

  HasTransition := False;
end;

procedure AddTransition(var A: TAutomaton; const source, symbol, target: AnsiString);
begin
  SetLength(A.transitions, Length(A.transitions) + 1);
  A.transitions[High(A.transitions)].source := source;
  A.transitions[High(A.transitions)].symbol := symbol;
  A.transitions[High(A.transitions)].target := target;
end;


// -------------------------------------------------------------
// Função auxiliar: vai completar o AFD para ele ser completo e assim poder minimizar
// -------------------------------------------------------------
procedure CompleteAFD(var A: TAutomaton);
var
  i, s: Integer;
  needSink: Boolean;
  sinkState: AnsiString;
begin
  needSink := False;
  sinkState := 'D'; // Nome do estado poço

  // Verifica se há transições ausentes
  for i := 0 to High(A.states) do
  begin
    for s := 0 to High(A.alphabet) do
    begin
      if not HasTransition(A, A.states[i], A.alphabet[s]) then
      begin
        needSink := True;
        Break;  // Sai do loop se uma transição ausente for encontrada
      end;
    end;
    if needSink then Break;
  end;

  // Se precisar de estado poço, cria o estado e as transições faltantes
  if needSink then
  begin
    SetLength(A.states, Length(A.states) + 1);
    A.states[High(A.states)] := sinkState;  // Adiciona o estado poço

    // Adiciona as transições ausentes apontando para o estado poço
    for i := 0 to High(A.states) - 1 do
      for s := 0 to High(A.alphabet) do
        if not HasTransition(A, A.states[i], A.alphabet[s]) then
          AddTransition(A, A.states[i], A.alphabet[s], sinkState);

    // Adiciona as transições do estado poço para ele mesmo
    for s := 0 to High(A.alphabet) do
      AddTransition(A, sinkState, A.alphabet[s], sinkState);
  end;
end;




// -------------------------------------------------------------
// Função auxiliar: retorna o destino de uma transição
// -------------------------------------------------------------
function GetTarget(const A: TAutomaton; const state, symbol: AnsiString): AnsiString;
var
  i: Integer;
begin
  for i := 0 to High(A.transitions) do
    if (A.transitions[i].source = state) and (A.transitions[i].symbol = symbol) then
    begin
      GetTarget := A.transitions[i].target;
      Exit;
    end;

  // Se não houver transição, retorna string vazia
  GetTarget := '';
end;

// -------------------------------------------------------------
// Função auxiliar: verifica se um estado está nos finais
// -------------------------------------------------------------
function IsFinal(const A: TAutomaton; const state: AnsiString): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(A.finalStates) do
    if A.finalStates[i] = state then
    begin
      IsFinal := True;
      Exit;
    end;

  IsFinal := False;
end;

// -------------------------------------------------------------
// Função auxiliar: retorna índice de estado
// -------------------------------------------------------------
function IndexOfState(const A: TAutomaton; const state: AnsiString): Integer;
var
  i: Integer;
begin
  for i := 0 to High(A.states) do
    if A.states[i] = state then
    begin
      IndexOfState := i;
      Exit;
    end;
  IndexOfState := -1;
end;

// -------------------------------------------------------------
// Imprime (Tabela de Distinção)
// -------------------------------------------------------------

procedure PrintTable(const table: TDistinctionTable; const A: TAutomaton);
var
  i, j: Integer;
begin
  Write('     ');
  for j := 0 to High(A.states) do
    Write(A.states[j]:5);
  Writeln;

  for i := 0 to High(table) do
  begin
    Write(A.states[i]:5);
    for j := 0 to High(table[i]) do
    begin
      if table[i][j] then
        Write('  1  ')
      else
        Write('  0  ');
    end;
    Writeln;
  end;
end;



// -------------------------------------------------------------
// Algoritmo de Minimização (Tabela de Distinção)
// -------------------------------------------------------------
procedure MinimizeAFD(var A: TAutomaton);

var
  n, i, j, s: Integer;
  table: array of array of Boolean;
  changed: Boolean;
  t1, t2: AnsiString;
  parent: array of Integer;
  repName: array of AnsiString; // nome do representante por índice (após achar root)
  mapToRep: array of AnsiString; // mapeia A.states[i] -> nome do representante
  newA: TAutomaton;


function Find(x: Integer): Integer;
  begin
    if parent[x] <> x then
      parent[x] := Find(parent[x]);
    Find := parent[x];
  end;

  procedure UnionSet(a, b: Integer);
  var ra, rb: Integer;
  begin
    ra := Find(a);
    rb := Find(b);
    if ra <> rb then parent[rb] := ra;
  end;

  function TableMarked(i1, i2: Integer): Boolean;
  begin
    TableMarked := table[i1][i2] or table[i2][i1];
  end;

  function ContainsStr(const arr: array of AnsiString; const x: AnsiString): Boolean;
  var k: Integer;
  begin
    for k := 0 to High(arr) do
      if arr[k] = x then begin
        ContainsStr := True;
        Exit;
      end;
    ContainsStr := False;
  end;

  procedure AddUniqueTransition(var A2: TAutomaton; const src, sym, tgt: AnsiString);
  var k: Integer;
  begin
    for k := 0 to High(A2.transitions) do
      if (A2.transitions[k].source = src) and (A2.transitions[k].symbol = sym) then
      begin
        A2.transitions[k].target := tgt;
        Exit;
      end;

    SetLength(A2.transitions, Length(A2.transitions) + 1);
    A2.transitions[High(A2.transitions)].source := src;
    A2.transitions[High(A2.transitions)].symbol := sym;
    A2.transitions[High(A2.transitions)].target := tgt;
  end;

 

begin

  //0. Verificações

  // verificar se ele é um AFD
  ClassifyAutomaton(A);

  // se não for AFD, interrompe a minimização
  if A.classification <> 'AFD' then

  begin
    writeln('Erro: o automato precisa ser AFD antes da minimização');
    Exit;
  end;

  //Completa o AFD caso não seja completo
  CompleteAFD(A);



  writeln('>> Minimizando automato AFD...');

  n := Length(A.states);
  if n = 0 then Exit;



  // -------------------------------------------------------------
  // 1. Criar tabela NxN de " Equivalentes" (False = equivalentes)
  // -------------------------------------------------------------
  SetLength(table, n);
  for i := 0 to n - 1 do
    begin
      SetLength(table[i], n);
      for j := 0 to n - 1 do
        table[i][j] := False;
    end;


  // -------------------------------------------------------------
  // 2. Marcar pares (final, não-final) como não equivalentes
  // -------------------------------------------------------------
  for i := 0 to n - 1 do
  for j := 0 to n - 1 do
    if not table[i][j] then
      if IsFinal(A, A.states[i]) <> IsFinal(A, A.states[j]) then
        table[i][j] := True;

  
  writeln('Tabela inicial (1 = ignorar / 0 = analisar):');
  PrintTable(table, A);


  // -------------------------------------------------------------
  // 3. Procura e marca os estados não equivalentes
  // -------------------------------------------------------------
  repeat
    changed := False;

    for i := 0 to n - 1 do
      for j := 0 to n - 1 do
      begin
        if not table[i][j] then
        begin
          // Verificar para cada símbolo
          for s := 0 to High(A.alphabet) do
          begin
            t1 := GetTarget(A, A.states[i], A.alphabet[s]);
            t2 := GetTarget(A, A.states[j], A.alphabet[s]);

            if (t1 <> '') and (t2 <> '') then
            begin
              if table[IndexOfState(A, t1)][IndexOfState(A, t2)] then
              begin
                table[i][j] := True;
                changed := True;
                Break;
              end;
            end;
          end;
        end;
      end;

  until not changed;

  printTable(table,A);

  // -------------------------------------------------------------
  // 4. Fusão de estados equivalentes (reconstruindo o AFD)
  //    - Cria classes de equivalência com Union-Find
  //    - Reconstrói states, finalStates, initialState e transitions
  // -------------------------------------------------------------
  // --------- 4.1 Inicializa Union-Find ----------
  SetLength(parent, n);
  for i := 0 to n - 1 do
    parent[i] := i;

  // --------- 4.2 Une pares equivalentes ----------
  // se NÃO está marcado na tabela => equivalente => union
  for i := 0 to n - 1 do
    for j := i + 1 to n - 1 do
      if not TableMarked(i, j) then
        UnionSet(i, j);

  // --------- 4.3 Define nome do representante e mapeamento old -> rep ----------
  SetLength(repName, n);
  SetLength(mapToRep, n);

  // primeiro: decide um nome para cada root (vamos usar o primeiro índice que aparecer como root)
  for i := 0 to n - 1 do
    repName[i] := ''; // vazio

  for i := 0 to n - 1 do
  begin
    j := Find(i); // root
    if repName[j] = '' then
      repName[j] := A.states[j]; // nome do representante = nome do estado no root
  end;

  // agora mapeia cada estado antigo para o nome do representante do seu root
  for i := 0 to n - 1 do
  begin
    j := Find(i);
    mapToRep[i] := repName[j];
  end;

  // --------- 4.4 Reconstrói o novo automato newA ----------
  // copia alfabeto
  SetLength(newA.alphabet, Length(A.alphabet));
  for s := 0 to High(A.alphabet) do
    newA.alphabet[s] := A.alphabet[s];

  // zera listas do newA
  SetLength(newA.states, 0);
  SetLength(newA.finalStates, 0);
  SetLength(newA.initialState, 0);
  SetLength(newA.transitions, 0);

  // states = só representantes
  for i := 0 to n - 1 do
  begin
    if Find(i) = i then // é root
      if not ContainsStr(newA.states, repName[i]) then
begin
  SetLength(newA.states, Length(newA.states) + 1);
  newA.states[High(newA.states)] := repName[i];
end;

  end;

  // finalStates: se qualquer membro era final, o representante vira final
  for i := 0 to n - 1 do
    if IsFinal(A, A.states[i]) then
      if not ContainsStr(newA.finalStates, mapToRep[i]) then
begin
  SetLength(newA.finalStates, Length(newA.finalStates) + 1);
  newA.finalStates[High(newA.finalStates)] := mapToRep[i];
end;


  // initialState (no seu tipo é array; estamos assumindo 1 estado inicial)
  if Length(A.initialState) > 0 then
  begin
    i := IndexOfState(A, A.initialState[0]);
    SetLength(newA.initialState, 1);
    if i <> -1 then
      newA.initialState[0] := mapToRep[i]
    else
      newA.initialState[0] := A.initialState[0]; // fallback
  end;

  // transitions: para cada representante e símbolo, cria transição usando o representante como base
  for i := 0 to n - 1 do
  begin
    if Find(i) = i then // root
    begin
      for s := 0 to High(A.alphabet) do
      begin
        t1 := GetTarget(A, A.states[i], A.alphabet[s]); // destino no A antigo
        if t1 <> '' then
        begin
          j := IndexOfState(A, t1);
          if j <> -1 then
            AddUniqueTransition(newA, repName[i], A.alphabet[s], mapToRep[j]);
        end;
      end;
    end;
  end;

  // --------- 4.5 Substitui A pelo newA ----------
  A := newA;

  A.classification := 'AFD-MINIMO';
  SaveAutomatonJSON('./data/output/AFD_MINIMO.json', A);

  writeln('>> Minimização concluída!');
end;

end.
