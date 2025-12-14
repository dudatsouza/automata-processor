unit afd_minimization;

interface

uses
  sysutils, automaton, utils, io;

procedure MinimizeAFD(var A: TAutomaton);

implementation

// =============================================================
// FUNÇÕES AUXILIARES DE BUSCA E VERIFICAÇÃO
// =============================================================

function IndexOfState(const A: TAutomaton; const state: String): Integer;
var
  i: Integer;
begin
  IndexOfState := -1;
  for i := 0 to A.countStates - 1 do
    if A.states[i] = state then
    begin
      IndexOfState := i;
      Exit;
    end;
end;

function IsFinal(const A: TAutomaton; const state: String): Boolean;
var
  i: Integer;
begin
  IsFinal := False;
  for i := 0 to A.countFinal - 1 do
    if A.finalStates[i] = state then
    begin
      IsFinal := True;
      Exit;
    end;
end;

function HasTransition(const A: TAutomaton; const state, symbol: String): Boolean;
var
  i: Integer;
begin
  HasTransition := False;
  for i := 0 to A.countTransitions - 1 do
    if (A.transitions[i].source = state) and
       (A.transitions[i].symbol = symbol) then
    begin
      HasTransition := True;
      Exit;
    end;
end;

function GetTarget(const A: TAutomaton; const state, symbol: String): String;
var
  i: Integer;
begin
  GetTarget := '';
  for i := 0 to A.countTransitions - 1 do
    if (A.transitions[i].source = state) and (A.transitions[i].symbol = symbol) then
    begin
      GetTarget := A.transitions[i].target;
      Exit;
    end;
end;

// =============================================================
// PROCEDIMENTOS DE MANIPULAÇÃO DO AUTÔMATO
// =============================================================

procedure AddTransition(var A: TAutomaton; const source, symbol, target: String);
begin
  if A.countTransitions >= MAX_TRANSITIONS then
  begin
    writeln('ERRO: Limite de transicoes atingido em AddTransition.');
    Exit;
  end;

  A.transitions[A.countTransitions].source := source;
  A.transitions[A.countTransitions].symbol := symbol;
  A.transitions[A.countTransitions].target := target;
  Inc(A.countTransitions);
end;

procedure CompleteAFD(var A: TAutomaton);
var
  i, s: Integer;
  needSink: Boolean;
  sinkState: String;
begin
  needSink := False;
  sinkState := 'ERRO'; // Nome do estado poço

  // 1. Verifica se há transições ausentes
  for i := 0 to A.countStates - 1 do
  begin
    for s := 0 to A.countAlphabet - 1 do
    begin
      if not HasTransition(A, A.states[i], A.alphabet[s]) then
      begin
        needSink := True;
        Break;
      end;
    end;
    if needSink then Break;
  end;

  // 2. Se precisar, cria o estado poço e preenche buracos
  if needSink then
  begin
    // Verifica se já existe um estado com esse nome para não duplicar
    if IndexOfState(A, sinkState) = -1 then
    begin
      if A.countStates >= MAX_STATES then
      begin
        writeln('ERRO: Limite de estados atingido ao criar estado poco.');
        Exit;
      end;
      A.states[A.countStates] := sinkState;
      Inc(A.countStates);
    end;

    // Adiciona transições faltantes para o poço
    // Nota: Iteramos até countStates original ou incluindo o novo? 
    // O poço também precisa ter transições para ele mesmo.
    // O loop abaixo cobre todos os estados atuais (incluindo o recém adicionado se o indice atualizou)
    
    // Para segurança, iteramos sobre o countStates atualizado
    for i := 0 to A.countStates - 1 do
    begin
      for s := 0 to A.countAlphabet - 1 do
      begin
        if not HasTransition(A, A.states[i], A.alphabet[s]) then
          AddTransition(A, A.states[i], A.alphabet[s], sinkState);
      end;
    end;
  end;
end;

procedure AddUniqueTransitionToNew(var A2: TAutomaton; const src, sym, tgt: String);
var
  k: Integer;
  exists: Boolean;
begin
  exists := False;
  for k := 0 to A2.countTransitions - 1 do
    if (A2.transitions[k].source = src) and 
       (A2.transitions[k].symbol = sym) then
    begin
      // Se já existe (determinístico), apenas atualiza (ou ignora se for igual)
      A2.transitions[k].target := tgt;
      exists := True;
      Break;
    end;

  if not exists then
    AddTransition(A2, src, sym, tgt);
end;

function ContainsStr(const arr: array of String; count: Integer; const x: String): Boolean;
var
  k: Integer;
begin
  ContainsStr := False;
  for k := 0 to count - 1 do
    if arr[k] = x then
    begin
      ContainsStr := True;
      Exit;
    end;
end;

// =============================================================
// ALGORITMO PRINCIPAL DE MINIMIZAÇÃO
// =============================================================

procedure MinimizeAFD(var A: TAutomaton);
var
  n, i, j, s: Integer;
  // Estruturas Estáticas Locais
  Table: array[0..MAX_STATES, 0..MAX_STATES] of Boolean;
  Parent: array[0..MAX_STATES] of Integer;
  RepName: array[0..MAX_STATES] of String;
  MapToRep: array[0..MAX_STATES] of String;
  
  changed: Boolean;
  t1, t2: String;
  idx1, idx2: Integer;
  
  newA: TAutomaton; // Autômato temporário para reconstrução

  // Funções aninhadas para Union-Find acessando variáveis locais
  function Find(x: Integer): Integer;
  begin
    if Parent[x] <> x then
      Parent[x] := Find(Parent[x]);
    Find := Parent[x];
  end;

  procedure UnionSet(a, b: Integer);
  var ra, rb: Integer;
  begin
    ra := Find(a);
    rb := Find(b);
    if ra <> rb then Parent[rb] := ra;
  end;

  function TableMarked(i1, i2: Integer): Boolean;
  begin
    TableMarked := Table[i1, i2] or Table[i2, i1];
  end;

begin
  // 0. Verificações
  ClassifyAutomaton(A);
  if A.classification = 'AFD-MINIMO' then
  begin
    writeln('>> O automato ja esta classificado como AFD-MINIMO. Operacao cancelada.');
    Exit;
  end;

  if A.classification <> 'AFD' then
  begin
    writeln('Erro: o automato precisa ser AFD antes da minimizacao');
    Exit;
  end;

  CompleteAFD(A);
  writeln('>> Minimizando automato AFD...');

  n := A.countStates;
  if n = 0 then Exit;

  // 1. Inicializar Tabela (False = Equivalentes / True = Distintos)
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      Table[i, j] := False;

  // 2. Marcar pares triviais (Final vs Não-Final)
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
    begin
      if IsFinal(A, A.states[i]) <> IsFinal(A, A.states[j]) then
        Table[i, j] := True;
    end;

  // 3. Loop principal de marcação
  repeat
    changed := False;
    for i := 0 to n - 1 do
    begin
      for j := 0 to n - 1 do
      begin
        // Se ainda são considerados equivalentes, tentamos distinguir
        if not Table[i, j] then
        begin
          // Para cada símbolo do alfabeto
          for s := 0 to A.countAlphabet - 1 do
          begin
            t1 := GetTarget(A, A.states[i], A.alphabet[s]);
            t2 := GetTarget(A, A.states[j], A.alphabet[s]);

            if (t1 <> '') and (t2 <> '') then
            begin
              idx1 := IndexOfState(A, t1);
              idx2 := IndexOfState(A, t2);

              if (idx1 <> -1) and (idx2 <> -1) then
              begin
                // Se os destinos são distintos, então a origem também é
                if Table[idx1, idx2] then
                begin
                  Table[i, j] := True;
                  changed := True;
                  Break; // Sai do loop de símbolos
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  until not changed;

  // 4. Fusão de Estados (Reconstrução)
  
  // 4.1 Inicializa Union-Find
  for i := 0 to n - 1 do Parent[i] := i;

  // 4.2 Une pares não marcados (equivalentes)
  for i := 0 to n - 1 do
    for j := i + 1 to n - 1 do
      if not TableMarked(i, j) then
        UnionSet(i, j);

  // 4.3 Define Representantes
  for i := 0 to n - 1 do RepName[i] := '';

  for i := 0 to n - 1 do
  begin
    j := Find(i); // Root
    // Se o root ainda não tem nome de representante, pega o nome do estado atual
    if RepName[j] = '' then
      RepName[j] := A.states[j]; 
  end;

  // Mapeia cada estado original para o nome do seu representante
  for i := 0 to n - 1 do
  begin
    j := Find(i);
    MapToRep[i] := RepName[j];
  end;

  // 4.4 Construir newA
  newA.countStates := 0;
  newA.countAlphabet := 0;
  newA.countFinal := 0;
  newA.countInitial := 0;
  newA.countTransitions := 0;

  // Copia alfabeto
  for s := 0 to A.countAlphabet - 1 do
  begin
    newA.alphabet[s] := A.alphabet[s];
    Inc(newA.countAlphabet);
  end;

  // Copia estados (apenas os representantes únicos)
  for i := 0 to n - 1 do
  begin
    if Find(i) = i then // É root
    begin
      // Adiciona se ainda não existe (segurança)
      if not ContainsStr(newA.states, newA.countStates, RepName[i]) then
      begin
        newA.states[newA.countStates] := RepName[i];
        Inc(newA.countStates);
      end;
    end;
  end;

  // Copia estados finais
  // Se um estado original era final, seu representante será final
  for i := 0 to n - 1 do
  begin
    if IsFinal(A, A.states[i]) then
    begin
      if not ContainsStr(newA.finalStates, newA.countFinal, MapToRep[i]) then
      begin
        newA.finalStates[newA.countFinal] := MapToRep[i];
        Inc(newA.countFinal);
      end;
    end;
  end;

  // Define estado inicial
  if A.countInitial > 0 then
  begin
    i := IndexOfState(A, A.initialState[0]);
    if i <> -1 then
    begin
      newA.initialState[0] := MapToRep[i];
      newA.countInitial := 1;
    end;
  end;

  // Cria transições do autômato minimizado
  // Para cada estado representante, olhamos suas transições originais
  for i := 0 to n - 1 do
  begin
    if Find(i) = i then // É root/representante
    begin
      // RepName[i] é o nome do estado no novo automato.
      // A.states[i] é o nome no antigo (que por acaso é igual ao repname neste ponto da logica)
      
      for s := 0 to A.countAlphabet - 1 do
      begin
        t1 := GetTarget(A, A.states[i], A.alphabet[s]); // Destino original
        if t1 <> '' then
        begin
          j := IndexOfState(A, t1);
          if j <> -1 then
          begin
            // Origem: Representante do i
            // Destino: Representante do j
            AddUniqueTransitionToNew(newA, RepName[i], A.alphabet[s], MapToRep[j]);
          end;
        end;
      end;
    end;
  end;

  // 4.5 Substituição final
  A := newA;
  ClassifyAutomaton(A);

  if A.classification = 'AFN' then
  begin
    writeln('>> Minimização de AFD executada com sucesso!');
    SaveAutomatonJSON('./data/output/AFD_MINIMO.json', A);
  end
  else
  begin
    writeln('!! [ERRO DE CLASSIFICACAO]: Conversao falhou ou foi classificada incorretamente como ', A.classification, '!!');
  end;

end;

end.