unit utils;

interface

uses
  automaton, Crt;

var
  AutomatonObj: TAutomaton;

procedure ClassifyAutomaton(var A: TAutomaton);
function ContainsEpsilon(var A: TAutomaton): Boolean;
function IsDeterministic(var A: TAutomaton): Boolean;
function IsMinimizedAFD(var A: TAutomaton): Boolean;
procedure ShowAutomatonDetails(var A: TAutomaton);

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

function IsMinimizedAFD(var A: TAutomaton): Boolean;
var
  i, j, k, idxTarget1, idxTarget2: Integer;
  Reachable: array[0..MAX_STATES] of Boolean;
  Queue: array[0..MAX_STATES] of Integer;
  qStart, qEnd, currIdx, targetIdx: Integer;
  
  // Tabela de Distinguibilidade (True = Distintos, False = Equivalentes)
  Distinguishable: array[0..MAX_STATES, 0..MAX_STATES] of Boolean;
  changed: Boolean;
  t1, t2: String;

  // Função auxiliar interna para pegar índice pelo nome
  function GetStateIndex(name: String): Integer;
  var r: Integer;
  begin
    GetStateIndex := -1;
    for r := 0 to A.countStates - 1 do
      if A.states[r] = name then
      begin
        GetStateIndex := r;
        Exit;
      end;
  end;

  // Função auxiliar para pegar destino
  function GetTarget(srcIdx: Integer; sym: String): String;
  var tr: Integer;
  begin
    GetTarget := '';
    for tr := 0 to A.countTransitions - 1 do
      if (A.transitions[tr].source = A.states[srcIdx]) and
         (A.transitions[tr].symbol = sym) then
      begin
        GetTarget := A.transitions[tr].target;
        Exit;
      end;
  end;

  function IsFinal(idx: Integer): Boolean;
  var f: Integer;
  begin
    IsFinal := False;
    for f := 0 to A.countFinal - 1 do
      if A.finalStates[f] = A.states[idx] then
      begin
        IsFinal := True;
        Exit;
      end;
  end;

begin
  // 0. Pré-requisito: Deve ser determinístico
  if not IsDeterministic(A) then
  begin
    IsMinimizedAFD := False;
    Exit;
  end;

  if A.countStates = 0 then
  begin
    IsMinimizedAFD := True;
    Exit;
  end;

  // --- PASSO 1: VERIFICAR ESTADOS INACESSÍVEIS (BFS) ---
  for i := 0 to A.countStates - 1 do Reachable[i] := False;
  
  qStart := 0; 
  qEnd := 0;
  
  // Adiciona estado inicial na fila
  if A.countInitial > 0 then
  begin
    currIdx := GetStateIndex(A.initialState[0]);
    if currIdx <> -1 then
    begin
      Reachable[currIdx] := True;
      Queue[qEnd] := currIdx;
      Inc(qEnd);
    end;
  end;

  while qStart < qEnd do
  begin
    currIdx := Queue[qStart];
    Inc(qStart);

    // Verifica todos os vizinhos
    for i := 0 to A.countTransitions - 1 do
    begin
      if A.transitions[i].source = A.states[currIdx] then
      begin
        targetIdx := GetStateIndex(A.transitions[i].target);
        if (targetIdx <> -1) and (not Reachable[targetIdx]) then
        begin
          Reachable[targetIdx] := True;
          Queue[qEnd] := targetIdx;
          Inc(qEnd);
        end;
      end;
    end;
  end;

  // Se a quantidade de visitados for menor que o total, existem inalcançáveis -> Não é mínimo
  for i := 0 to A.countStates - 1 do
    if not Reachable[i] then
    begin
      IsMinimizedAFD := False;
      Exit;
    end;

  // --- PASSO 2: VERIFICAR ESTADOS EQUIVALENTES (Tabela) ---
  
  // Inicializa tabela: Tudo False (assume equivalentes a princípio)
  for i := 0 to A.countStates - 1 do
    for j := 0 to A.countStates - 1 do
      Distinguishable[i, j] := False;

  // Marca pares triviais: (Final) vs (Não-Final)
  for i := 0 to A.countStates - 1 do
    for j := 0 to A.countStates - 1 do
    begin
      if IsFinal(i) <> IsFinal(j) then
        Distinguishable[i, j] := True;
    end;

  // Loop de marcação até estabilizar
  repeat
    changed := False;
    for i := 0 to A.countStates - 1 do
    begin
      for j := 0 to A.countStates - 1 do
      begin
        // Se ainda são considerados equivalentes (False), tenta distinguir
        if (i <> j) and (not Distinguishable[i, j]) then
        begin
          for k := 0 to A.countAlphabet - 1 do
          begin
            t1 := GetTarget(i, A.alphabet[k]);
            t2 := GetTarget(j, A.alphabet[k]);
            
            // Se ambos vão para algum lugar
            if (t1 <> '') and (t2 <> '') then
            begin
              idxTarget1 := GetStateIndex(t1);
              idxTarget2 := GetStateIndex(t2);
              
              if (idxTarget1 <> -1) and (idxTarget2 <> -1) then
              begin
                // Se os destinos são distinguíveis, então as origens também são
                if Distinguishable[idxTarget1, idxTarget2] then
                begin
                  Distinguishable[i, j] := True;
                  changed := True;
                  Break;
                end;
              end;
            end
            // Se um tem transição e o outro não para o mesmo símbolo -> Distintos
            else if (t1 <> '') <> (t2 <> '') then
            begin
                Distinguishable[i, j] := True;
                changed := True;
                Break;
            end;
          end;
        end;
      end;
    end;
  until not changed;

  // Verificação Final: Se existir algum par (i, j) com i != j que NÃO seja distinguível,
  // significa que eles são equivalentes e poderiam ser fundidos. Logo, não é mínimo.
  for i := 0 to A.countStates - 1 do
    for j := i + 1 to A.countStates - 1 do // Olha apenas triângulo superior
    begin
      if not Distinguishable[i, j] then
      begin
        IsMinimizedAFD := False; // Encontrou par equivalente
        Exit;
      end;
    end;

  IsMinimizedAFD := True;
end;

procedure ShowAutomatonDetails(var A: TAutomaton);
var
  i: Integer;
begin
  writeln;
  writeln('--- DETALHES DO AUTÔMATO ATUAL - (', A.classification, ') ---');

  write('Alfabeto: { ');
  for i := 0 to A.countAlphabet - 1 do
  begin
    if i > 0 then write(', ');
    write(A.alphabet[i]);
  end;
  writeln(' }');

  write('Estados: { ');
  for i := 0 to A.countStates - 1 do
  begin
    if i > 0 then write(', ');
    write(A.states[i]);
  end;
  writeln(' }');

  write('Estado(s) Inicial(is): { ');
  for i := 0 to A.countInitial - 1 do
  begin
    if i > 0 then write(', ');
    write(A.initialState[i]);
  end;
  writeln(' }');

  write('Estado(s) Final(is): { ');
  for i := 0 to A.countFinal - 1 do
  begin
    if i > 0 then write(', ');
    write(A.finalStates[i]);
  end;
  writeln(' }');

  writeln('Transicoes (Total: ', A.countTransitions, '):');
  for i := 0 to A.countTransitions - 1 do
    writeln('  ', A.transitions[i].source, ' --[', A.transitions[i].symbol, ']--> ', A.transitions[i].target);

  writeln('------------------------------------------------');
  writeln;
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

  // 3. Verifica Determinismo
  if IsDeterministic(A) then
  begin
    // Se for AFD, agora fazemos a Verificação Rigorosa de Minimização
    if IsMinimizedAFD(A) then
      A.classification := 'AFD-MINIMO'
    else
      A.classification := 'AFD';
  end
  else
  begin
    A.classification := 'AFN';
  end;
end;

end.