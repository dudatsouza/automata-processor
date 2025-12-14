unit utils;

interface

uses
  automaton;

var
  AutomatonObj: TAutomaton; // Criando nosso automoto global

procedure ClassifyAutomaton(var A: TAutomaton);
function ContainsEpsilon(var A: TAutomaton): Boolean;
function IsDeterministic(var A: TAutomaton): Boolean;
function IsMinimizedAFD(var A: TAutomaton): Boolean;
procedure ShowAutomatonDetails(var A: TAutomaton);

implementation

// Verificação de vazio (ε, λ, '')
function ContainsEpsilon(var A: TAutomaton): Boolean;
var
  i: Integer;
begin
  ContainsEpsilon := False; 
  
  for i := 0 to A.countTransitions - 1 do
  begin
    // Verifica Epsilon ('ε'), String Vazia (''), ou Lambda ('λ')
    if (A.transitions[i].symbol = 'ε') or 
       (A.transitions[i].symbol = '') or 
       (A.transitions[i].symbol = 'λ') then
    begin
      ContainsEpsilon := True;
      Exit;
    end;
  end;
end;

// Verificação se é determinístico
function IsDeterministic(var A: TAutomaton): Boolean;
var
  i, j, count: Integer;
begin
  // Confirmar q não é um MULTI-INICIAL
  if A.countInitial <> 1 then
  begin
    IsDeterministic := False;
    Exit;
  end;

  // Percorre todas as transições para validar regras de AFD
  for i := 0 to A.countTransitions - 1 do
  begin
    // Confirmar q não tem vazio (ε, λ, '')
    if (A.transitions[i].symbol = 'ε') or 
       (A.transitions[i].symbol = '') or 
       (A.transitions[i].symbol = 'λ') then
    begin
      IsDeterministic := False;
      Exit;
    end;

    // Confirmar se há ambiguidade (mesma origem, mesmo símbolo, destinos diferentes)
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

// Verificação se é um AFD-MINIMO
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

  // Função auxiliar para ver se é estado final
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
  // Verificação se é determinístico
  if not IsDeterministic(A) then
  begin
    IsMinimizedAFD := False;
    Exit;
  end;

  // Se não tiver nenhum estado, é minimo
  if A.countStates = 0 then
  begin
    IsMinimizedAFD := True;
    Exit;
  end;

  // Verificar se existe algum estado q não é alcansável (usanmos aq BFS)
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

  // Análise principal - Verificar se há estados equivalentes (usamos aq a Tabela de Distinguibilidade)
  
  // Inicializa tabela: Tudo False (assume equivalentes a princípio)
  for i := 0 to A.countStates - 1 do
    for j := 0 to A.countStates - 1 do
      Distinguishable[i, j] := False;

  // Compara dois estados, vendo se os dois são finais ou não-finais, se forem diferentes, obviamente nao são iguais
  for i := 0 to A.countStates - 1 do
    for j := 0 to A.countStates - 1 do
    begin
      if IsFinal(i) <> IsFinal(j) then
        Distinguishable[i, j] := True;
    end;

  // Loop de marcação até estabilizar (enquanto estiver coisa diferente, continua verificando)
  repeat
    changed := False;

    // vamos comparar dois estados
    for i := 0 to A.countStates - 1 do
    begin
      for j := 0 to A.countStates - 1 do
      begin
        // Se ainda são considerados equivalentes (False), tenta distinguir
        if (i <> j) and (not Distinguishable[i, j]) then
        begin
          // Analisar comportamento de cada estado para cada letra do alfabeto, se eles tiverem o mesmo comportamento para a mesma letra, eles podem ser EQUIVALENTES (continuam 'False' na tabela).
          
          // Para cada letra do alfabeto, testamos se 'i' e 'j' se comportam igual
          for k := 0 to A.countAlphabet - 1 do
          begin
            // Vê para onde 'i' vai (t1) e para onde 'j' vai (t2) com a letra atual
            t1 := GetTarget(i, A.alphabet[k]);
            t2 := GetTarget(j, A.alphabet[k]);
            
            // CASO 1: Ambos os estados possuem caminho (transição)
            if (t1 <> '') and (t2 <> '') then
            begin
              idxTarget1 := GetStateIndex(t1);
              idxTarget2 := GetStateIndex(t2);
              
              if (idxTarget1 <> -1) and (idxTarget2 <> -1) then
              begin
                // A Lógica do "Dominó":
                // Se os destinos (filhos) já são sabidos como diferentes na tabela,
                // então as origens (pais 'i' e 'j') obrigatoriamente também são.
                if Distinguishable[idxTarget1, idxTarget2] then
                begin
                  Distinguishable[i, j] := True;
                  changed := True; // Avisa que mudou algo para repetir o ciclo
                  Break; // Já achamos uma diferença, não precisa testar outras letras
                end;
              end;
            end
            
            // CASO 2: Um tem transição e o outro não (Inconsistência Estrutural)
            // Se um "anda" e o outro "trava" com a mesma letra, eles são diferentes.
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

  // Conferindo para dar a resposta 
  
  // Varre a tabela procurando pares de estados distintos (i, j) que sobreviveram como "Iguais"
  for i := 0 to A.countStates - 1 do
    for j := i + 1 to A.countStates - 1 do // "j = i + 1" garante que não comparamos (0,0) nem repetimos (0,1) e (1,0)
    begin
      // Se a tabela ainda diz False, significa que não achamos NENHUMA diferença entre 'i' e 'j'.
      // Conclusão: Eles são estados equivalentes (gêmeos).
      if not Distinguishable[i, j] then
      begin
        IsMinimizedAFD := False; // Se tem estados gêmeos, o automato tem "gordura", logo não é mínimo.
        Exit;
      end;
    end;

  // Se o loop terminou sem achar nenhum par gêmeo, todos os estados são únicos e necessários.
  IsMinimizedAFD := True;
end;

// Função para mostrar Autômato Atual
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

// Classificação do Autômato
procedure ClassifyAutomaton(var A: TAutomaton);
begin
  // Verificar se tem mais de um estado inicial
  if A.countInitial > 1 then
  begin
    A.classification := 'MULTI-INICIAL';
    Exit;
  end;

  // Verificar se tem vazio (ε, λ, '')
  if ContainsEpsilon(A) then
  begin
    A.classification := 'AFN-E';
    Exit;
  end;

  // Verificar se é Determinismo
  if IsDeterministic(A) then
  begin
    // Se for AFD, vetificar se está minimizado ou não
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