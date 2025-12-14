unit afne_conversion;

interface

uses
  automaton, utils, sysutils, io;

procedure ConvertMultiInitialToAFNE(var A: TAutomaton);

implementation

// Verifica se estado existe usando os contadores manuais
function StateExists(const A: TAutomaton; stateName: String): Boolean;
var
  i: Integer;
begin
  StateExists := False;
  // Loop de 0 até count - 1 (Pascal Puro)
  for i := 0 to A.countStates - 1 do
  begin
    if A.states[i] = stateName then
    begin
      StateExists := True;
      Exit;
    end;
  end;
end;

// Renomeia estados em todas as listas estáticas
procedure RenameState(var A: TAutomaton; oldName, newName: String);
var
  i: Integer;
begin
  // 1. Atualizar lista de estados
  for i := 0 to A.countStates - 1 do
    if A.states[i] = oldName then
      A.states[i] := newName;

  // 2. Atualizar transições (origem e destino)
  for i := 0 to A.countTransitions - 1 do
  begin
    if A.transitions[i].source = oldName then
      A.transitions[i].source := newName;
    if A.transitions[i].target = oldName then
      A.transitions[i].target := newName;
  end;

  // 3. Atualizar estados finais
  for i := 0 to A.countFinal - 1 do
    if A.finalStates[i] = oldName then
      A.finalStates[i] := newName;

  // 4. Atualizar estados iniciais
  for i := 0 to A.countInitial - 1 do
    if A.initialState[i] = oldName then
      A.initialState[i] := newName;
end;

// Garante que 'targetName' esteja livre
procedure EnsureNameIsAvailable(var A: TAutomaton; targetName: String; backupPrefix: String);
var
  k: Integer;
  safeName: String;
begin
  if not StateExists(A, targetName) then
    Exit;

  k := 1; 
  repeat
    safeName := backupPrefix + IntToStr(k);
    
    if safeName = targetName then 
    begin
       Inc(k);
       Continue;
    end;

    if StateExists(A, safeName) then
    begin
      Inc(k);
      Continue;
    end;
    
    // Encontrou livre
    Break; 
  until False;

  writeln('>> Conflito: O estado "', targetName, '" ja existe. Renomeando para "', safeName, '"...');
  RenameState(A, targetName, safeName);
end;

procedure ConvertMultiInitialToAFNE(var A: TAutomaton);
var
  i: Integer;
  newInitial: String;
begin
  // 1. Validação: Só converte se tiver > 1 inicial
  if A.countInitial <= 1 then
    Exit;

  writeln('>> Convertendo automato Multi-Inicial para AFN-E...');

  // 2. Preparação: Garantir que 'q0' esteja livre
  EnsureNameIsAvailable(A, 'q0', 'q');
  
  newInitial := 'q0';

  // 3. Criar transições epsilon do novo q0 para os iniciais antigos
  // Em vez de SetLength, adicionamos ao fim do array estático
  for i := 0 to A.countInitial - 1 do
  begin
    // Verifica overflow do array estático
    if A.countTransitions >= MAX_TRANSITIONS then
    begin
      writeln('ERRO CRITICO: Limite de transicoes atingido ao criar epsilons.');
      Break; 
    end;

    // Adiciona nova transição
    A.transitions[A.countTransitions].source := newInitial;
    A.transitions[A.countTransitions].target := A.initialState[i];
    A.transitions[A.countTransitions].symbol := 'ε'; 
    Inc(A.countTransitions); // Incrementa contador manual
  end;

  // 4. Registrar o novo estado na lista de estados
  if not StateExists(A, newInitial) then
  begin
    if A.countStates < MAX_STATES then
    begin
      A.states[A.countStates] := newInitial;
      Inc(A.countStates);
    end;
  end;

  // 5. Atualizar o estado inicial único
  // O array initialState é estático, mas logicamente agora só tem 1 elemento
  A.initialState[0] := newInitial;
  A.countInitial := 1;

  // 6. Reclassificar
  ClassifyAutomaton(A); 

  writeln('>> Conversao Multi-Inicial para AFN-E executada com sucesso!');
  SaveAutomatonJSON('./data/output/AFN_multiinicial_conertido.json', A);
end;

end.