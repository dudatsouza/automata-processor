unit afne_conversion;

interface

uses
  sysutils, automaton, utils, io;  // sysutils é usado apenas para transformar um inteiro em uma string (IntToStr)

procedure ConvertMultiInitialToAFNE(var A: TAutomaton);

implementation

// Verificação se existe um estado específico
function StateExists(const A: TAutomaton; stateName: String): Boolean;
var
  i: Integer;
begin
  StateExists := False;
  for i := 0 to A.countStates - 1 do
  begin
    if A.states[i] = stateName then
    begin
      StateExists := True;
      Exit;
    end;
  end;
end;

// Renomeia estados em todos os lugares
procedure RenameState(var A: TAutomaton; oldName, newName: String);
var
  i: Integer;
begin
  // Atualiza lista de estados
  for i := 0 to A.countStates - 1 do
    if A.states[i] = oldName then
      A.states[i] := newName;

  // Atualiza transições (origem e destino)
  for i := 0 to A.countTransitions - 1 do
  begin
    if A.transitions[i].source = oldName then
      A.transitions[i].source := newName;
    if A.transitions[i].target = oldName then
      A.transitions[i].target := newName;
  end;

  // Atualiza estados finais
  for i := 0 to A.countFinal - 1 do
    if A.finalStates[i] = oldName then
      A.finalStates[i] := newName;

  // Atualiza estados iniciais
  for i := 0 to A.countInitial - 1 do
    if A.initialState[i] = oldName then
      A.initialState[i] := newName;
end;

// Fazer com que o 'targetName' seja o estado inicial, se tiver algum estado com esse nome, mude
procedure EnsureNameIsAvailable(var A: TAutomaton; targetName: String; backupPrefix: String);
var
  k: Integer;
  safeName: String;
begin
  // se nao existir, e estiver liberado, tudo certo, continue
  if not StateExists(A, targetName) then
    Exit;

  k := 1; // k começa com 1, pois q0, será o estado inicial
  repeat
    safeName := backupPrefix + IntToStr(k); // atribuição de um novo possível nome
    
    if safeName = targetName then 
    begin
       Inc(k);
       Continue;
    end;

    // verificar se o novo nome gerado já esta atribuido a algum outro estado
    if StateExists(A, safeName) then
    begin
      Inc(k);
      Continue;
    end;
    
    // encontrou livre
    Break; 
  until False;

  writeln('>> Conflito: O estado "', targetName, '" ja existe. Renomeando para "', safeName, '"...');
  RenameState(A, targetName, safeName);
end;

// Conversor de MULTI-INICIAL para AFN-E
procedure ConvertMultiInitialToAFNE(var A: TAutomaton);
var
  i: Integer;
  newInitial: String;
begin
  // se ele nao tiver mais doq um estado inicial, nao faz nada
  if A.countInitial <= 1 then
    Exit;

  writeln('>> Convertendo automato Multi-Inicial para AFN-E...');

  // garantir q o nosso futura estado inicial esteja livre (q0)
  EnsureNameIsAvailable(A, 'q0', 'q');
  
  newInitial := 'q0';

  // Cria uma transição de q0 (novo estado inicial) para os antigos estados inicias
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
    Inc(A.countTransitions); 
  end;

  // Adiciona o novo estado na lista de estados
  if not StateExists(A, newInitial) then
  begin
    if A.countStates < MAX_STATES then
    begin
      A.states[A.countStates] := newInitial;
      Inc(A.countStates);
    end;
  end;

  // Atualiza o estado inicial único
  A.initialState[0] := newInitial;

  // limpar os antigos estados iniciais
  for i := 1 to MAX_INITIAL_STATES - 1 do
    A.initialState[i] := '';

  // conta quantos estados reais tem, ignorando os vazios
  A.countInitial := 0; 
  for i := 0 to MAX_INITIAL_STATES - 1 do
  begin
    if Trim(A.initialState[i]) <> '' then
      Inc(A.countInitial);
  end;

  // Verificar se deu certo, se nao, repetir a funcao, até q fique apenas 1 estado inicial
  if A.countInitial <> 1 then
  begin
    writeln('!! ALERTA: A consolidacao falhou. Detectados ', A.countInitial, ' iniciais. Tentando novamente...');
    
    // Chamada recursiva para tentar corrigir
    ConvertMultiInitialToAFNE(A);
  end;

  // Reclassificar o automato
  ClassifyAutomaton(A);

  if A.classification = 'AFN-E' then
  begin
    writeln('>> Conversao Multi-Inicial para AFN-E executada com sucesso!');
    SaveAutomatonJSON('./data/output/AFN_multiinicial_conertido.json', A);
  end
  else
  begin
    writeln('!! [ERRO DE CLASSIFICACAO]: Conversao falhou ou foi classificada incorretamente como ', A.classification, '!!');
  end;

end;

end.