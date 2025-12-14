unit words_test;

interface

uses
  automaton; 

procedure TestWords(var A: TAutomaton);

implementation

// ============================================================================
// FUNÇÕES AUXILIARES LOCAIS (Estáticas)
// ============================================================================

// Verifica se uma string está no array (usando contador manual)
function ContainsString(const Arr: array of String; Count: Integer; const Val: String): Boolean;
var
  i: Integer;
begin
  ContainsString := False;
  for i := 0 to Count - 1 do
    if Arr[i] = Val then
    begin
      ContainsString := True;
      Exit;
    end;
end;

// Verifica se é estado final
function IsStateFinal(const A: TAutomaton; const state: String): Boolean;
begin
  IsStateFinal := ContainsString(A.finalStates, A.countFinal, state);
end;

// ============================================================================
// LÓGICA DE SIMULAÇÃO
// ============================================================================

function IsWordAccepted(var A: TAutomaton; const word: String; var reason: String): Boolean;
var
  // Buffers estáticos para simular o conjunto de estados ativos
  currentStates: array[0..MAX_STATES] of String;
  countCurrent: Integer;
  
  nextStates: array[0..MAX_STATES] of String;
  countNext: Integer;
  
  i, j, t: Integer;
  ch: String; // String, pois o símbolo no autômato é String
begin
  reason := '';

  // 1. Inicializa estados atuais com os estados iniciais
  countCurrent := 0;
  for i := 0 to A.countInitial - 1 do
  begin
    currentStates[countCurrent] := A.initialState[i];
    Inc(countCurrent);
  end;

  // 2. Caso especial: Palavra Vazia
  if word = '' then
  begin
    for i := 0 to countCurrent - 1 do
      if IsStateFinal(A, currentStates[i]) then
      begin
        IsWordAccepted := True;
        Exit;
      end;

    reason := 'Palavra vazia nao aceita: estado inicial nao eh final';
    IsWordAccepted := False;
    Exit;
  end;

  // 3. Loop para cada caractere da palavra
  for i := 1 to Length(word) do
  begin
    ch := word[i]; // Converte char para string automaticamente
    countNext := 0; // Limpa o buffer de próximos estados

    // Para cada estado atual (simula não-determinismo)
    for j := 0 to countCurrent - 1 do
    begin
      // Procurar transições no autômato
      for t := 0 to A.countTransitions - 1 do
      begin
        // Verifica origem E símbolo
        if (A.transitions[t].source = currentStates[j]) and
           (A.transitions[t].symbol = ch) then
        begin
          // Adiciona ao nextStates se ainda não estiver lá (evita duplicatas)
          if not ContainsString(nextStates, countNext, A.transitions[t].target) then
          begin
            // Verificação de segurança de limite
            if countNext < MAX_STATES then
            begin
              nextStates[countNext] := A.transitions[t].target;
              Inc(countNext);
            end;
          end;
        end;
      end;
    end;

    // Se não houver estados seguintes, travou
    if countNext = 0 then
    begin
      reason := 'Travou: Nenhuma transicao para [' + ch + ']';
      IsWordAccepted := False;
      Exit;
    end;

    // Avança: Current := Next
    countCurrent := countNext;
    for j := 0 to countNext - 1 do
      currentStates[j] := nextStates[j];
  end;

  // 4. Verifica se parou em algum estado final
  for i := 0 to countCurrent - 1 do
    if IsStateFinal(A, currentStates[i]) then
    begin
      IsWordAccepted := True;
      Exit;
    end;

  reason := 'Palavra terminou em estado nao-final';
  IsWordAccepted := False;
end;

// ============================================================================
// INTERFACE DE TESTE
// ============================================================================
procedure TestWords(var A: TAutomaton);
var
  word: String;
  accepted: Boolean;
  reason: String;
  i: Integer;
begin
  writeln;
  writeln('========== TESTADOR DE PALAVRAS ==========');

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