unit words_test;

interface

uses
  automaton, utils; 

procedure TestWords(var A: TAutomaton);

implementation


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

function IsWordAccepted(var A: TAutomaton; const word: String; var reason: String): Boolean;
var
  // Buffers estáticos para simular o conjunto de estados ativos
  currentStates: array[0..MAX_STATES] of String;
  countCurrent: Integer;
  
  nextStates: array[0..MAX_STATES] of String;
  countNext: Integer;
  
  i, j, t: Integer;
  ch: String; // String, pois o símbolo no autômato é String
  alphabetString: String;
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

    if not ContainsString(A.alphabet, A.countAlphabet, ch) then
    begin
      // --- CONSTRUÇÃO DA MENSAGEM DE ERRO DETALHADA ---
      // Usa 'j' (já declarado) e 'alphabetString' (movido para o VAR principal)
      begin
        alphabetString := '';
        
        // Constrói a lista de símbolos do alfabeto: {a, b, c}
        for j := 0 to A.countAlphabet - 1 do
        begin
          if j > 0 then 
            alphabetString := alphabetString + ', ';
          alphabetString := alphabetString + A.alphabet[j];
        end;
        
        reason := 'Simbolo "' + ch + '" nao pertence ao alfabeto do automato. O alfabeto aceito eh composto por {' + alphabetString + '}';
      end;
      // --------------------------------------------------
      
      IsWordAccepted := False;
      Exit;
    end;

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

procedure TestWords(var A: TAutomaton);
var
  word: String;
  accepted: Boolean;
  reason: String;
begin
  writeln;
  writeln('----- TESTADOR DE PALAVRAS -----');

  ShowAutomatonDetails(A);

  writeln;
  writeln('----- TESTE UMA PALAVRA -----');


  while True do
  begin
    writeln('Digite "sair" para retornar ao menu.');
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