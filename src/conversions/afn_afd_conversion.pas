unit afn_afd_conversion;

interface

uses
  automaton, io, utils;

procedure ConvertAFNToAFD(var A: TAutomaton);

implementation

// ============================================================================
// FUNÇÕES AUXILIARES (Estilo Pascal Puro / Estático)
// ============================================================================

// Verifica se string existe no array (Busca Linear)
function ArrayContains(const Arr: array of String; Count: Integer; const Val: String): Boolean;
var
  i: Integer;
begin
  ArrayContains := False;
  for i := 0 to Count - 1 do
    if Arr[i] = Val then
    begin
      ArrayContains := True;
      Exit;
    end;
end;

// Adiciona ao array apenas se não existir (Simula Conjunto)
procedure ArrayAddUnique(var Arr: array of String; var Count: Integer; const Val: String);
begin
  // Verifica limite (Assumindo que quem chama passou um array grande o suficiente)
  // No caso, usamos MAX_STATES como base
  if not ArrayContains(Arr, Count, Val) then
  begin
    Arr[Count] := Val;
    Inc(Count);
  end;
end;

// Ordena array de strings (Bubble Sort) - Essencial para "q0_q1" == "q1_q0"
procedure ArraySort(var Arr: array of String; Count: Integer);
var
  i, j: Integer;
  temp: String;
begin
  if Count < 2 then Exit;
  
  for i := 0 to Count - 2 do
    for j := 0 to Count - 2 - i do
      if Arr[j] > Arr[j + 1] then
      begin
        temp := Arr[j];
        Arr[j] := Arr[j + 1];
        Arr[j + 1] := temp;
      end;
end;

// Quebra string por delimitador (Simula Split)
// Preenche o buffer 'Parts' e atualiza 'Count'
procedure CustomSplit(const Text: String; Delimiter: Char; var Parts: array of String; var Count: Integer);
var
  i: Integer;
  buffer: String;
begin
  Count := 0;
  buffer := '';
  
  for i := 1 to Length(Text) do
  begin
    if Text[i] = Delimiter then
    begin
      Parts[Count] := buffer;
      Inc(Count);
      buffer := '';
    end
    else
      buffer := buffer + Text[i];
  end;
  // Adiciona o último pedaço
  Parts[Count] := buffer;
  Inc(Count);
end;

// Junta array em string (Simula Join)
function ArrayToString(const Arr: array of String; Count: Integer; Delimiter: Char): String;
var
  i: Integer;
  res, temp: String;
begin
  res := '';
  for i := 0 to Count - 1 do
  begin
    if i > 0 then 
      temp := Delimiter + Arr[i]
    else 
      temp := Arr[i];
    
    // Verifica se vai estourar o limite de 255 chars do Pascal
    if Length(res) + Length(temp) > 255 then
    begin
      writeln('ERRO FATAL: O nome do estado composto excedeu 255 caracteres.');
      writeln('Isso ocorre em automatos muito complexos convertidos para AFD.');
      Halt(1); // Para o programa antes de gerar lógica errada
    end;

    res := res + temp;
  end;
  ArrayToString := res;
end;

// ============================================================================
// ALGORITMO PRINCIPAL
// ============================================================================

procedure ConvertAFNToAFD(var A: TAutomaton);
var
  // Estruturas do Algoritmo de Subconjuntos (Estáticas)
  Queue: array[0..MAX_STATES] of String;
  QHead, QTail: Integer;

  NewStateNames: array[0..MAX_STATES] of String;
  NewStateCount: Integer;

  NewTransitions: array[0..MAX_TRANSITIONS] of TTransition;
  TransCount: Integer;

  FinalStatesList: array[0..MAX_FINAL_STATES] of String;
  FinalCount: Integer;

  // Variáveis temporárias
  CurrentStateName, NewName, Symbol: String;
  
  // Buffers para manipulação de conjuntos ("q0_q1" -> ["q0", "q1"])
  CurrentParts: array[0..MAX_STATES] of String;
  CPCount: Integer;
  
  TargetParts: array[0..MAX_STATES] of String;
  TPCount: Integer;
  
  i, j, k: Integer;
  isFinal: Boolean;
begin
  writeln('>> Convertendo AFN para AFD (Algoritmo de Subconjuntos)...');

  // 1. Validação inicial
  if A.countInitial = 0 then
  begin
    writeln('ERRO: Automato sem estado inicial.');
    Exit;
  end;

  // 2. Inicialização das estruturas
  QHead := 0; 
  QTail := 0;
  NewStateCount := 0;
  TransCount := 0;
  FinalCount := 0;

  // 3. Configurar estado inicial do AFD
  // Assumimos que o AFN já tem um único inicial (devido a conversão anterior)
  // Se não tiver epsilons, o inicial do AFD é apenas {q0}
  CurrentStateName := A.initialState[0]; 
  
  // Enfileirar (Push)
  Queue[QTail] := CurrentStateName; Inc(QTail);
  
  // Adicionar à lista de estados conhecidos do AFD
  NewStateNames[NewStateCount] := CurrentStateName; Inc(NewStateCount);

  // 4. Loop Principal (Enquanto houver estados na fila)
  while QHead < QTail do
  begin
    // Desenfileirar (Pop)
    CurrentStateName := Queue[QHead];
    Inc(QHead);

    // Quebrar o nome composto "q0_q1" em partes ["q0", "q1"]
    CustomSplit(CurrentStateName, '_', CurrentParts, CPCount);

    // Para cada símbolo do alfabeto
    for i := 0 to A.countAlphabet - 1 do
    begin
      Symbol := A.alphabet[i];
      TPCount := 0; // Limpa alvos para este símbolo

      // Descobrir para onde vamos com esse símbolo
      // (União dos destinos de todos os sub-estados)
      for j := 0 to CPCount - 1 do
      begin
        // Procura transições no autômato original
        for k := 0 to A.countTransitions - 1 do
        begin
          if (A.transitions[k].source = CurrentParts[j]) and 
             (A.transitions[k].symbol = Symbol) then
          begin
            // Adiciona destino ao conjunto (sem duplicatas)
            // Usa TargetParts como buffer temporário
            ArrayAddUnique(TargetParts, TPCount, A.transitions[k].target);
          end;
        end;
      end;

      // Se não vai para lugar nenhum, ignora (AFD incompleto por enquanto)
      if TPCount = 0 then Continue;

      // Ordenar para garantir nome canônico (q1_q0 -> q0_q1)
      ArraySort(TargetParts, TPCount);

      // Criar nome do novo estado composto
      NewName := ArrayToString(TargetParts, TPCount, '_');

      // Se esse estado ainda não existe na lista de novos estados...
      if not ArrayContains(NewStateNames, NewStateCount, NewName) then
      begin
        // Verifica overflow de estados
        if NewStateCount >= MAX_STATES then
        begin
          writeln('ERRO: Limite de estados atingido durante conversao AFD.');
          Break;
        end;
        
        NewStateNames[NewStateCount] := NewName;
        Inc(NewStateCount);

        // Adiciona na fila para processar depois
        Queue[QTail] := NewName;
        Inc(QTail);
      end;

      // Registrar transição do AFD: Current -> Symbol -> New
      if TransCount < MAX_TRANSITIONS then
      begin
        NewTransitions[TransCount].source := CurrentStateName;
        NewTransitions[TransCount].symbol := Symbol;
        NewTransitions[TransCount].target := NewName;
        Inc(TransCount);
      end;
    end;
  end;

  // 5. Calcular Estados Finais do AFD
  // Um estado "q0_q1" é final se q0 OU q1 forem finais no original
  for i := 0 to NewStateCount - 1 do
  begin
    CustomSplit(NewStateNames[i], '_', CurrentParts, CPCount);
    isFinal := False;

    for j := 0 to CPCount - 1 do
    begin
      // Verifica se a parte atual é final no original
      if ArrayContains(A.finalStates, A.countFinal, CurrentParts[j]) then
      begin
        isFinal := True;
        Break;
      end;
    end;

    if isFinal then
    begin
      if FinalCount < MAX_FINAL_STATES then
      begin
        FinalStatesList[FinalCount] := NewStateNames[i];
        Inc(FinalCount);
      end;
    end;
  end;

  // 6. Atualizar o objeto Autômato
  
  // Copiar Estados
  A.countStates := NewStateCount;
  for i := 0 to NewStateCount - 1 do
    A.states[i] := NewStateNames[i];

  // Copiar Transições
  A.countTransitions := TransCount;
  for i := 0 to TransCount - 1 do
    A.transitions[i] := NewTransitions[i];

  // Copiar Finais
  A.countFinal := FinalCount;
  for i := 0 to FinalCount - 1 do
    A.finalStates[i] := FinalStatesList[i];

  // Inicial (sempre o primeiro inserido na BFS)
  A.countInitial := 1;
  if NewStateCount > 0 then
    A.initialState[0] := NewStateNames[0]
  else
    A.initialState[0] := '';

  // Reclassificar
  ClassifyAutomaton(A);

  writeln('>> Conversao AFN -> AFD concluida com sucesso!');
  SaveAutomatonJSON('./data/output/AFD.json', A);
end;

end.