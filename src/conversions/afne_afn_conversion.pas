unit afne_afn_conversion;

interface

uses
  automaton, io, sysutils, utils;

procedure ConvertAFNEToAFN(var A: TAutomaton);

implementation

// ------------------------------------------------------------
// Calcula o epsilon-fecho (Estados alcançáveis apenas com epsilon)
// ------------------------------------------------------------
procedure ComputeEpsilonClosure(const state: String; const A: TAutomaton; var closure: array of boolean);
var
  i, sIndex: Integer;
begin
  // 1. Achar índice do estado na lista principal
  sIndex := -1;
  for i := 0 to A.countStates - 1 do
    if A.states[i] = state then
    begin
      sIndex := i;
      Break;
    end;

  // Se não achou ou já visitou, sai
  if (sIndex = -1) or (closure[sIndex]) then
    Exit;

  closure[sIndex] := True; // Marca como parte do fecho

  // 2. DFS recursiva para achar transições epsilon
  for i := 0 to A.countTransitions - 1 do
  begin
    // Procura transições saindo deste estado com simbolo epsilon
    if (A.transitions[i].source = state) and (A.transitions[i].symbol = 'ε') then
      ComputeEpsilonClosure(A.transitions[i].target, A, closure);
  end;
end;

// ------------------------------------------------------------
// Verifica se um estado é Final no autômato original
// ------------------------------------------------------------
function IsStateFinal(const A: TAutomaton; stateName: String): Boolean;
var
  i: Integer;
begin
  IsStateFinal := False;
  for i := 0 to A.countFinal - 1 do
    if A.finalStates[i] = stateName then
    begin
      IsStateFinal := True;
      Exit;
    end;
end;

// ------------------------------------------------------------
// Verifica se uma string já existe num array (evita duplicação)
// ------------------------------------------------------------
function StringExistsInArray(const list: array of String; const count: Integer; const value: String): Boolean;
var
  i: Integer;
begin
  StringExistsInArray := False;
  for i := 0 to count - 1 do
    if list[i] = value then
    begin
      StringExistsInArray := True;
      Exit;
    end;
end;

// ------------------------------------------------------------
// Procedimento Principal
// ------------------------------------------------------------
procedure ConvertAFNEToAFN(var A: TAutomaton);
var
  // Iteradores
  i, j, k, x, sIdx: Integer;
  
  // Buffers Estáticos (Substituindo arrays dinâmicos)
  originClosure: array[0..MAX_STATES] of Boolean;
  targetClosure: array[0..MAX_STATES] of Boolean;
  
  tempTransitions: array[0..MAX_TRANSITIONS] of TTransition;
  tempTransCount: Integer;
  
  tempFinals: array[0..MAX_FINAL_STATES] of String;
  tempFinalCount: Integer;

  // Variáveis auxiliares
  originState, intermediateState, finalTargetState: String;
  isFinal, transitionExists: Boolean;

begin
  writeln('>> Convertendo automato AFN-E para AFN...');

  // Inicialização de contadores temporários
  tempTransCount := 0;
  tempFinalCount := 0;

  // ========================================================
  // Loop Principal: Para cada estado do autômato (origem)
  // ========================================================
  for sIdx := 0 to A.countStates - 1 do
  begin
    originState := A.states[sIdx];

    // 1. Limpar o array de fecho (Lógica SEGURA do HEAD)
    // Usamos MAX_STATES para garantir que não sobra lixo de memória
    for i := 0 to MAX_STATES do originClosure[i] := False;

    // 2. Calcular o Epsilon Fecho
    ComputeEpsilonClosure(originState, A, originClosure);

    // 3. Debug / Prints (Funcionalidade da Feature ADAPTADA)
    // Agora está ativo e usando os contadores corretos (countStates)
    writeln('Imprimindo E-fechos encontrados...');
    write('e-fecho(', originState, ') = { ');

    for i := 0 to A.countStates - 1 do
    begin
      // Verifica se o índice 'i' está marcado como True no array estático
      if originClosure[i] then
        write(A.states[i], ' ');
    end;
    writeln('}');
    writeln;

    // ========================================================
    // Passo 2: Gerar Novas Transições
    // Lógica: q --(ε*)--> p --(a)--> r --(ε*)--> s  ===>  q --(a)--> s
    // ========================================================
    
    // Varre todos os estados 'p' (intermediate)
    for i := 0 to A.countStates - 1 do
    begin
      // Se 'p' não está no fecho de 'q', ignora
      if not originClosure[i] then Continue;
      
      intermediateState := A.states[i]; 

      // Buscar transições REAIS (não-epsilon) saindo de 'p'
      for j := 0 to A.countTransitions - 1 do
      begin
        if (A.transitions[j].source = intermediateState) and (A.transitions[j].symbol <> 'ε') then
        begin
          // Temos: p --a--> r (r = target original)
          // Agora calculamos o fecho de 'r' para achar os 's' finais
          
          for k := 0 to MAX_STATES do targetClosure[k] := False;
          ComputeEpsilonClosure(A.transitions[j].target, A, targetClosure);

          // Criar transições de 'originState' para todos os 's' (finalTargetState)
          for k := 0 to A.countStates - 1 do
          begin
            // Se estado k está no fecho do destino
            if targetClosure[k] then
            begin
              finalTargetState := A.states[k];
              
              // Verifica duplicidade antes de adicionar
              transitionExists := False;
              for x := 0 to tempTransCount - 1 do
              begin
                 if (tempTransitions[x].source = originState) and
                    (tempTransitions[x].symbol = A.transitions[j].symbol) and
                    (tempTransitions[x].target = finalTargetState) then
                 begin
                   transitionExists := True;
                   Break;
                 end;
              end;

              if not transitionExists then
              begin
                 if tempTransCount >= MAX_TRANSITIONS then
                 begin
                   writeln('ERRO: Limite de transicoes excedido na conversao AFN.');
                   Break;
                 end;

                 tempTransitions[tempTransCount].source := originState;
                 tempTransitions[tempTransCount].symbol := A.transitions[j].symbol;
                 tempTransitions[tempTransCount].target := finalTargetState;
                 Inc(tempTransCount);
              end;
            end;
          end;
        end;
      end;
    end;

    // ========================================================
    // Passo 3: Recalcular Estados Finais
    // Se o fecho de 'q' alcança um final original, 'q' vira final
    // ========================================================
    isFinal := False;
    for i := 0 to A.countStates - 1 do
    begin
      if originClosure[i] and IsStateFinal(A, A.states[i]) then
      begin
        isFinal := True;
        Break;
      end;
    end;

    if isFinal and not StringExistsInArray(tempFinals, tempFinalCount, originState) then
    begin
      if tempFinalCount < MAX_FINAL_STATES then
      begin
        tempFinals[tempFinalCount] := originState;
        Inc(tempFinalCount);
      end;
    end;
  end;

  // ========================================================
  // Passo 4: Aplicar alterações no Objeto Principal (Cópia de volta)
  // ========================================================
  
  // 4.1 Substituir Transições
  A.countTransitions := tempTransCount;
  for i := 0 to tempTransCount - 1 do
    A.transitions[i] := tempTransitions[i];

  // 4.2 Substituir Estados Finais
  A.countFinal := tempFinalCount;
  for i := 0 to tempFinalCount - 1 do
    A.finalStates[i] := tempFinals[i];

  // 4.3 Reclassificar
  ClassifyAutomaton(A);

  writeln('>> Conversao AFN-E para AFN executada com sucesso!');
  
  SaveAutomatonJSON('./data/output/AFN_converted.json', A);
end;

end.