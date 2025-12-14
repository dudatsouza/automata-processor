unit afne_afn_conversion;

interface

uses
  automaton, utils, io;

procedure ConvertAFNEToAFN(var A: TAutomaton);

implementation

// Função encontra os estados q sao possiveis ser alcancados por transicoes vazias
procedure ComputeEpsilonClosure(const state: String; const A: TAutomaton; var closure: array of boolean);
var
  i, sIndex: Integer;
begin
  // Encontra o indice do estado
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

  // Busca de transicoes vazias, usamos aqui DFS recusivo
  for i := 0 to A.countTransitions - 1 do
  begin
    // Procura transições saindo deste estado com simbolo epsilon
    if (A.transitions[i].source = state) and (A.transitions[i].symbol = '') then
      ComputeEpsilonClosure(A.transitions[i].target, A, closure);
  end;
end;

// Função para identificar se é um estadp final
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

// FUnção conferir se um termo existe em uma lista
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

// Função de Converter AFN-E para AFN
procedure ConvertAFNEToAFN(var A: TAutomaton);
var
  // Iteradores
  i, j, k, x, sIdx: Integer;
  
  // Arrays estáticos
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

  // Analise de cada estado do automato 
  for sIdx := 0 to A.countStates - 1 do
  begin
    originState := A.states[sIdx];

    // limpa o array
    for i := 0 to MAX_STATES do originClosure[i] := False;

    // calcula o fecho
    ComputeEpsilonClosure(originState, A, originClosure);

    // Varre todos os estados 'p' (intermediate)
    for i := 0 to A.countStates - 1 do
    begin
      // Se 'p' não está no fecho de 'q', ignora
      if not originClosure[i] then Continue;
      
      intermediateState := A.states[i]; 

      // Buscar transições REAIS (não-epsilon) saindo de 'p'
      for j := 0 to A.countTransitions - 1 do
      begin
        if (A.transitions[j].source = intermediateState) and (A.transitions[j].symbol <> '') then
        begin
          // Temos: p --a--> r (r = target original)
          
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

    // Recalcular Estados Finais
    // Se o fecho de 'q' alcança um final original, 'q' vira final
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

  // Aplicar alterações no Objeto Principal (Cópia de volta)
  
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

  if (A.classification = 'AFN') or (A.classification = 'AFD') or (A.classification = 'AFD-MINIMO') then
  begin
    writeln('>> Conversao AFN-E para AFN executada com sucesso!');
    
    // Verifica classificações mais específicas e salva com o nome apropriado
    if A.classification = 'AFD-MINIMO' then
    begin
      writeln('   (Nota: O classificador identificou que o automato resultante ja eh AFD MINIMO!)');
      SaveAutomatonJSON('./data/output/AFN_E_para_AFD_MINIMO.json', A);
    end
    else if A.classification = 'AFD' then
    begin
      writeln('   (Nota: O classificador identificou que o automato resultante ja eh AFD.)');
      SaveAutomatonJSON('./data/output/AFN_E_para_AFD.json', A);
    end
    else
    begin
      // Caso padrão: AFN
      SaveAutomatonJSON('./data/output/AFN_converted.json', A);
    end;
    
  end 
  else
  begin
    writeln('!! [ERRO DE CLASSIFICACAO]: Conversao falhou ou foi classificada incorretamente como ', A.classification, '!!');
  end;

end;

end.