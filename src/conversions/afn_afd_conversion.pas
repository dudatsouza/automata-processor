unit afn_afd_conversion;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, automaton, io;

procedure ConvertAFNToAFD(var A: TAutomaton);

implementation

// --------------------------------------------------------------------
// Split: separa uma string por um caractere delimitador e retorna
//        uma TStringList (quem chamar deve liberar a lista).
// --------------------------------------------------------------------
function Split(const s: AnsiString; Delimiter: AnsiChar): TStringList;
var
  i: Integer;
  temp: AnsiString;
begin
  Result := TStringList.Create;
  temp := '';
  for i := 1 to Length(s) do
  begin
    if s[i] = Delimiter then
    begin
      Result.Add(temp);
      temp := '';
    end
    else
      temp := temp + s[i];
  end;
  // adicionar o último segmento (pode ser string vazia se s terminar com delimitador)
  Result.Add(temp);
end;

// --------------------------------------------------------------------
// GetTargets: dado um autômato A, uma lista de estados (StateList) e um
// símbolo, retorna uma TStringList com todos os estados destino
// distintos (duplicatas removidas). Caller libera o resultado.
// --------------------------------------------------------------------
function GetTargets(var A: TAutomaton; StateList: TStringList;
  Symbol: AnsiString): TStringList;
var
  i, j: Integer;
  found: Boolean;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  for i := 0 to High(A.transitions) do
  begin
    if A.transitions[i].symbol <> Symbol then
      Continue;

    // se a origem da transição está em StateList, adiciona o destino
    for j := 0 to StateList.Count - 1 do
      if A.transitions[i].source = StateList[j] then
      begin
        // TStringList com Sorted + dupIgnore já evita duplicatas
        Result.Add(A.transitions[i].target);
        Break;
      end;
  end;
end;

// --------------------------------------------------------------------
// StateSetToName: converte uma TStringList (ordenada) em um nome único
// de estado concatenado por '_' (ex: q0_q1_q2). Não altera a lista.
// --------------------------------------------------------------------
function StateSetToName(States: TStringList): AnsiString;
var
  i: Integer;
begin
  Result := '';
  // assumir que States já está ordenada (ou podemos ordenar localmente)
  for i := 0 to States.Count - 1 do
  begin
    if i > 0 then
      Result := Result + '_';
    Result := Result + States[i];
  end;
end;

// --------------------------------------------------------------------
// ContainsStateName: verifica se o nome (p.ex. 'q0_q1') já está em
// uma TStringList de nomes.
// --------------------------------------------------------------------
function ContainsStateName(StateNames: TStringList; const Name: AnsiString): Boolean;
begin
  Result := StateNames.IndexOf(Name) <> -1;
end;

// --------------------------------------------------------------------
// ConvertAFNToAFD: implementação do algoritmo de subconjuntos sem
// suporte a transições-ε (como você pediu, AFN sem epsilons).
// --------------------------------------------------------------------
procedure ConvertAFNToAFD(var A: TAutomaton);
var
  NewStateNames: TStringList; // nomes dos estados do AFD (ex: 'q0_q1')
  Queue: TStringList;         // fila de processamento de nomes
  Targets: TStringList;
  Parts: TStringList;
  FinalStatesList: TStringList;

  currName, newName: AnsiString;
  symbol: AnsiString;
  i, j, k: Integer;

  // array dinâmico temporário para guardar transições do AFD
  NewTransitions: array of TTransition;

  // temporários para construir arrays de AnsiString ao final
  idx: Integer;
  isFinal: Boolean;
begin
  writeln('>> Convertendo AFN para AFD...');

  // inicializar listas
  NewStateNames := TStringList.Create;
  Queue := TStringList.Create;
  FinalStatesList := TStringList.Create;

  NewStateNames.Sorted := True; // manter ordenado para nomes consistentes
  NewStateNames.Duplicates := dupIgnore;

  // Estado inicial do AFD é o conjunto que contém apenas o estado inicial do AFN
  if Length(A.initialState) = 0 then
  begin
    writeln('>> Erro: automato sem estado inicial definido.');
    NewStateNames.Free;
    Queue.Free;
    FinalStatesList.Free;
    Exit;
  end;

  // colocar o estado inicial (nome simples, ex: 'q0')
  NewStateNames.Add(A.initialState[0]);
  Queue.Add(A.initialState[0]);

  // não há transições ainda
  SetLength(NewTransitions, 0);

  // PROCESSAR a fila
  while Queue.Count > 0 do
  begin
    currName := Queue[0];
    Queue.Delete(0);

    // transformar currName (ex: 'q0_q1') em lista de partes ['q0','q1']
    Parts := Split(currName, '_');

    // para cada símbolo do alfabeto do AFN
    for i := 0 to High(A.alphabet) do
    begin
      symbol := A.alphabet[i];

      // obter os destinos a partir de todos os estados do conjunto Parts
      Targets := GetTargets(A, Parts, symbol);

      // se não há destinos, ignoramos (não cria transição)
      if Targets.Count = 0 then
      begin
        Targets.Free;
        Continue;
      end;

      // garantir ordem e sem duplicatas (GetTargets já faz Sorted + dupIgnore)
      Targets.Sort;

      // nome do novo estado (concatenacao)
      newName := StateSetToName(Targets);

      // se novo nome nao existir, adiciona à lista de estados e fila
      if not ContainsStateName(NewStateNames, newName) then
      begin
        NewStateNames.Add(newName);
        Queue.Add(newName);
      end;

      // adicionar transição (currName, symbol) -> newName
      idx := Length(NewTransitions);
      SetLength(NewTransitions, idx + 1);
      NewTransitions[idx].source := currName;
      NewTransitions[idx].target := newName;
      NewTransitions[idx].symbol := symbol;

      Targets.Free;
    end;

    Parts.Free;
  end;

  // Determinar estados finais do AFD:
  // um estado-conjunto é final se conter qualquer estado final do AFN
  for i := 0 to NewStateNames.Count - 1 do
  begin
    // quebrar o nome em partes para comparar precisamente
    Parts := Split(NewStateNames[i], '_');
    isFinal := False;
    for j := 0 to Parts.Count - 1 do
    begin
      for k := 0 to High(A.finalStates) do
      begin
        if Parts[j] = A.finalStates[k] then
        begin
          isFinal := True;
          Break;
        end;
      end;
      if isFinal then
        Break;
    end;
    Parts.Free;
    if isFinal then
      FinalStatesList.Add(NewStateNames[i]);
  end;

  // Sobrescrever o autômato A com a versão AFD construída

  // estados
  SetLength(A.states, NewStateNames.Count);
  for i := 0 to NewStateNames.Count - 1 do
    A.states[i] := NewStateNames[i];

  // transicoes
  SetLength(A.transitions, Length(NewTransitions));
  for i := 0 to High(NewTransitions) do
    A.transitions[i] := NewTransitions[i];

  // estados finais
  SetLength(A.finalStates, FinalStatesList.Count);
  for i := 0 to FinalStatesList.Count - 1 do
    A.finalStates[i] := FinalStatesList[i];

  // estado inicial (agora é o primeiro estado criado)
  SetLength(A.initialState, 1);
  if NewStateNames.Count > 0 then
    A.initialState[0] := NewStateNames[0]
  else
    A.initialState[0] := '';

  A.classification := 'AFD';

  // salvar em JSON usando rotina existente
  SaveAutomatonJSON('./data/output/AFD.json', A);

  // liberar recursos
  NewStateNames.Free;
  Queue.Free;
  FinalStatesList.Free;

  writeln('>> Conversao AFN -> AFD concluida. Arquivo salvo em ./data/output/AFD.json');
end;

end.
