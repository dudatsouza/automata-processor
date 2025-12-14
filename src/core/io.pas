unit io;

{$mode objfpc}{$H+} // Modo ObjFPC para arrays dinâmicos e Strings Longas (AnsiString)

interface

uses
  SysUtils, Classes, fpjson, jsonparser, automaton;

type
  // Estrutura temporária para leitura do JSON (Dinâmica)
  TAutomatonData = record
    Name: string;
    States: array of string;
    Alphabet: array of string;
    // InitialState agora é ARRAY OF STRING para suportar ["q0"] ou ["q0", "q1"]
    InitialState: array of string; 
    FinalStates: array of string;
    Transitions: TJSONArray;
  end;

function ReadJSON(const FilePath: string): TAutomatonData;
function ConvertTAutomatonData(const Data: TAutomatonData): TAutomaton;
procedure WriteResult(const FilePath: string; const Text: string);
procedure SaveAutomatonJSON(const FilePath: string; const A: TAutomaton);

implementation

function ReadJSON(const FilePath: string): TAutomatonData;
var
  InputFile: TStringList;
  JsonData: TJSONData;
  Obj: TJSONObject;
  InitStatesJSON: TJSONData;
  i: Integer;
begin
  if not FileExists(FilePath) then
  begin
    WriteLn('Erro: arquivo "', FilePath, '" nao encontrado.');
    Halt(1);
  end;

  InputFile := TStringList.Create;
  try
    InputFile.LoadFromFile(FilePath);
    JsonData := GetJSON(InputFile.Text);
    Obj := JsonData as TJSONObject;

    Result.Name := Obj.Get('nome', '');

    // --- Leitura Robusta de Estado Inicial (String ou Array) ---
    InitStatesJSON := Obj.Find('estado_inicial');
    if InitStatesJSON <> nil then
    begin
      if InitStatesJSON.JSONType = jtArray then
      begin
        // Se for array ["q0", "q1"]
        with TJSONArray(InitStatesJSON) do
        begin
          SetLength(Result.InitialState, Count);
          for i := 0 to Count - 1 do
            Result.InitialState[i] := Strings[i];
        end;
      end
      else
      begin
        // Se for string simples "q0" (retrocompatibilidade)
        SetLength(Result.InitialState, 1);
        Result.InitialState[0] := InitStatesJSON.AsString;
      end;
    end
    else
    begin
      SetLength(Result.InitialState, 0);
    end;
    // -----------------------------------------------------------

    // Ler Estados
    if Obj.Find('estados') <> nil then
    with Obj.Arrays['estados'] do
    begin
      SetLength(Result.States, Count);
      for i := 0 to Count - 1 do
        Result.States[i] := Strings[i];
    end;

    // Ler Alfabeto
    if Obj.Find('alfabeto') <> nil then
    with Obj.Arrays['alfabeto'] do
    begin
      SetLength(Result.Alphabet, Count);
      for i := 0 to Count - 1 do
        Result.Alphabet[i] := Strings[i];
    end;

    // Ler Estados Finais
    if Obj.Find('estados_finais') <> nil then
    with Obj.Arrays['estados_finais'] do
    begin
      SetLength(Result.FinalStates, Count);
      for i := 0 to Count - 1 do
        Result.FinalStates[i] := Strings[i];
    end;

    // Clona o array de transições
    if Obj.Find('transicoes') <> nil then
      Result.Transitions := Obj.Arrays['transicoes'].Clone as TJSONArray
    else
      Result.Transitions := TJSONArray.Create; // Vazio se não existir

  finally
    InputFile.Free;
    JsonData.Free;
  end;
end;

function ConvertTAutomatonData(const Data: TAutomatonData): TAutomaton;
var
  i: Integer;
  Obj: TJSONObject;
begin
  // Inicializa os contadores manuais do formato estático
  Result.countStates := 0;
  Result.countAlphabet := 0;
  Result.countFinal := 0;
  Result.countInitial := 0;
  Result.countTransitions := 0;
  Result.classification := '';

  // 1. Converter Estados
  for i := 0 to Length(Data.States) - 1 do
  begin
    if Result.countStates >= MAX_STATES then Break;
    Result.states[Result.countStates] := Data.States[i];
    Inc(Result.countStates);
  end;

  // 2. Converter Alfabeto
  for i := 0 to Length(Data.Alphabet) - 1 do
  begin
    if Result.countAlphabet >= MAX_ALPHABET then Break;
    Result.alphabet[Result.countAlphabet] := Data.Alphabet[i];
    Inc(Result.countAlphabet);
  end;

  // 3. Converter Finais
  for i := 0 to Length(Data.FinalStates) - 1 do
  begin
    if Result.countFinal >= MAX_FINAL_STATES then Break;
    Result.finalStates[Result.countFinal] := Data.FinalStates[i];
    Inc(Result.countFinal);
  end;

  // 4. Converter Estado(s) Inicial(is) - AGORA TRATA COMO ARRAY
  for i := 0 to Length(Data.InitialState) - 1 do
  begin
    if Result.countInitial >= MAX_INITIAL_STATES then Break;
    Result.initialState[Result.countInitial] := Data.InitialState[i];
    Inc(Result.countInitial);
  end;

  // 5. Converter Transições
  if Data.Transitions <> nil then
  begin
    for i := 0 to Data.Transitions.Count - 1 do
    begin
      if Result.countTransitions >= MAX_TRANSITIONS then Break;

      Obj := Data.Transitions.Objects[i];
      Result.transitions[Result.countTransitions].source := Obj.Get('origem', '');
      Result.transitions[Result.countTransitions].target := Obj.Get('destino', '');
      Result.transitions[Result.countTransitions].symbol := Obj.Get('simbolo', '');
      
      Inc(Result.countTransitions);
    end;
  end;
end;

procedure WriteResult(const FilePath: string; const Text: string);
var
  OutputFile: TextFile;
begin
  AssignFile(OutputFile, FilePath);
  Rewrite(OutputFile);
  WriteLn(OutputFile, Text);
  CloseFile(OutputFile);
  WriteLn('Resultado salvo em: ', FilePath);
end;

procedure SaveAutomatonJSON(const FilePath: string; const A: TAutomaton);
var
  OutputFile: TextFile;
  i: Integer;
begin
  AssignFile(OutputFile, FilePath);
  Rewrite(OutputFile);

  // Abertura do JSON
  WriteLn(OutputFile, '{');
  
  // Nome
  WriteLn(OutputFile, '  "nome" : "Automato_' + A.classification + '",');
  WriteLn(OutputFile, ''); 

  // --- Alfabeto ---
  Write(OutputFile, '  "alfabeto" : [');
  for i := 0 to A.countAlphabet - 1 do
  begin
    Write(OutputFile, '"' + A.alphabet[i] + '"');
    if i < A.countAlphabet - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');

  // --- Estados ---
  Write(OutputFile, '  "estados" : [');
  for i := 0 to A.countStates - 1 do
  begin
    Write(OutputFile, '"' + A.states[i] + '"');
    if i < A.countStates - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');

  // --- Estados Finais ---
  Write(OutputFile, '  "estados_finais" : [');
  for i := 0 to A.countFinal - 1 do
  begin
    Write(OutputFile, '"' + A.finalStates[i] + '"');
    if i < A.countFinal - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');

  // --- Estado Inicial ---
  Write(OutputFile, '  "estado_inicial" : [');
  for i := 0 to A.countInitial - 1 do
  begin
    Write(OutputFile, '"' + A.initialState[i] + '"');
    if i < A.countInitial - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');
  WriteLn(OutputFile, '');

  // --- Transições ---
  WriteLn(OutputFile, '  "transicoes" : [');
  
  for i := 0 to A.countTransitions - 1 do
  begin
    Write(OutputFile, '    { ');
    Write(OutputFile, '"origem" : "' + A.transitions[i].source + '", ');
    Write(OutputFile, '"destino" : "' + A.transitions[i].target + '", ');
    Write(OutputFile, '"simbolo" : "' + A.transitions[i].symbol + '"');
    Write(OutputFile, ' }');

    if i < A.countTransitions - 1 then
      WriteLn(OutputFile, ',')
    else
      WriteLn(OutputFile, '');
  end;
  
  WriteLn(OutputFile, '  ]');
  WriteLn(OutputFile, '}');

  CloseFile(OutputFile);

  WriteLn;
  WriteLn('>> Automato ', A.classification, ' salvo em: ', FilePath);
  WriteLn;
end;

end.