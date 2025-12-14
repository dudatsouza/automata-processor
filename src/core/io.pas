unit io;

{$mode objfpc}{$H+} 

interface

uses // aqui podemos usar coisas do pascal moderno, pois nao afeta em nada na logica do trabalho, apenas como trata a entrada e saida
  SysUtils, Classes, fpjson, jsonparser, automaton;

type
  TAutomatonData = record
    Name: string;
    States: array of string;
    Alphabet: array of string;
    InitialState: array of string; 
    FinalStates: array of string;
    Transitions: TJSONArray;
  end;

function ReadJSON(const FilePath: string): TAutomatonData;
function ConvertTAutomatonData(const Data: TAutomatonData): TAutomaton;
procedure SaveAutomatonJSON(const FilePath: string; const A: TAutomaton);

implementation

// Leitura do JSON
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

    // Leitura de Estado Inicial 
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

    // Leitura dos Estados
    if Obj.Find('estados') <> nil then
    with Obj.Arrays['estados'] do
    begin
      SetLength(Result.States, Count);
      for i := 0 to Count - 1 do
        Result.States[i] := Strings[i];
    end;

    // Leitura do Alfabeto
    if Obj.Find('alfabeto') <> nil then
    with Obj.Arrays['alfabeto'] do
    begin
      SetLength(Result.Alphabet, Count);
      for i := 0 to Count - 1 do
        Result.Alphabet[i] := Strings[i];
    end;

    // Leitura dos Estados Finais
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
      Result.Transitions := TJSONArray.Create; 

  finally
    InputFile.Free;
    JsonData.Free;
  end;
end;

// Passa para TAutomaton para conseguir executar as conversões e outras funções
function ConvertTAutomatonData(const Data: TAutomatonData): TAutomaton;
var
  i: Integer;
  Obj: TJSONObject;
begin
  Result.countStates := 0;
  Result.countAlphabet := 0;
  Result.countFinal := 0;
  Result.countInitial := 0;
  Result.countTransitions := 0;
  Result.classification := '';

  // Conversão dos Estados
  for i := 0 to Length(Data.States) - 1 do
  begin
    if Result.countStates >= MAX_STATES then Break;
    Result.states[Result.countStates] := Data.States[i];
    Inc(Result.countStates);
  end;

  // Conversão do Alfabeto
  for i := 0 to Length(Data.Alphabet) - 1 do
  begin
    if Result.countAlphabet >= MAX_ALPHABET then Break;
    Result.alphabet[Result.countAlphabet] := Data.Alphabet[i];
    Inc(Result.countAlphabet);
  end;

  // Conversão dos Estados Finais
  for i := 0 to Length(Data.FinalStates) - 1 do
  begin
    if Result.countFinal >= MAX_FINAL_STATES then Break;
    Result.finalStates[Result.countFinal] := Data.FinalStates[i];
    Inc(Result.countFinal);
  end;

  // Conversão dos Estado) Iniciais
  for i := 0 to Length(Data.InitialState) - 1 do
  begin
    if Result.countInitial >= MAX_INITIAL_STATES then Break;
    Result.initialState[Result.countInitial] := Data.InitialState[i];
    Inc(Result.countInitial);
  end;

  // Conversão das Transições
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


// Escrita no JSON
procedure SaveAutomatonJSON(const FilePath: string; const A: TAutomaton);
var
  OutputFile: TextFile;
  i: Integer;
begin
  AssignFile(OutputFile, FilePath);
  Rewrite(OutputFile);

  // Abertura do JSON
  WriteLn(OutputFile, '{');
  
  // Escrita do Nome
  WriteLn(OutputFile, '  "nome" : "Automato_' + A.classification + '",');
  WriteLn(OutputFile, ''); 

  // Escrita do Alfabeto 
  Write(OutputFile, '  "alfabeto" : [');
  for i := 0 to A.countAlphabet - 1 do
  begin
    Write(OutputFile, '"' + A.alphabet[i] + '"');
    if i < A.countAlphabet - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');

  // Escrita dos Estados 
  Write(OutputFile, '  "estados" : [');
  for i := 0 to A.countStates - 1 do
  begin
    Write(OutputFile, '"' + A.states[i] + '"');
    if i < A.countStates - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');

  // Escrita dos Estados Finais 
  Write(OutputFile, '  "estados_finais" : [');
  for i := 0 to A.countFinal - 1 do
  begin
    Write(OutputFile, '"' + A.finalStates[i] + '"');
    if i < A.countFinal - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');

  // Escrita do Estado Inicial 
  Write(OutputFile, '  "estado_inicial" : [');
  for i := 0 to A.countInitial - 1 do
  begin
    Write(OutputFile, '"' + A.initialState[i] + '"');
    if i < A.countInitial - 1 then Write(OutputFile, ', ');
  end;
  WriteLn(OutputFile, '],');
  WriteLn(OutputFile, '');

  // Escrita das Transições 
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