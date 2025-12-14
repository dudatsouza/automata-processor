unit io;

{$mode objfpc}{$H+}

interface

uses
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
procedure WriteResult(const FilePath: string; const Text: string);
procedure SaveAutomatonJSON(const FilePath: string; var A: TAutomaton);

implementation

function ReadJSON(const FilePath: string): TAutomatonData;
var
  InputFile: TStringList;
  JsonData: TJSONData;
  Obj: TJSONObject;
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
    
    with Obj.Arrays['estado_inicial'] do
    begin
      SetLength(Result.InitialState, Count);
      for i := 0 to Count - 1 do
        Result.InitialState[i] := Strings[i];
    end;


    with Obj.Arrays['estados'] do
    begin
      SetLength(Result.States, Count);
      for i := 0 to Count - 1 do
        Result.States[i] := Strings[i];
    end;

    with Obj.Arrays['alfabeto'] do
    begin
      SetLength(Result.Alphabet, Count);
      for i := 0 to Count - 1 do
        Result.Alphabet[i] := Strings[i];
    end;

    with Obj.Arrays['estados_finais'] do
    begin
      SetLength(Result.FinalStates, Count);
      for i := 0 to Count - 1 do
        Result.FinalStates[i] := Strings[i];
    end;

    // Guardar as transições completas (array de objetos)
    Result.Transitions := Obj.Arrays['transicoes'].Clone as TJSONArray;
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
  // Converter arrays simples
  SetLength(Result.alphabet, Length(Data.Alphabet));
  for i := 0 to High(Data.Alphabet) do
    Result.alphabet[i] := Data.Alphabet[i];

  SetLength(Result.states, Length(Data.States));
  for i := 0 to High(Data.States) do
    Result.states[i] := Data.States[i];

  SetLength(Result.finalStates, Length(Data.FinalStates));
  for i := 0 to High(Data.FinalStates) do
    Result.finalStates[i] := Data.FinalStates[i];

  // Converter estado inicial (que era string) para array
  SetLength(Result.initialState, Length(Data.InitialState));
  for i := 0 to High(Data.InitialState) do
    Result.initialState[i] := Data.InitialState[i];


  // Transições
  SetLength(Result.transitions, Data.Transitions.Count);
  for i := 0 to Data.Transitions.Count - 1 do
  begin
    Obj := Data.Transitions.Objects[i];
    Result.transitions[i].source := Obj.Get('origem', '');
    Result.transitions[i].target := Obj.Get('destino', '');
    Result.transitions[i].symbol := Obj.Get('simbolo', '');
  end;

  Result.classification := '';
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

procedure SaveAutomatonJSON(const FilePath: string; var A: TAutomaton);
var
  JsonObj: TJSONObject;
  AlphabetArray, StatesArray, FinalStatesArray, InitialStateArray, TransitionsArray: TJSONArray;
  TransObj: TJSONObject;
  i: Integer;
  OutputFile: TextFile;
begin
  JsonObj := TJSONObject.Create;
  try
    JsonObj.Add('nome', 'Automato_' + A.classification);

    AlphabetArray := TJSONArray.Create;
    for i := 0 to High(A.alphabet) do
      AlphabetArray.Add(A.alphabet[i]);
    JsonObj.Add('alfabeto', AlphabetArray);

    StatesArray := TJSONArray.Create;
    for i := 0 to High(A.states) do
      StatesArray.Add(A.states[i]);
    JsonObj.Add('estados', StatesArray);

    FinalStatesArray := TJSONArray.Create;
    for i := 0 to High(A.finalStates) do
      FinalStatesArray.Add(A.finalStates[i]);
    JsonObj.Add('estados_finais', FinalStatesArray);

    InitialStateArray := TJSONArray.Create;
    for i := 0 to High(A.initialState) do
      InitialStateArray.Add(A.initialState[i]);
    JsonObj.Add('estado_inicial', InitialStateArray);

    TransitionsArray := TJSONArray.Create;
    for i := 0 to High(A.transitions) do
    begin
      TransObj := TJSONObject.Create;
      TransObj.Add('origem', A.transitions[i].source);
      TransObj.Add('destino', A.transitions[i].target);
      TransObj.Add('simbolo', A.transitions[i].symbol);
      TransitionsArray.Add(TransObj);
    end;
    JsonObj.Add('transicoes', TransitionsArray);

    AssignFile(OutputFile, FilePath);
    Rewrite(OutputFile);
    WriteLn(OutputFile, JsonObj.AsJSON);
    CloseFile(OutputFile);

    WriteLn;
    WriteLn('>> Automato ', A.classification, ' salvo em: ', FilePath);
    WriteLn;
  finally
    JsonObj.Free;
  end;
end;

end.
