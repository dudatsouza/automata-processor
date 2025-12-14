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
procedure SaveAutomatonJSON(const FilePath: string; const A: TAutomaton);

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
  Result.countStates := 0;
  Result.countAlphabet := 0;
  Result.countFinal := 0;
  Result.countInitial := 0;
  Result.countTransitions := 0;
  Result.classification := '';

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
    if Result.countStates >= MAX_STATES then Break;
    Result.states[Result.countStates] := Data.States[i];
    Inc(Result.countStates);
  end;

  for i := 0 to Length(Data.Alphabet) - 1 do
  begin
    if Result.countAlphabet >= MAX_ALPHABET then Break;
    Result.alphabet[Result.countAlphabet] := Data.Alphabet[i];
    Inc(Result.countAlphabet);
  end;

  for i := 0 to Length(Data.FinalStates) - 1 do
  begin
    if Result.countFinal >= MAX_FINAL_STATES then Break;
    Result.finalStates[Result.countFinal] := Data.FinalStates[i];
    Inc(Result.countFinal);
  end;

  if Data.InitialState <> '' then
  begin
    Result.initialState[0] := Data.InitialState;
    Result.countInitial := 1;
  end;

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

// ============================================================================
// NOVA VERSÃO: Escrita Manual para Formatação Bonita (Pretty Print)
// ============================================================================
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
  WriteLn(OutputFile, ''); // Linha em branco para separar

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

  // --- Transições (Formatadas linha a linha) ---
  WriteLn(OutputFile, '  "transicoes" : [');
  
  for i := 0 to A.countTransitions - 1 do
  begin
    Write(OutputFile, '    { ');
    Write(OutputFile, '"origem" : "' + A.transitions[i].source + '", ');
    Write(OutputFile, '"destino" : "' + A.transitions[i].target + '", ');
    Write(OutputFile, '"simbolo" : "' + A.transitions[i].symbol + '"');
    Write(OutputFile, ' }');

    // Se não for a última, coloca vírgula e pula linha
    if i < A.countTransitions - 1 then
      WriteLn(OutputFile, ',')
    else
      WriteLn(OutputFile, ''); // Última não tem vírgula
  end;
  
  WriteLn(OutputFile, '  ]');
  
  // Fechamento do JSON
  WriteLn(OutputFile, '}');

  CloseFile(OutputFile);

  WriteLn;
  WriteLn('>> Automato ', A.classification, ' salvo em: ', FilePath);
  WriteLn;
end;

end.