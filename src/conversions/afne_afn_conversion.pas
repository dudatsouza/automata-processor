unit afne_afn_conversion;

interface

uses
  automaton, io;

procedure ConvertAFNEToAFN(var A: TAutomaton);

implementation

// ------------------------------------------------------------
// Compute the epsilon-closure of a state
// ------------------------------------------------------------
procedure ComputeEpsilonClosure(const state: AnsiString; var A: TAutomaton; var closure: array of boolean);
var
  i, sIndex: Integer;
begin
  // Find index of the state
  sIndex := -1;
  for i := 0 to High(A.states) do
    if A.states[i] = state then
    begin
      sIndex := i;
      break;
    end;

  if sIndex = -1 then
    Exit;

  if closure[sIndex] then
    Exit; // already visited

  closure[sIndex] := True;

  // Explore epsilon transitions
  for i := 0 to High(A.transitions) do
  begin
    if (A.transitions[i].source = state) and (A.transitions[i].symbol = 'ε') then
      ComputeEpsilonClosure(A.transitions[i].target, A, closure);
  end;
end;

// ------------------------------------------------------------
// Convert AFN-e to AFN
// ------------------------------------------------------------
procedure ConvertAFNEToAFN(var A: TAutomaton);
var
  i, j, sIdx: Integer;
  closure: array of boolean;
  newTransitions: array of TTransition;
  newCount: Integer;
  origin, target: AnsiString;
begin
  writeln('>> Convertendo automato AFN-E para AFN...');

  // --------------------------------------------------------
  // Step 1: Build epsilon-closures
  // --------------------------------------------------------
  SetLength(closure, Length(A.states));
  newCount := 0;
  SetLength(newTransitions, 0);

  // --------------------------------------------------------
  // Step 2: For each state, compute closure and generate transitions
  // --------------------------------------------------------
  for sIdx := 0 to High(A.states) do
  begin
    // Reset closure
    for i := 0 to High(closure) do
      closure[i] := False;

    // Compute epsilon-closure of A.states[sIdx]
    ComputeEpsilonClosure(A.states[sIdx], A, closure);

    // For every state reachable in closure
    for i := 0 to High(A.states) do
    begin
      if not closure[i] then
        Continue;

      origin := A.states[sIdx];

      // For every non-ε transition from closure state
      for j := 0 to High(A.transitions) do
      begin
        if (A.transitions[j].source = A.states[i]) and (A.transitions[j].symbol <> 'ε') then
        begin
          target := A.transitions[j].target;

          // Add new transition
          SetLength(newTransitions, newCount + 1);
          newTransitions[newCount].source := origin;
          newTransitions[newCount].symbol := A.transitions[j].symbol;
          newTransitions[newCount].target := target;
          Inc(newCount);
        end;
      end;
    end;
  end;

  // --------------------------------------------------------
  // Step 3: Update final states
  // Any state whose closure contains a final state must be final
  // --------------------------------------------------------
  for sIdx := 0 to High(A.states) do
  begin
    for i := 0 to High(closure) do
      closure[i] := False;

    ComputeEpsilonClosure(A.states[sIdx], A, closure);

    for i := 0 to High(A.states) do
      if closure[i] then
        if (A.states[i] = '') then Continue;

    for i := 0 to High(A.states) do
    begin
      if closure[i] then
      begin
        // If closure contains a final state -> mark state as final
        for j := 0 to High(A.finalStates) do
          if A.finalStates[j] = A.states[i] then
          begin
            SetLength(A.finalStates, Length(A.finalStates) + 1);
            A.finalStates[High(A.finalStates)] := A.states[sIdx];
            Break;
          end;
      end;
    end;
  end;

  // --------------------------------------------------------
  // Replace transitions
  // --------------------------------------------------------
  A.transitions := newTransitions;

  // Update classification
  A.classification := 'AFN';

  writeln('>> Conversao AFN-E para AFN executada com sucesso!');
  SaveAutomatonJSON('./data/output/AFN.json', A);
end;

end.
