program Main;

uses
    io, automaton, afne_conversion, afne_afn_conversion, afn_afd_conversion,
    afd_minimization, words_test, utils, Crt;

var
    option: integer;
    path: string;
    fileInput: string;
    exitProgram: boolean;
    data: TAutomatonData;
    changeInput: boolean;

procedure Clear();
begin
  writeln();
  writeln('---------------------------------');
  write('Pressione ENTER para continuar...');
  readln;

  ClrScr; 

  ShowAutomatonDetails(AutomatonObj); 
end;

procedure ConfirmConversion(const currentType, targetType, conversionSteps: String; var run: Boolean);
var
  opt: Char;
begin
  writeln;
  writeln('==============================================');
  writeln('AVISO: O automato atual (', currentType, ') nao suporta a operacao diretamente (', targetType, ').');
  writeln('A seguinte cadeia de conversoes sera executada:');
  writeln('  ', conversionSteps);
  writeln('----------------------------------------------');
  write('Deseja prosseguir (S/N)? ');
  readln(opt);

  // Verifica se a resposta começa com 'S' ou 's'
  run := (UpCase(opt) = 'S'); 
end;

procedure MenuMultiInitial();
var
    runConversion: Boolean;
begin
    Clear();
    writeln('--- MENU: MULTI-INICIAL ---');
    writeln('0. Converter multiestado inicial em AFN-e');
    writeln('1. Converter de AFN-e para AFN');
    writeln('2. Converter de AFN para AFD');
    writeln('3. Minimizar AFD');
    writeln('4. Testar palavras');
    writeln('5. Mostrar automato atual'); // << NOVA OPÇÃO
    writeln('6. Alterar entrada');
    writeln('7. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: begin ConvertMultiInitialToAFNE(AutomatonObj); ClassifyAutomaton(AutomatonObj); ShowAutomatonDetails(AutomatonObj); end;
        1: 
        begin
            ConfirmConversion('MULTI-INICIAL', 'AFN', 'MULTI-INICIAL -> AFN-E -> AFN', runConversion);
            if runConversion then
            begin
                ConvertMultiInitialToAFNE(AutomatonObj);
                ConvertAFNEToAFN(AutomatonObj);
            end;
        end;

        2: 
        begin
            ConfirmConversion('MULTI-INICIAL', 'AFD', 'MULTI-INICIAL -> AFN-E -> AFN -> AFD', runConversion);
            if runConversion then
            begin
                ConvertMultiInitialToAFNE(AutomatonObj);
                ConvertAFNEToAFN(AutomatonObj);
                ConvertAFNToAFD(AutomatonObj);
            end;
        end;
        
        3: 
        begin
            ConfirmConversion('MULTI-INICIAL', 'AFD MINIMO', 'MULTI-INICIAL -> AFN-E -> AFN -> AFD -> MINIMIZAR', runConversion);
            if runConversion then
            begin
                ConvertMultiInitialToAFNE(AutomatonObj);
                ConvertAFNEToAFN(AutomatonObj);
                ConvertAFNToAFD(AutomatonObj);
                MinimizeAFD(AutomatonObj);
            end;
        end;
        
        4: TestWords(AutomatonObj);
        5: ShowAutomatonDetails(AutomatonObj);
        6: changeInput := true;
        7: exitProgram := true;
    end;
end;

procedure MenuAFNE();
var
    runConversion: Boolean;
begin
    Clear();
    writeln('--- MENU: AFN-E ---');
    writeln('0. Converter de AFN-e para AFN');
    writeln('1. Converter de AFN para AFD'); 
    writeln('2. Minimizar AFD'); 
    writeln('3. Testar palavras');
    writeln('4. Mostrar automato atual'); 
    writeln('5. Alterar entrada');
    writeln('6. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        // Opção 0: Conversão direta
        0: begin 
            ConvertAFNEToAFN(AutomatonObj); 
        end;

        // Opção 1: Pipeline para AFD
        1: 
        begin
            ConfirmConversion('AFN-E', 'AFD', 'AFN-E -> AFN -> AFD', runConversion);
            if runConversion then
            begin
                ConvertAFNEToAFN(AutomatonObj);
                ConvertAFNToAFD(AutomatonObj);
            end;
        end;

        // Opção 2: Pipeline para AFD Mínimo
        2: 
        begin
            ConfirmConversion('AFN-E', 'AFD MINIMO', 'AFN-E -> AFN -> AFD -> MINIMIZAR', runConversion);
            if runConversion then
            begin
                ConvertAFNEToAFN(AutomatonObj);
                ConvertAFNToAFD(AutomatonObj);
                MinimizeAFD(AutomatonObj);
            end;
        end;
        
        3: TestWords(AutomatonObj);
        4: ShowAutomatonDetails(AutomatonObj); 
        5: changeInput := true;
        6: exitProgram := true;
    end;
end;

procedure MenuAFN();
var
    runConversion: Boolean;
begin
    Clear();
    writeln('--- MENU: AFN ---');
    writeln('0. Converter de AFN para AFD');
    writeln('1. Minimizar AFD'); 
    writeln('2. Testar palavras');
    writeln('3. Mostrar automato atual'); 
    writeln('4. Alterar entrada');
    writeln('5. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        // Opção 0: Conversão direta
        0: begin 
            ConvertAFNToAFD(AutomatonObj); 
        end;

        // Opção 1: Pipeline para AFD Mínimo
        1: 
        begin
            ConfirmConversion('AFN', 'AFD MINIMO', 'AFN -> AFD -> MINIMIZAR', runConversion);
            if runConversion then
            begin
                ConvertAFNToAFD(AutomatonObj);
                MinimizeAFD(AutomatonObj);
            end;
        end;
        
        2: TestWords(AutomatonObj);
        3: ShowAutomatonDetails(AutomatonObj); 
        4: changeInput := true;
        5: exitProgram := true;
    end;
end;

procedure MenuAFD();
begin
    Clear();
    writeln('--- MENU: AFD ---');
    writeln('0. Minimizar AFD');
    writeln('1. Testar palavras');
    writeln('2. Mostrar automato atual'); 
    writeln('3. Alterar entrada');
    writeln('4. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        // Opção 0: Conversão direta
        0: begin 
            MinimizeAFD(AutomatonObj); 
        end;
        
        1: TestWords(AutomatonObj);
        2: ShowAutomatonDetails(AutomatonObj); 
        3: changeInput := true;
        4: exitProgram := true;
    end;
end;

procedure MenuAFDMinimized();
begin
    Clear();
    writeln('--- MENU: AFD MINIMIZADO ---');
    writeln('0. Testar palavras');
    writeln('1. Mostrar automato atual'); 
    writeln('2. Alterar entrada');
    writeln('3. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: TestWords(AutomatonObj);
        1: ShowAutomatonDetails(AutomatonObj); 
        2: changeInput := true;
        3: exitProgram := true;
    end;
end;

begin
    ClrScr;   // limpa o terminal (Windows, macOS, Linux)

    exitProgram := false;

    fileInput := 'multi.json';
    
    repeat
        writeln;
        writeln('=== CONVERSOR DE AUTOMATOS ===');
        writeln('1. Ler arquivo padrao (', fileInput, ')');
        writeln('2. Escolher outro arquivo');
        writeln('3. Cancelar');
        write('Opcao: ');
        readln(option);

        if option = 1 then
            path := './data/input/' + fileInput
        else if option = 2 then
        begin
            write('Informe o nome do arquivo JSON: ');
            readln(path);
            path := './data/input/' + path;
        end
        else if option = 3 then
        begin
            exitProgram := true;
            continue;
        end;

        write('Lendo arquivo: ' + path);

        // Read and classify automaton
        data := ReadJSON(path);
        AutomatonObj := ConvertTAutomatonData(data);
        ClassifyAutomaton(AutomatonObj);

        writeln;
        writeln('Automato classificado como: ', AutomatonObj.classification);

        ShowAutomatonDetails(AutomatonObj);

        // Show correct menu
        exitProgram := false;
        changeInput := false;

        repeat
            if AutomatonObj.classification = 'MULTI-INICIAL' then
                MenuMultiInitial()
            else if AutomatonObj.classification = 'AFN-E' then
                MenuAFNE()
            else if AutomatonObj.classification = 'AFN' then
                MenuAFN()
            else if AutomatonObj.classification = 'AFD' then
                MenuAFD()
            else if AutomatonObj.classification = 'AFD-MINIMO' then
                MenuAFDMinimized()
            else
            begin
                writeln('Tipo de automato desconhecido!');
                exitProgram := true;
            end;

        until exitProgram or changeInput;

    until exitProgram and (not changeInput);

    writeln('Encerrando programa...');
end.
