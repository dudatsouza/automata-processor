program Main;

uses
    io, automaton, afne_conversion, afne_afn_conversion, afn_afd_conversion,
    afd_minimization, words_test, utils;

var
    AutomatonObj: TAutomaton;
    option: integer;
    path: string;
    exitProgram: boolean;
    data: TAutomatonData;
    changeInput: boolean;

procedure MenuMultiInitial();
begin
    writeln('--- MENU: MULTI-INICIAL ---');
    writeln('0. Converter multiestado inicial em AFN-e');
    writeln('1. Converter de AFN-e para AFN');
    writeln('2. Converter de AFN para AFD');
    writeln('3. Minimizar AFD');
    writeln('4. Testar palavras');
    writeln('5. Alterar entrada');
    writeln('6. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: ConvertMultiInitialToAFNE(AutomatonObj);
        1: ConvertAFNEToAFN(AutomatonObj);
        2: ConvertAFNToAFD(AutomatonObj);
        3: MinimizeAFD(AutomatonObj);
        4: TestWords(AutomatonObj);
        5: changeInput := true;
        6: exitProgram := true;
    end;
end;

procedure MenuAFNE();
begin
    writeln('--- MENU: AFN-E ---');
    writeln('0. Converter de AFN-e para AFN');
    writeln('1. Converter de AFN para AFD');
    writeln('2. Minimizar AFD');
    writeln('3. Testar palavras');
    writeln('4. Alterar entrada');
    writeln('5. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: ConvertAFNEToAFN(AutomatonObj);
        1: ConvertAFNToAFD(AutomatonObj);
        2: MinimizeAFD(AutomatonObj);
        3: TestWords(AutomatonObj);
        4: changeInput := true;
        5: exitProgram := true;
    end;
end;

procedure MenuAFN();
begin
    writeln('--- MENU: AFN ---');
    writeln('0. Converter de AFN para AFD');
    writeln('1. Minimizar AFD');
    writeln('2. Testar palavras');
    writeln('3. Alterar entrada');
    writeln('4. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: ConvertAFNToAFD(AutomatonObj);
        1: MinimizeAFD(AutomatonObj);
        2: TestWords(AutomatonObj);
        3: changeInput := true;
        4: exitProgram := true;
    end;
end;

procedure MenuAFD();
begin
    writeln('--- MENU: AFD ---');
    writeln('0. Minimizar AFD');
    writeln('1. Testar palavras');
    writeln('2. Alterar entrada');
    writeln('3. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: MinimizeAFD(AutomatonObj);
        1: TestWords(AutomatonObj);
        2: changeInput := true;
        3: exitProgram := true;
    end;
end;

procedure MenuAFDMinimized();
begin
    writeln('--- MENU: AFD MINIMIZADO ---');
    writeln('0. Testar palavras');
    writeln('1. Alterar entrada');
    writeln('2. Sair');
    write('Opcao: ');
    readln(option);

    case option of
        0: TestWords(AutomatonObj);
        1: changeInput := true;
        2: exitProgram := true;
    end;
end;

begin
    exitProgram := false;

    repeat
        writeln;
        writeln('=== CONVERSOR DE AUTOMATOS ===');
        writeln('1. Ler arquivo padrao');
        writeln('2. Escolher outro arquivo');
        writeln('3. Cancelar');
        write('Opcao: ');
        readln(option);

        if option = 1 then
            path := './data/input/afd_para_minimizar.json'
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

        // Read and classify automaton
        data := ReadJSON(path);
        AutomatonObj := ConvertTAutomatonData(data);
        ClassifyAutomaton(AutomatonObj);

        writeln;
        writeln('Automato classificado como: ', AutomatonObj.classification);
        writeln;

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
