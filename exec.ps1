Write-Host "Iniciando build limpo..."

$SRC  = "src"
$MAIN = "src/main/main.pas"
$BIN  = "bin"
$EXE  = "$BIN/automata.exe"

function Clean {
    Write-Host "Limpando arquivos de compilação..."

    if (Test-Path $BIN) {
        Remove-Item -Recurse -Force $BIN
    }

    Get-ChildItem -Recurse -Include *.o, *.ppu -ErrorAction SilentlyContinue |
        Remove-Item -Force

    Write-Host "Limpeza concluída."
}

# Clean explícito
if ($args.Length -gt 0 -and $args[0] -eq "clean") {
    Clean
    exit 0
}

# Clean automático antes do build
Clean

Write-Host "Compilando projeto..."

# Recria pasta bin
New-Item -ItemType Directory -Path $BIN -Force | Out-Null

$CMD = "fpc -Fusrc -Fusrc/core -Fusrc/conversions `"$MAIN`" -o`"$EXE`""

Write-Host "Executando: $CMD"
Invoke-Expression $CMD

if ($LASTEXITCODE -ne 0) {
    Write-Host "Erro na compilação!"
    exit 1
}

Write-Host "Compilado com sucesso!"
Write-Host "Executando o programa..."

& $EXE
