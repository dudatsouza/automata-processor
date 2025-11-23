Write-Host "Compilando projeto..."

$SRC = "src"
$MAIN = "src/main/main.pas"
$BIN = "bin"

if (!(Test-Path $BIN)) {
    New-Item -ItemType Directory -Path $BIN | Out-Null
}

$EXE = "$BIN/automata.exe"

$CMD = "fpc -Fusrc -Fusrc/core -Fusrc/conversions -Fusrc/tests `"$MAIN`" -o`"$EXE`""

Write-Host "Executando: $CMD"
Invoke-Expression $CMD

if ($LASTEXITCODE -ne 0) {
    Write-Host "Erro na compilação!"
    exit 1
}

Write-Host "Compilado com sucesso!"
Write-Host "Executando o programa..."

& $EXE
