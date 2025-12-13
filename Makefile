# ---------- Configurações ----------
FPC      = fpc
SRC      = src
MAIN     = $(SRC)/main/main.pas
BIN      = bin
TARGET   = $(BIN)/automata
INPUT    = data/input/automato.json

UNIT_DIRS = \
	-Fu$(SRC) \
	-Fu$(SRC)/core \
	-Fu$(SRC)/conversions \
	-Fu$(SRC)/tests

# ---------- Targets ----------
.PHONY: all build run clean

# Comportamento padrão
all: clean build run

build:
	@echo "Compilando projeto..."
	@mkdir -p $(BIN)
	@$(FPC) $(UNIT_DIRS) $(MAIN) -o$(TARGET)

run:
	@echo "Executando o programa..."
	@./$(TARGET) $(INPUT)

clean:
	@echo "Limpando arquivos de compilação..."
	@rm -rf $(BIN)
	@find . -name "*.o" -delete
	@find . -name "*.ppu" -delete
	@echo "Limpeza concluída."
