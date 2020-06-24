all:
	@echo "make compile -Para compilar"
	@echo "make run FILE=file.je -Para escuchar file.je"
	@echo "make clean -Para limpiar los archivos"

compile:
	@echo "Compilando módulos"
	ghc --make -dynamic Main.hs -o MP

run:
	@echo "Creando sonido..."
	./MP ${FILE}

clean:
	@echo "Limpiando archivos"
	@rm MP *.o *.bin *.hi
