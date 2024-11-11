# Makefile for compiling and running COOL files

# The OCaml compiler and output executable
OCAMLC = ocamlc
MAIN_EXEC = main

# Target to run the Makefile with a filename parameter
# Usage: make run FILE=filename.cl
all:
	$(OCAMLC) main.ml -o $(MAIN_EXEC)

# Clean up generated files
clean:
	rm -f $(MAIN_EXEC) *.cmi *.cmo
