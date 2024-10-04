# name of the compiler
OCAMLC = ocamlc

TARGET = test
SRC = pa2.ml
SHELL := /bin/bash

all:
	$(OCAMLC) -o $(TARGET) $(SRC)
tests:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./tests.sh
clean:
	rm -f $(TARGET) *.cmo *.cmi 
simple:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./cool --parse simple.cl
	./$(TARGET) simple.cl-ast