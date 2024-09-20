# name of the compiler
OCAMLC = ocamlc

TARGET = rosetta
SRC = rosetta.ml
SHELL := /bin/bash

all:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./$(TARGET) < test.txt
tests:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./tests.sh
clean:
	rm -f $(TARGET) *.cmo *.cmi 
