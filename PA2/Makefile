# name of the compiler
OCAMLC = ocamlc

TARGET = test
SRC = main.ml
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
arth:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./cool --parse arth.cl
	./$(TARGET) arth.cl-ast
checker:
	$(OCAMLC) -o $(TARGET) $(SRC)
	./cool --parse $(CURRFILE).cl
	./cool --class-map $(CURRFILE).cl
	./$(TARGET) $(CURRFILE).cl-ast
	diff -u  $(CURRFILE).cl-ast-type $(CURRFILE).cl-ast-test