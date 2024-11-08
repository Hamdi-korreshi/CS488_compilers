# Makefile for compiling and running COOL files

# The OCaml compiler and output executable
OCAMLC = ocamlc
MAIN_EXEC = main
COOL_EXEC = ./cool
OUTPUT_A = a.out
OUTPUT_B = b.out

# Ensure commands run every time
.PHONY: %.out run clean

# Pattern rule to make the COOL file name parameterizable
%.out: %.cl
	# Compile main.ml to main executable
	$(OCAMLC) -o $(MAIN_EXEC) main.ml

	# Parse the COOL file using ./cool --parse
	$(COOL_EXEC) --parse $<

	# Run the COOL file and save the output to a.out
	$(COOL_EXEC) $< > $(OUTPUT_A)

	# Run main with the COOL file and save output to -test.cl-tac
	./$(MAIN_EXEC) $< > $<-test.cl-tac

	# Run the generated -test.cl-tac file and save output to b.out
	$(COOL_EXEC) $<-test.cl-tac > $(OUTPUT_B)

	# Compare a.out and b.out and display if there are differences
	@diff $(OUTPUT_A) $(OUTPUT_B) > /dev/null; \
	if [ $$? -eq 0 ]; then \
		echo "No differences found"; \
	else \
		echo "Differences found"; \
	fi

# Target to run the Makefile with a filename parameter
# Usage: make run FILE=filename.cl
run:
	$(MAKE) $(FILE).out

# Clean up generated files
clean:
	rm -f $(MAIN_EXEC) *.out *-test.cl-tac
