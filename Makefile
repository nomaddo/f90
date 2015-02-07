OCAML=ocamlc

all:
	$(OCAML) -c parse_tree.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	$(OCAML) parser.mli
	$(OCAML) -o calc parse_tree.ml parser.ml lexer.ml calc.ml
