OCAML=ocamlc

all:
	$(OCAML) -c parse_tree.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	$(OCAML) -c parser.mli
	$(OCAML) -c parser.ml
	$(OCAML) -c lexer.ml
	$(OCAML) -c calc.ml
	$(OCAML) -o calc parse_tree.cmo parser.cmo lexer.cmo calc.cmo
