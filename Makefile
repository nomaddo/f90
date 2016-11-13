OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlc ${LIB}
LIB=-package sexplib,ppx_sexp_conv
LLIB=

OBJS=location.cmo parse_tree.cmo parser.cmo lexer.cmo calc.cmo

calc: ${OBJS}
	$(OCAMLC) ${LLIB} -linkpkg -linkall -o calc ${OBJS}

test:
	for file in `find test -name *.f90`; do \
	echo $${file}; \
	./calc $${file}; \
	done \

clean:
	rm -rf *.cm? parser.ml parser.mli lexer.ml *.output

parser.cmo: parser.ml parser.cmi
	ocamlc -c $<

parser.mli: parser.mly
	menhir -v $<

%.cmi: %.mli
	${OCAMLC} -c ${LIB} $<
%.cmo: %.ml
	${OCAMLC} -c ${LIB} $<
%.cmx: %.ml
	${OCAMLOPT} -c ${LIB} $<
%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	menhir -v $<

.PHONY: clean test
