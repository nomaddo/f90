OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlc ${LIB}
LIB=-package sexplib,ppx_sexp_conv
LLIB=

OBJS=parse_tree.cmo parser.cmo lexer.cmo calc.cmo

calc: ${OBJS}
	$(OCAMLC) ${LLIB} -linkpkg -linkall -o calc ${OBJS}

test: calc
	for file in `find test -name *.f90`; do \
	./calc $${file}; \
	done \

clean:
	rm -rf *.cm? parser.ml lexer.ml


parser.cmo: parser.ml
	ocamlc -c $<

%.cmo: %.ml
	${OCAMLC} -c ${LIB} $<
%.cmx: %.ml
	${OCAMLOPT} -c ${LIB} $<
%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	ocamlyacc $<
