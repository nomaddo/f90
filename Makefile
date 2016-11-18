OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlc ${LIB}
OCAMLDEP=ocamlfind ocamldep
OCAMLYACC=menhir

LIB=-package sexplib,ppx_sexp_conv
LLIB=

OBJS=location.cmo parse_tree.cmo parser.cmo lexer.cmo calc.cmo
OPT=-g

calc: ${OBJS}
	$(OCAMLC) ${LLIB} ${OPT} -linkpkg -linkall -o calc ${OBJS}

all: calc test

depend:
	$(OCAMLDEP) *.ml > .depend

test:
	for file in `find test -name *.f90 | sort`; do \
	echo $${file} ":"; \
	./calc $${file}; \
	done \

clean:
	rm -rf *.cm? parser.ml parser.mli lexer.ml *.output

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

parser.mli: parser.mly
	$(OCAMLYACC) -v $<

%.cmi: %.mli
	${OCAMLC} -c ${LIB} ${OPT} $<
%.cmo: %.ml
	${OCAMLC} -c ${LIB} ${OPT} $<
%.cmx: %.ml
	${OCAMLOPT} -c ${LIB} ${OPT} $<
%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	menhir -v $<

.PHONY: clean test

include .depend
