OCAMLBUILDDIR=$(shell ocamlfind query ocamlbuild)

all: _build/coq_builder
	@ ./_build/coq_builder -no-plugin coq.otarget

_build/coq_builder: myocamlbuild.ml coq_paths.ml
	@ echo "building builder...."
	@ ocamlbuild -cflags -I,`ocamlfind query ocamlbuild` -no-plugin myocamlbuild.cmo
	@ ocamlc.opt unix.cma -I $(OCAMLBUILDDIR) $(OCAMLBUILDDIR)/ocamlbuildlib.cma \
	   _build/coq_paths.cmo _build/myocamlbuild.cmo \
	   $(OCAMLBUILDDIR)/ocamlbuild.cmo -o _build/coq_builder
	@ echo "done"

clean:
	ocamlbuild -clean
