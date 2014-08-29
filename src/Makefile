OCAMLBUILDDIR=$(shell ocamlfind query ocamlbuild)

all: myocamlbuild.ml plugin PluginUtils.vo


plugin:
	ocamlbuild coq.otarget

PluginUtils.vo: plugin PluginUtils.v
	coqc -I . -R . PluginUtils PluginUtils

myocamlbuild.ml: ../myocamlbuild.template
	@ echo 'let coq_config_string = "'`coqc -config | tr '\n' '|'`'";; ' > myocamlbuild.ml
	@ echo >> myocamlbuild.ml
	@ cat ../myocamlbuild.template >> myocamlbuild.ml

clean:
	@ ocamlbuild -clean
	@ rm -f myocamlbuild.ml

-include ../.coq_config
