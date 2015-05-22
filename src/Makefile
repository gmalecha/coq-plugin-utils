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
	@ rm -rf myocamlbuild.ml .coq-native

-include ../.coq_config
