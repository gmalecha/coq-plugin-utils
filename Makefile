INSTALL_DIR=$(shell coqc -where)/user-contrib/PluginUtils
INSTALL_FILES=plugin_utils.cma plugin_utils.cmx plugin_utils.cmxa plugin_utils.cmi plugin_utils.o

plugin:
	$(MAKE) -C src

install: plugin
	install -d $(INSTALL_DIR)
	install -m 0644 src/myocamlbuild.ml $(INSTALL_DIR)/myocamlbuild.ml
	ocamlfind install -destdir $(INSTALL_DIR) coq-plugin-utils META $(INSTALL_FILES:%=src/_build/%)

uninstall:
	ocamlfind remove coq-plugin-utils
	rm -f $(INSTALL_DIR)/myocamlbuild.ml
