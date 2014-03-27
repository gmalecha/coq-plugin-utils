INSTALL_DIR=$(shell coqc -where)/user-contrib/PluginUtils
INSTALL_FILES=plugin_utils.cma plugin_utils.cmx plugin_utils.cmxa plugin_utils.cmi plugin_utils.o

plugin:
	$(MAKE) -C src

install: plugin
	install -d $(INSTALL_DIR)
	install -m 0644 src/myocamlbuild.ml $(INSTALL_DIR)/myocamlbuild.ml
	for x in $(INSTALL_FILES); do \
	  install -m 0644 src/_build/$$x $(INSTALL_DIR); \
	done

uninstall:
	rm -rf $(INSTALL_DIR)
