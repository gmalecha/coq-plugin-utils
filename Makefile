plugin:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	@ rm .coq_config

install:
	install -d $(INSTALL_DIR)
	install -m 0644 src/myocamlbuild.ml $(INSTALL_DIR)/myocamlbuild.ml
	for x in $(INSTALL_FILES); do \
	  install -m 0644 src/_build/$$x $(INSTALL_DIR); \
	done
	install -m 0644 src/PluginUtils.vo $(INSTALL_DIR)

uninstall:
	rm -rf $(INSTALL_DIR)

.coq_config:
	@ coqc -config > .coq_config

include .coq_config

INSTALL_DIR=$(COQLIB)/user-contrib/PluginUtils
INSTALL_FILES=plugin_utils.cma plugin_utils.cmx plugin_utils.cmxs plugin_utils.cmxa plugin_utils.cmi plugin_utils.o
