INSTALL_DIR=$(shell coqc -where)/user-contrib/PluginUtils
INSTALL_FILES=plugin_utils.cma plugin_utils.cmxs plugin_utils.cmi

plugin:
	$(MAKE) -C src

install: plugin
	for i in $(INSTALL_FILES); do \
	  install -d `dirname $(INSTALL_DIR)/$$i`; \
	  install -m 0644 src/_build/$$i $(INSTALL_DIR)/$$i; \
	done
