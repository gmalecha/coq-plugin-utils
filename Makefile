all: Makefile.coq
	@ $(MAKE) -f Makefile.coq all

clean: Makefile.coq
	@ $(MAKE) -f Makefile.coq clean
	@ rm Makefile.coq

install: all
	@ $(MAKE) -f Makefile.coq install

SRCS=$(wildcard src/*.ml) \
     $(wildcard src/*.mli) \
     $(wildcard src/*.ml4) \
     $(wildcard src/*.mllib) \
     $(wildcard src/*.v) \
     $(wildcard test-suite/*.v)

Makefile.coq: Makefile
	@ coq_makefile -I src -R src PluginUtils $(SRCS) -o Makefile.coq
