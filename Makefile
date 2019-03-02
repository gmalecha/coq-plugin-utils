plugin: Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq:
	$(COQBIN)coq_makefile -f _CoqProject -o Makefile.coq

clean:
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq

install:
	$(MAKE) -f Makefile.coq install

uninstall:
	$(MAKE) -f Makefile.coq uninstall
