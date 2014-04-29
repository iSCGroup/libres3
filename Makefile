all test install uninstall clean: myocamlbuild.ml
	@export \
	    OCAMLFIND_DESTDIR=`pwd`/_build/lib/ocaml/site-lib \
	    OCAMLPATH=`pwd`/_build/lib/ocaml/site-lib:$$OCAMLPATH \
	    CAML_LD_LIBRARY_PATH=`pwd`/_build/lib/ocaml/site-lib/stublibs:$$CAML_LD_LIBRARY_PATH \
	    PATH=`pwd`/_build/bin:$$PATH \
	    CPPFLAGS="`cat config.cppflags`" LDFLAGS="`cat config.ldflags`" &&\
	$(MAKE) $@.target
reinstall:
	$(MAKE) uninstall
	$(MAKE) install
check: test

distclean:
	cd libres3 && ocaml setup.ml -distclean -quiet
	ocamlbuild -clean -quiet
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	@echo "Please run './configure' first"
	@false

all.target test.target install.target reinstall.target uninstall.target clean.target:
	ocamlbuild $@ -j 0

README.html: README
	pandoc --toc -s --smart -t html5 --data-dir=`pwd` -H foghorn.css -f markdown $< -o $@

.PHONY: all test check install uninstall reinstall clean distclean all.target test.target check.target install.target uninstall.target reinstall.target clean.target distclean.target
