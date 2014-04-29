# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP
headache:
	git ls-files -- src/ test/ | grep -Fv src/sxclient | grep -Fv .mldylib | grep -v ml.ab | grep -Fv netcgi_fcgip.ml | grep -Fv src/files | grep -Fv .mllib | grep -Fv .odocl | grep -Fv META | xargs headache -h _header.server 
	git ls-files -- src/sxclient | grep -Fv .mllib | grep -Fv .mldylib | grep -Fv .odocl | grep -Fv https_clientp.ml | grep -Fv META | xargs headache -h _header.client

metrics.html: build
	find _build -name '*.ml' -print0 | grep -ZzFv myocamlbuild.ml | xargs -0 mascot -config mascot.conf -html metrics.html 

metrics: build
	find _build -name '*.ml' -print0 | grep -ZzFv myocamlbuild.ml | xargs -0 mascot -config mascot.conf -text -

coverage:
	# clean to make sure all files are built with coverage
	# checking enabled
	$(SETUP) -clean
	$(MAKE) cov

cov:
	$(SETUP) -build -tag pkg_bisect -tag pkg_str -tag syntax_camlp4o -tag syntax_bisect_pp $(BUILDFLAGS)
	rm -f bisect*.out
	$(SETUP) -test $(TESTFLAGS)
	ln -s _build/version.ml version.ml
	bisect-report.opt -html coverage.html bisect*.out -I _build
	rm -f version.ml
	x-www-browser ./coverage.html/index.html
update:
	oasis setup
	sed -i -e 's/bindir)/sbindir)/' setup.ml
