OBUILD = ocamlbuild -use-ocamlfind
ALL = mParser.cma charStream.cma
INSTALL_FILES = mParser.cma mParser.cmxa mParser.cmi mParser.mli mParser.a charStream.cma charStream.cmxa charStream.cmi charStream.mli charStream.a

all: all.opt all.byte

all.byte::
	$(OBUILD) $(ALL)

all.opt::
	$(OBUILD) $(ALL) $(ALL:.cma=.cmxa)

docs:
	$(OBUILD) mParser.docdir/index.html

clean:
	ocamlbuild -clean

install: all
	cd _build && $(MAKE) -f ../Makefile realinstall

realinstall:
	ocamlfind install mParser ../META $(INSTALL_FILES)

uninstall:
	ocamlfind remove mParser
