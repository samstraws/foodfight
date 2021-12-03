MODULES=food category move player state command display main autoState
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,ounit2,str,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

clean:
	ocamlbuild -clean
	rm -rf doc
	rm -rf src.zip doc.public doc.private _coverage bisect*.coverage

zip:
	zip src.zip *.ml* *.txt _tags Makefile

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

docs: docs-public

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)
