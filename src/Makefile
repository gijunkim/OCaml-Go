MODULES=game graphic main menu
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST = test.byte
MAIN=main.byte
MENU = menu.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS = oUnit

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug -pkgs $(PKGS) $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MENU) && ./$(MENU)

zip:
	zip a6src.zip *.ml* *.json _tags Makefile
	
docs: docs-public
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.public $(MLS)


