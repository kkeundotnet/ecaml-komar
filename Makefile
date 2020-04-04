.PHONY: default build check test install uninstall clean

default: build

build:
	dune build

check:
	dune build @check

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq
