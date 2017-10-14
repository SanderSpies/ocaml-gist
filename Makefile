
.PHONY: build clean test

build:
	jbuilder build @install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install

doc: build
	cp _build/default/ocaml_gist/index.html docs/index.html
	cp _build/default/ocaml_gist/stdlib.js docs/stdlib.js
	cp _build/default/ocaml_gist/ocaml_webworker.js docs/ocaml_webworker.js
	cp _build/default/ocaml_gist/ocaml_gist.bc.js docs/ocaml_gist.bc.js
	cp _build/default/ocaml_gist/codemirror.js docs/codemirror.js
	cp _build/default/ocaml_gist/codemirror.css docs/codemirror.css
	cp _build/default/ocaml_gist/error.svg docs/error.svg
	cp _build/default/ocaml_gist/execute.svg docs/execute.svg
	cp _build/default/ocaml_gist/ocaml.js docs/ocaml.js
	cp _build/default/ocaml_gist/show-hint.js docs/show-hint.js
	cp _build/default/ocaml_gist/active-line.js docs/active-line.js

dev_env:
	cp _build/default/ocaml_gist/stdlib.js ocaml_gist/src/stdlib.js
	cp _build/default/ocaml_gist/ocaml_webworker.js ocaml_gist/src/ocaml_webworker.js

dev: build
	opam pin remove ocaml-webworker
	opam pin remove ocaml-gist
	opam pin add -y ocaml-webworker .
	opam pin add -y ocaml-gist .

run:
	jbuilder build @run
