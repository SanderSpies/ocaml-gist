
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
	cp _build/default/ocaml_gist/ocaml.js docs/ocaml.js

run:
	jbuilder build @run
