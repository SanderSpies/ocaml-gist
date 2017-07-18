
.PHONY: build

default: clean build

clean:
	jbuilder clean

build:
	jbuilder build @build

doc: build
	cp _build/default/test.html docs/test.html
	cp _build/default/stdlib2.cmis.js docs/stdlib2.cmis.js
	cp _build/default/code_execution_webworker.js docs/code_execution_webworker.js
	cp _build/default/gist_tool.bc.js docs/gist_tool.bc.js

run:
	jbuilder build @run
