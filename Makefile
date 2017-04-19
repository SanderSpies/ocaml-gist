STD_PACKAGES=js_of_ocaml

default: gist_tool

code_execution_webworker.cma:
	ocamlfind ocamlc -a -o ./build/code_execution_webworker.cma \
	-syntax camlp4o \
	-package js_of_ocaml.syntax \
	-package \
		$(STD_PACKAGES) \
	-I +compiler-libs \
		./src/ml/code_execution_webworker.ml

code_execution_webworker: code_execution_webworker.cma
	jsoo_mktop \
	./build/code_execution_webworker.cma \
	-o build/code_execution_webworker \
	-jsopt +weak.js -jsopt +toplevel.js -jsopt +dynlink.js -jsopt +nat.js
	mv *.cmis.js ./build/
	rm src/ml/code_execution_webworker.c*

gist_tool.byte:
	ocamlfind ocamlc -o ./build/gist_tool.byte \
	-syntax camlp4o \
	-linkpkg \
	-package js_of_ocaml.syntax \
	-package \
		$(STD_PACKAGES) \
		./src/ml/gist_tool.ml

gist_tool_js:
	cp ./src/web/codemirror.js build/
	cp ./src/web/codemirror.css build/
	cp ./src/web/ocaml.js build/

gist_tool: gist_tool.byte gist_tool_js code_execution_webworker
	js_of_ocaml \
	./build/gist_tool.byte \
	--opt 3 \
	-o build/gist_tool.js
	rm src/ml/gist_tool.c*

output: gist_tool
