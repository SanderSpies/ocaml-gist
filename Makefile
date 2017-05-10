STD_PACKAGES=js_of_ocaml
MERLIN = ./src/ml/merlin_lite/src/
OCAML_VERSION=404

default: gist_tool

clean:
	find . -name '*.cm*' -delete

merlin_lite: clean
	ocamlfind ocamlc -a -o ./build/merlin_lite.cma \
	-package str \
	-package unix \
	-package yojson \
	-package findlib \
	-package js_of_ocaml \
	-I $(MERLIN)sturgeon_null \
	-I $(MERLIN)utils \
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION) \
	-I $(MERLIN)extend \
	-I $(MERLIN)ocaml/support \
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION)/utils \
	-I $(MERLIN)kernel \
	-I $(MERLIN)frontend \
	-I $(MERLIN)analysis \
	-I $(MERLIN)frontend/new \
	-I +compiler-libs \
	$(MERLIN)utils/std.ml \
	$(MERLIN)extend/extend_protocol.ml \
	$(MERLIN)sturgeon_null/sturgeon_stub.ml \
	$(MERLIN)utils/logger.ml \
	$(MERLIN)utils/stat_cache.ml \
	$(MERLIN)utils/file_cache.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/browse_raw.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/raw_compat.ml \
	$(MERLIN)ocaml/support/location_aux.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/saved_parts.mli \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/saved_parts.ml \
	$(MERLIN)ocaml/support/msupport.ml \
	$(MERLIN)kernel/mbrowse.ml \
	$(MERLIN)utils/marg.ml \
	$(MERLIN)utils/ppxsetup.ml \
	$(MERLIN)kernel/msource.ml \
	$(MERLIN)frontend/query_protocol.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/tail_analysis.ml \
	$(MERLIN)analysis/browse_misc.ml \
	$(MERLIN)analysis/type_utils.ml \
	$(MERLIN)analysis/expansion.ml \
	$(MERLIN)analysis/completion.ml

code_execution_webworker.cma: merlin_lite
	ocamlfind ocamlc  -a -o ./build/code_execution_webworker.cma \
	-syntax camlp4o \
	-linkpkg \
	-package js_of_ocaml.syntax \
	-package str \
	-package unix \
	-I $(MERLIN)analysis \
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION) \
	-I $(MERLIN)kernel \
	-I $(MERLIN)frontend \
	-I $(MERLIN)utils \
	-I +compiler-libs \
		./build/merlin_lite.cma \
		./src/ml/code_execution_webworker.ml

code_execution_webworker: code_execution_webworker.cma
	jsoo_mktop \
	./build/code_execution_webworker.cma \
	-o build/code_execution_webworker \
	-jsopt +weak.js -jsopt +toplevel.js -jsopt +dynlink.js -jsopt +nat.js
	mv *.cmis.js ./build/

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

output: gist_tool
