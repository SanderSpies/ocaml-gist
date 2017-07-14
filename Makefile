STD_PACKAGES=js_of_ocaml
MERLIN = ./src/ml/merlin_lite/src/
OCAML_VERSION=404

default: gist_tool

clean:
	find . -name '*.cm*' -delete

stdlib: clean
	ocamlfind ocamlc -package findlib findlib.cma ./src/ml/build/stdlib_builder.ml -o stdlib_builder
	./stdlib_builder

merlin_lite: clean
	ocamlfind ocamlc -a -o ./docs/merlin_lite.cma \
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
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION)/typing \
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION)/parsing \
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
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/parsing/pprintast2.ml \
	$(MERLIN)ocaml/support/msupport.ml \
	$(MERLIN)ocaml/support/path_aux.ml \
	$(MERLIN)kernel/mbrowse.ml \
	$(MERLIN)utils/marg.ml \
	$(MERLIN)utils/ppxsetup.ml \
	$(MERLIN)kernel/msource.ml \
	$(MERLIN)kernel/mconfig.ml \
	$(MERLIN)kernel/mreader.ml \
	$(MERLIN)frontend/query_protocol.ml \
	$(MERLIN)frontend/query_json.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/tail_analysis.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/typing/tast_helper.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/typing/untypeast2.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/typing/parmatch2.ml \
	$(MERLIN)ocaml/typer_$(OCAML_VERSION)/parsing/longident2.ml \
	$(MERLIN)analysis/browse_tree.ml \
	$(MERLIN)analysis/browse_misc.ml \
	$(MERLIN)analysis/type_utils.ml \
	$(MERLIN)analysis/expansion.ml \
	$(MERLIN)analysis/completion.ml \
	$(MERLIN)analysis/destruct.ml \
	$(MERLIN)analysis/ocamldoc.ml \
	$(MERLIN)analysis/outline.ml \
	$(MERLIN)ocaml/support/cmt_cache.ml \
	$(MERLIN)analysis/typedtrie.ml \
	$(MERLIN)utils/misc2.ml \
	$(MERLIN)analysis/track_definition.ml


code_execution_webworker.cma: merlin_lite stdlib
	ocamlfind ocamlc -a -o ./docs/code_execution_webworker.cma \
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
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION)/typing \
	-I $(MERLIN)ocaml/typer_$(OCAML_VERSION)/parsing \
	-I +compiler-libs \
		./docs/merlin_lite.cma \
		./src/ml/code_execution_webworker.ml

code_execution_webworker: code_execution_webworker.cma
	jsoo_mktop \
	./docs/code_execution_webworker.cma \
	-o docs/code_execution_webworker \
	-jsopt +weak.js -jsopt +toplevel.js -jsopt +dynlink.js -jsopt +nat.js \
	-jsopt "--pretty" \
	-jsopt "--disable shortvar"
	mv *.cmis.js ./docs/

gist_tool.byte:
	ocamlfind ocamlc -o ./docs/gist_tool.byte \
	-syntax camlp4o \
	-linkpkg \
	-package js_of_ocaml.syntax \
	-package \
		$(STD_PACKAGES) \
		./src/ml/gist_tool.ml

gist_tool_js:
	cp ./src/web/codemirror.js docs/
	cp ./src/web/codemirror.css docs/
	cp ./src/web/ocaml.js docs/

gist_tool: gist_tool.byte gist_tool_js code_execution_webworker
	js_of_ocaml \
	./docs/gist_tool.byte \
	--opt 3 \
	-o docs/gist_tool.js

output: gist_tool
