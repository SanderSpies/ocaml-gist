STD_PACKAGES=js_of_ocaml

foobar:
	ocamlfind ocamlc -a -o gist.cma \
	-syntax camlp4o \
	-package js_of_ocaml.syntax \
	-package \
		$(STD_PACKAGES) \
	-I +compiler-libs \
		gist.ml

#  -jsopt "--pretty"
# -jsopt "--disable shortvar"
# -verbose
default2: foobar
	jsoo_mktop \
	./gist.cma \
	-o build/foo \
	-jsopt +weak.js -jsopt +toplevel.js -jsopt +dynlink.js -jsopt +nat.js
	mv *.cmis.js ./build/
