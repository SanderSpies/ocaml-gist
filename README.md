ocaml-gist
===
Tooling to assist with OCaml gist experiences on the web.

Description
---
With ocaml-gist you can demonstrate OCaml code on the web. This code can be
edited and executed without needing a server. It also provides type
assisted features: autocomplete with documentation and type info on hover.

Demo 1: https://sanderspies.github.io/ocaml-gist/output_stdlib/index.html

Demo 2: https://sanderspies.github.io/ocaml-gist/output_base/index.html

Do note that everything is still a work in progress.

Usage
---
Requirements: OCaml 4.04.2 for now. 4.02.3 should work also, but not tested
in a while. Other versions not supported yet.

Install ocaml-gist
```
opam pin add -y ocaml-webworker https://github.com/SanderSpies/ocaml-gist.git
opam pin add -y ocaml-gist https://github.com/SanderSpies/ocaml-gist.git
```

Create an examples folder for your project.

Either use the `og-create` command directly:

```
og-create
   --lib lib1
   --lib lib2
   --input examples_folder
   --output output_folder
   --doc
```

or via jbuilder:

```
(alias (
  (name bla)
  (deps (foo.cma))
  (action (progn
    (run og-create
         --lib lib1
         --input examples_folder
         --output output_folder
         --doc
    )
  ))
))
```

(see also https://github.com/SanderSpies/ocaml-gist/test/jbuild)

Run the `output_folder` in a webserver and go to the `index.html` page on
your website.

Acknowledgements
---
OCaml Labs - for sponsoring my work on this
Merlin - most of the code in ocaml_webworker/merlin_lite comes from Merlin
Js_of_ocaml - ocaml_webworker/cmti_bundler.ml is based on code from Jsoo

Demo's
---
The code for these demo's is in: /test.

https://sanderspies.github.io/ocaml-gist/output_stdlib/index.html

https://sanderspies.github.io/ocaml-gist/output_base/index.html

https://sanderspies.github.io/ocaml-gist/output_core_kernel_no_doc/index.html
(note: core_kernel with documentation results in JS files over 150MB and can't be uploaded to GitHub)
(note 2: core_kernel needs to be compiled with a few changes)

https://sanderspies.github.io/ocaml-gist/output_lwt/index.html

License
===
MIT
