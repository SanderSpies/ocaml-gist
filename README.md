OCaml-gist
===
Tooling to assist with OCaml editor experiences on the web.

Warning
---
Work in progress. I'm trying to get it a shape where it's usable by others.

The following text is incorrect for now.   

Creating a standard library
---
By default the `og-stdlib` takes all the `.cmti` files in the directory where
compiler-libs is located and runs `jsoo_mkcmis` on them. It then combines
them to a single `stdlib.cmis.js` file.

This file is expected to be loaded by the OCaml-webworker.

Options:
- `-stdlib ./foo/bar`
  To change the source of the stdlib.
- `-package foo`
  To add a package to the stdlib.

Creating a webworker
---
The `og-webworker` command creates a webworker which expects a `stdlib.cmis.js`
file to be present in the same directory.

This webworker is expected to be loaded by another tool, like the gist tool
below.

It supports the following commands:
- type
- execute
- complete_prefix
- locate
- outline
- shape
- documentation

Currently it doesn't support any command-line options.

Gist tool for libraries
---
To create a gist for a set of example files run `og-generate`.

By default it will take all the `.ml` files in the current folder and
create an online example experience.

Demo
---
https://sanderspies.github.io/ocaml-gist/index.html

License
===
MIT
