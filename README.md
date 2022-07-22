
## Steps to create the monorepo

* `opam pin -n cohttp-eio.dev git+https://github.com/mirage/ocaml-cohttp.git`
* `opam pin -n http.dev git+https://github.com/mirage/ocaml-cohttp.git`
* `opam monorepo lock`
* `opam monorepo pull`

