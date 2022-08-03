
## Steps to create the monorepo

Install `opam monorepo`.
It doesn't install yet on 5.0.0, but you can install it on a different switch and reuse the binary.

Ensure you have the alpha and duniverse repositories configured:

```
$ opam remote
[NOTE] These are the repositories in use by the current switch. Use '--all' to see all configured repositories.

<><> Repository configuration for switch 5.0.0~alpha1 <><><><><><><><><><><><><>
 1 dune-universe git+https://github.com/dune-universe/opam-overlays.git
 2 alpha         git+https://github.com/kit-ty-kate/opam-alpha-repository.git
 3 default       https://opam.ocaml.org
```

```
opam monorepo lock
opam monorepo pull
git checkout 754836818dc5c -- duniverse/eio duniverse/ocaml-cohttp duniverse/luv duniverse/httpaf
dune build ./example
```
