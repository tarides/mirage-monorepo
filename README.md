
## Steps to create the monorepo

Install `opam monorepo`.
It doesn't install yet on 5.0.0, but you can install it on a different switch and reuse the binary.

```
opam monorepo lock
opam monorepo pull
git apply patches/httpaf_eio.patch
dune build ./example
```
