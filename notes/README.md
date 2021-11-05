Tools used so far:

- cabal-edit
- hlint
- stan
- summoner
- ghcup
- `implicit-hie` generates `hie.yaml`.

- For Pretty printer, we will use the package
  https://hackage.haskell.org/package/prettyprinter, which supports
  nice annotations and colors using Ansi-terminal subpackage:
    `cabal-edit add prettyprinter-ansi-terminal`.

- Two options for the kind of container we can use for Context. Using
  the package container and for performance, unordered-containers. The
  latter seems to have the same API naming than the former. So, in
  principle, it doesn't matter which we use.


- See
  [gluedeval](https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784).
  During elaboration different kind of evaluation strategies may be
  needed.
    - top vs. local scope. 
- On equality type-checking, see [abstract](https://github.com/anjapetkovic/anjapetkovic.github.io/blob/master/talks/2021-06-17-TYPES2021/abstract.pdf)
- To document the code, see https://kowainik.github.io/posts/haddock-tips

Initial task order for Minijuvix indicated between parenthesis:
1. Parser (3.)
2. Typechecker   (1.)
3. Compiler (2.)
4. Interpreter (4.)

- On deriving stuff using Standalone Der.
See https://kowainik.github.io/posts/deriving.
- To avoid boilerplate in the cabal file, one could use [common stanzas](https://vrom911.github.io/blog/common-stanzas). At the moment, I'm using cabal-edit to keep the bounds and this tool does not support stanzas. So be it.
- Using MultiParamTypeClasses to allow me deriving multi instances in one line. 

- TODO: make a `ref.bib` listing all the repositories and the source-code from where I took code,
inspiration, whatever thing.