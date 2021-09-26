MiniJuvix  <!-- [![GitHub CI](https://github.com/heliaxdev/MiniJuvix/workflows/CI/badge.svg)](https://github.com/heliaxdev/MiniJuvix/actions) -->
[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE) [![Haskell CI](https://github.com/heliaxdev/MiniJuvix/actions/workflows/haskell.yml/badge.svg?branch=master)](https://github.com/heliaxdev/MiniJuvix/actions/workflows/haskell.yml)
=========


This repository aims to be a conservative Haskell project of a tiny
language with dependent types based on the Juvix project. The primary
purpose is to be a laboratory to study/experiment in implementing a
functional language with dependent types. 

The following is a tentative project structure, but it can change at
any moment. See below the file project structure. The diagram shows
the design of the compiler related with the project structure. 

<p align="center">
<img src="doc/minijuvix.png">
</p>

In the picture, syntax transformations are denoted by pᵢ (e.g. passes in
the Juvix Translate library) and checking operations are denoted by cᵢ
(e.g. found in the Juvix Core library), and both families of items are the priority of this project.


```bash
$ tree src/
...
src
 ├─ app
 │ ├─ Main.hs
 │ └─ Options.hs
 ├─ MiniJuvix
 │ ├─ Pipeline.hs
 │ ├─ Parser
 │ ├─ Syntax
 │ │ ├─ Sugared.hs
 │ │ ├─ Desugared.hs
 │ │ ├─ Core.hs
 │ │ └─ Eval.hs

 │ ├─ Typing
 │ │ ├─ Scopechecking.hs
 │ │ ├─ Coverage.hs
 │ │ ├─ Typechecking.hs
 │ │ ├─ Termination.hs
 │ │ └─ Erasure.hs
 │ └─ Utils
 │   ├─ Parsing.hs
 │   ├─ Pretty.hs
 │   ├─ File.hs
 │   └─ Version.hs
 └─ test
     └─ Spec.hs
...
```
