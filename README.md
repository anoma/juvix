MiniJuvix  <!-- [![GitHub CI](https://github.com/heliaxdev/MiniJuvix/workflows/CI/badge.svg)](https://github.com/heliaxdev/MiniJuvix/actions) -->
[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE) [![Haskell CI](https://github.com/heliaxdev/MiniJuvix/actions/workflows/ci.yml/badge.svg?branch=qtt)](https://github.com/heliaxdev/MiniJuvix/actions/workflows/ci.yml)
=========

This repository aims to be a conservative Haskell project of a tiny
language with dependent types based on the Juvix project. The primary
purpose is to be a laboratory to study/experiment in implementing a
functional language with dependent types. 

In this branch `qtt`, MiniJuvix aims to be programming language based
on Quantitative type theory. Some Haskell code has been generated
automatically by `agda2hs`. In the future, we want to prove properties
about the Minijuvix programs directly in Agda.

The following is a tentative project structure, but it can change at
any moment. See below the file project structure. The diagram shows
the design of the compiler related with the project structure. 

<p align="center">
<img src="docs/minijuvix.png">
</p>

In the picture, syntax transformations are denoted by pᵢ and checking
operations are denoted by cᵢ .

