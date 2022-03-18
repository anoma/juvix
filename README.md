MiniJuvix  <!-- [![GitHub CI](https://github.com/heliaxdev/MiniJuvix/workflows/CI/badge.svg)](https://github.com/heliaxdev/MiniJuvix/actions) -->
[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE) [![Haskell CI](https://github.com/heliaxdev/MiniJuvix/actions/workflows/ci.yml/badge.svg?branch=qtt)](https://github.com/heliaxdev/MiniJuvix/actions/workflows/ci.yml)
====

Description
-----------

MiniJuvix is a dependently functional programming language for writing
efficient formally-verified [validity predicates](https://anoma.network/blog/validity-predicates/), which can be
deployed to various distributed ledgers.  This is a software released for experimentation and research purposes
only. No warranty is provided or implied.

MiniJuvix addresses many issues that we have
experienced while trying to write and deploy decentralised
applications present in the ecosystem of smart-contracts:

- the difficulty of
adequate program verification,
- the ceiling of compositional
complexity,
- the illegibility of execution costs, and
- the lock-in to
particular backends.

Quick Start
-----------

* Install [Stack](https://haskellstack.org):

 ````bash
 $ git clone https://github.com/heliaxdev/minijuvix.git
 $ cd minijuvix
 $ stack install
````

* How to install stack? if it's not installed.

  - For Ubuntu        : `apt install stack`
  - For Debian        : `apt install haskell-stack`
  - For Arch Linux    : `pacman -S stack`
  - For macOS : `brew install haskell-stack`
  - For Windows, following the instructions
  [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).

  It is required at least 8GB RAM for `stack` installation.

* To install MiniJuvix, you can download its sources using
 [Git](http://git-scm.com/) from the [Github
 repository](https://github.com/anoma/juvix.git). Then, the program
 can be downloaded and installed with the following commands:

 If the installation succeeds, you must be able to run the `minijuvix` command
 from any location. To get the complete list of commands, please run `minijuvix --help`.


Testing
-------

 ````bash
 $ stack test
 ````

Community
---------

We would love to hear what you think of MiniJuvix! Join us on [Discord](https://discord.gg/nsGaCZzJ)

