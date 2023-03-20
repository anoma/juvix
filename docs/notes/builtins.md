---
author: Jan Mas Rovira
title: Builtins
---

# Overview

The goal is to support builtin types and functions that get compiled to
efficient primitives. We plan on supporting primitives for the following
types of definitions:

1.  Builtin inductive definitions. For example:

    ```juvix
    builtin nat
    type Nat :=
      zero : Nat |
      suc : Nat → Nat;
    ```

    We will call this the canonical definition of natural numbers.

2.  Builtin function definitions. For example:

    ```juvix
    inifl 6 +;
    builtin nat-plus
    + : Nat → Nat → Nat;
    + zero b := b;
    + (suc a) b := suc (a + b);
    ```

3.  Builtin axiom definitions. For example:

    ```juvix
    builtin nat-print
    axiom printNat : Nat → Action;
    ```

## Collecting builtin information

The idea is that builtin definitions are treated normally throughout the
pipeline except in the backend part. There is one exception to that. We
need to collect information about the builtins that have been included
in the code and what are the terms that refer to them. For instance,
imagine that we find this definitions in a juvix module:

```juvix
builtin nat
type MyNat :=
    z : MyNat |
    s : MyNat → MyNat;
```

We need to take care of the following:

1.  Check that the definition `MyInt` is up to renaming equal to the
    canonical definition that we provide in the compiler.
2.  Rember a map from concrete to canonical names: {MyNat ↦ Nat; z ↦
    zero; s ↦ suc};
3.  Rembember that we have a definition for builtin natural numbers.
    This is necessary if later we attempt to define a builtin function
    or axiom that depends on natural numbers.

In the compiler we need to know the following:

1.  For inductives:
    1.  What is the primitive type that we will target in the backend:
        E.g. {Nat ↦ int}.
    2.  For constructors:
        1.  What is the primitive constructor function: E.g. {zero ↦ 0;
            suc ↦ plus<sub>one</sub>};
        2.  How to test if a term matches a pattern with that
            constructor. E.g. {zero ↦ is<sub>zero</sub>; suc ↦
            is<sub>notzero</sub>};
        3.  How to deconstruct/project each of the constructor
            arguments. E.g. {zero ↦ ∅; suc ↦ minus<sub>one</sub>}}. Note
            that if a constructor takes multiple arguments we will need
            to have a projection function for each argument.
2.  For functions and axioms:
    1.  What is the primitive function that we will target in the
        backend: E.g. {+ ↦ add}.
