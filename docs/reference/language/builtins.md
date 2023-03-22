# Built-ins

Juvix has support for the built-in natural type and a few functions that
are compiled to efficient primitives.

1.  Built-in inductive definitions.

    ```juvix
    builtin nat
    type Nat :=
      zero : Nat |
      suc : Nat → Nat;
    ```

2.  Builtin function definitions.

    ```juvix
    infixl 6 +;
    builtin nat-plus
    + : Nat → Nat → Nat;
    + zero b := b;
    + (suc a) b := suc (a + b);
    ```

3.  Builtin axiom definitions.

    ```juvix
    builtin nat-print
    axiom printNat : Nat → Action;
    ```
