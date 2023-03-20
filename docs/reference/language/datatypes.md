# Data types

A data type declaration consists of:

- the `type` keyword,
- a unique name for the type,
- the `:=` symbol,
- a non-empty list of constructor declarations – functions for
  building the elements of the data type.

The simplest data type is the `Unit` type with one constructor called
`unit`.

```juvix
type Unit := unit : Unit;
```

In the following example, we declare the type `Nat` – the unary
representation of natural numbers. This type comes with two
constructors: `zero` and `suc`. Example elements of type `Nat` are the
number one represented by `suc zero`, the number two represented by
`suc (suc zero)`, etc.

```juvix
type Nat :=
    zero : Nat
  | suc : Nat -> Nat;
```

Constructors can be used like normal functions or in patterns when
defining functions by pattern matching. For example, here is a function
adding two natural numbers:

```juvix
infixl 6 +;
+ : Nat -> Nat -> Nat;
+ zero b := b;
+ (suc a) b := suc (a + b);
```

A data type may have type parameters. A data type with a type parameter
`A` is called _polymorphic in_ `A`. A canonical example is the type
`List` polymorphic in the type of list elements.

```juvix
infixr 5 ::;
type List (A : Type) :=
    nil : List A
  | :: : A -> List A -> List A;

elem : {A : Type} -> (A -> A -> Bool) -> A -> List A -> Bool;
elem _ _ nil := false;
elem eq s (x :: xs) := eq s x || elem eq s xs;
```

For more examples of inductive types and how to use them, see [the Juvix
standard library](https://anoma.github.io/juvix-stdlib/).
