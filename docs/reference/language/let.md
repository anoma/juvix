# Local definitions

Local definitions are introduced with the `let` construct.

```juvix
sum : NList -> Nat;
sum lst :=
  let
    go : Nat -> NList -> Nat;
    go acc nnil := acc;
    go acc (ncons x xs) := go (acc + x) xs;
  in
  go 0 lst;
```

The declaractions in a `let` have the same syntax as declarations inside
a module, but they are visible only in the expression following the `in`
keyword.
