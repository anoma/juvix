# Example

A Juvix Markdown file name ends with `.juvix.md`. This kind of file must contain
a module declaration at the top, as shown below ---in the first code block. 

```juvix
module Test;
```

Certain blocks can be hidden from the output by adding the `hide` attribute, as shown below.

```juvix hide
import Stdlib.Prelude open;
```

```juvix
fib : Nat → Nat → Nat → Nat
  | zero x1 _ := x1
  
  | (suc n) x1 x2 := fib n x2 (x1 + x2);

fibonacci (n : Nat) : Nat := fib n 0 1;
```

Commands like `typecheck` and `compile` can be used with Juvix Markdown files.

```juvix
main : IO := readLn (printNatLn ∘ fibonacci ∘ stringToNat);
```