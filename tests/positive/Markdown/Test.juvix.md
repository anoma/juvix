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

Other code blocks are not touched, e.g:

```text
This is a text block
```


```haskell
module Test where
```

Blocks indented.

  ```haskell
    module Test where
  ```

Empty blocks:

```
```

We also use other markup for documentation such as:

!!! note

    We use this kind of markup for notes, solutions, and other stuff

    1. More text

        ```text
        f {n : Nat := 0} {m : Nat := n + 1} ....
        ```

    2. Second text


??? info "Solution"

    Initial function arguments that match variables or wildcards in all clauses can
    be moved to the left of the colon in the function definition. For example,

    ```juvix
    module move-to-left;
      import Stdlib.Data.Nat open;

      add (n : Nat) : Nat -> Nat
        | zero := n
        | (suc m) := suc (add n m);
    end;
    ```

    is equivalent to

    ```juvix
      module example-add;
        import Stdlib.Data.Nat open;
        add : Nat -> Nat -> Nat
          | n zero := n
          | n (suc m) := suc (add n m);
      end;
    ```