# Axiom

Axioms or postulates can be introduced by using the `axiom` keyword. For
example, let us imagine one wants to write a program that assumes _A_ is
a type, and there exists a term _x_ that inhabits _A_. Then the program
would look like the following.

```juvix
    module Example;
     axiom A : Type;
     axiom x : A;
    end;
```

Terms introduced by the `axiom` keyword lack any computational content.
Programs containing axioms not marked as builtins cannot be compiled to
most targets.
