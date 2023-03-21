# Termination

To not bring inconsistencies by function declarations, Juvix requires
that every function passes its termination checker. However, since this
is a strong requirement, often tricky to fulfil, we give the user the
possibility to skip this check in two different ways:

- Using the `terminating` keyword to annotate function type signatures
  as terminating. The syntax is the following.

```juvix
terminating fun : A → B;
```

Note that annotating a function as `terminating` means that _all_ its
function clauses pass the termination checker's criterion. To skip the
termination checker for mutual recursive functions, all the functions
involved must be annotated as `terminating`.

- Using the CLI global flag `--no-termination`.

```juvix
juvix typecheck --no-termination MyProgram.juvix
```

In any case, be aware that our termination checker is limited as it only
accepts a subset of recursion functions. The termination checker
algorithm is a slight modification of the algorithm for checking
termination in the Foetus's language.
