# Module system

## Defining a module

The `module` keyword starts the declaration of a module followed by its
name and body. The module declaration ends with the `end` keyword.

```juvix
-- ModuleName.juvix
module ModuleName;

end;
```

A <u>Juvix project</u> is a collection of Juvix modules inside one main
project folder containing a metadata file named `juvix.yaml`. Each Juvix
file has to define a <u>module</u> of the same name. The name of the
module must coincide with the path of the its file relative to its
project's root directory. For example, if the file is
`root/Data/List.juvix` then the module must be called `Data.List`,
assuming `root` is the project's folder.

To check that Juvix is correctly detecting your project's root, one can
run the command `juvix dev root File.juvix`.

## Importing modules

To bring into the current scope all module definitions from other
external modules, one can use the `import` keyword along with the
corresponding module name. This way, one gets all the imported names
qualified.

```juvix
-- A.juvix
module A;
   axiom Nat : Type;
   axiom zero : Nat;
end;

-- B.juvix
module B;
    import A;
    x : A.Nat;
    x := A.zero;
```

Additionally, one can <u>open</u> an imported module making available
all its names by their unqualified name.

```juvix
-- A.juvix
module A;
   axiom Nat : Type;
   axiom zero : Nat;
end;

-- B.juvix
module B;
    import A;
    open A;
    x : Nat;
    x := zero;
```

However, opening modules may create name collisions if you already have
the imported names as definitions in the current module. In this case,
Juvix will complain with an error, letting you know which symbols are
ambiguous. For example, in module `B` below, the name `a` is ambiguous.

```juvix
-- A.juvix
module A;
axiom A : Type;
axiom a : A;
end;

-- B.juvix
module B;
import A;
open A;
axiom a : A;

x := a;
end;
```

One alternative here is hiding the name `a` as follows.

```juvix
-- B.juvix
module B;
import A;
open A hiding {a};

axiom a : A;
x := a;

end;
```

Now, we can use the `open import` syntax to simplify the `import-open`
statements.

Instead of having:

```juvix
import Prelude;
open Prelude;
```

We simplify it by the expression:

```juvix
open import Prelude;
```

The `hiding` keyword can be used within an `open-import` statement.

```juvix
-- B.juvix
module A;
open import A hiding {a};
axiom a : A;
x := a;
end;
```

## Exporting symbols

The module `C` below does not typecheck. Both symbols, originally
defined in module `A`, are not visible in module `C` after importing
`B`. The symbols `A` and `a` are not exported by the module `B`. To
export symbols from an imported module, one can use the `public` keyword
at the end of the corresponding `open` statement. For example, the
module `C` typechecks after marking the import of `A` as `public` in
module `B`.

```juvix
-- A.juvix
module A;
axiom A : Type;
axiom a : A;
end;

-- B.juvix
module B;
open import A;
end;

-- C.juvix
module C;
open import B;

x : A;
x := a;
end;
```

Fix:

```juvix
-- B.juvix
module B;
open import A public;
end;
```
