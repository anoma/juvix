# Module system

Modules are the way in which we split our programs in separate files. Juvix also
supports local modules, which provide a way to better organize different scopes
within a file.

We call top modules those who are defined at the top of a file.
We call local modules those who are defined inside another module.

## Top modules

A module has a name and a body, which comprises a sequence of
[statements](statement.md).

In order to define a module named `Data.List` we will use the following syntax:

```juvix
module Data.List;

<body>
```

### Top module naming convention

Top modules that belong to a [project](project.md) must follow a naming
convention. That is, if `dir` is the root of a project, then the module in
`dir/Data/List.juvix` must be named `Data.List`.

## _Import_ and _open_ statements

In order to access the definitions from another modules we use an
_import_ statement. To import some module named `Data.List` we will write

```juvix
import Data.List;
```

Now, we can access the definitions in the imported module using _qualified
names_. E.g., `Data.List.sort`.

It is possible to import modules and give them a more convinent way thus:

```juvix
import Data.List as List;
```

If we want to access the contents of a module without the need to qualify the
names, we use an _open statement_. The syntax is as follows:

```juvix
open Data.List;
```

Now we can simply write `sort`. It is important to remember that when we
open a module, that module must be in scope.

Since importing and opening a module is done often, there is special syntax for
that. The following statement:

```juvix
open import Data.List;
```

Is equivalent to this:

```juvix
import Data.List;
open Data.List;
```

When opening a module, if we want to open an explicit subset of its definitions,
we will use the `using` keyword thus:

```juvix
open Data.List using {List; sort; reverse}
```

If we want to open all definitions of a module minus a subset, we
will use the `hiding` keyword thus:

```juvix
open Data.List hiding {head; tail}
```

Normally, all opened definitions are available under the current module, but
they are not exported. Meaning that if another module imports the current
module, it will only be able to access the definitions defined there but not
those which have been opened. If we want opened definitions to be exported, we
will use the `public` keyword thus:

```
module Prelude;

open import Data.List public;
```

Now, from another module we can access definitions in `Data.List` through the
`Prelude` module.

```
module MyModule;

open import Prelude;

-- List, sort, reverse, etc. are now in scope
```

## Local modules

Juvix modules have a hierarchical structure. So far we have discussed top level
modules, which have a one-to-one correspondence with files in the filesystem. On
the other hand, local modules are defined within another module. They can be
useful to group definitions within a file.

The syntax for local modules is as follows:

```
module Path.To.TopModule;

module Loc;
  <body>
end;
```

After the definition of a local module, we can access its definitions by using
qualified names. Local modules can be opened by open statements in the same way
as top modules.

Local modules inherit the scope of the parent module. Some shadowing rules
apply, and they probably follow your intuition:

1. Opening or defining a symbol shadows inherited instances of that symbol.
2. Opening a symbol does _not_ shadow a defined instanance of that symbol in the
   current module.
3. Conversely, defining a symbol in the current module does _not_ shadow an
   opened instance of that symbol.

As a consequence of 2 and 3, using a symbol that is both defined and opened
locally will result in an ambiguousity error.
