# Compiling simple programs

A Juvix file must declare a module whose name corresponds exactly to the
name of the file. For example, a file `Hello.juvix` must declare a
module `Hello`:

```juvix
-- Hello world example. This is a comment.
module Hello;

-- Import the standard library prelude, including the function 'printStringLn'
open import Stdlib.Prelude;

main : IO;
main := printStringLn "Hello world!";

end;
```

A file compiled to an executable must define the zero-argument function
`main` of type `IO` which is evaluated when running the program.

To compile the file `Hello.juvix` type `juvix compile Hello.juvix`.
Typing `juvix compile --help` will list all options to the `compile`
command.

# Compilation targets

Since version 0.3 Juvix supports three compilation targets. The targets
are specified with the `-t` option:
`juvix compile -t target file.juvix`.

1.  `native`. This is the default. Produces a native 64bit executable
    for your machine.
2.  `wasm32-wasi`. Produces a WebAssembly binary which uses the WASI
    runtime.
3.  `geb`. Produces a [GEB](https://anoma.github.io/geb/) input file.

# Juvix projects

A <u>Juvix project</u> is a collection of Juvix modules inside one main
project directory containing a `juvix.yaml` metadata file. The name of
each module must coincide with the path of the file it is defined in,
relative to the project's root directory. For example, if the file is
`root/Data/List.juvix` then the module must be called `Data.List`,
assuming `root` is the project's directory.

To check that Juvix is correctly detecting your project's root, you can
run the command `juvix dev root File.juvix`.

See also: [Modules Reference](../reference/language/modules.md).
