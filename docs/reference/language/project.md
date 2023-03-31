# Juvix project

A _juvix project_ is a collection of juvix modules plus some extra metadata. The
most convenient way to create a juvix project is to run the command `juvix init`.

A project is rooted in a directory. The root is set by creating a `juvix.yaml`,
which contains the following fields:

1. **name**: The name of the project. The name must not be empty and cannot
   exceed 100 characters. Lower case letters, digits and hyphen `-` are
   acceptable characters. The first letter must not be a hyphen. Summarizing, it
   must match the following regexp: `[a-z0-9][a-z0-9-]{0,99}`.
2. **version** (_optional_): The version of the project. It must follow the
   [SemVer](https://semver.org/) specification. If ommited the version will be
   assumed to be _0.0.0_.
3. **dependencies** (_optional_): The dependencies of the project given as a
   list. Each dependency is given as relative (or absolute) path to the root of
   another juvix project. If the field is ommited, it will be assumed to contain
   the juvix standard library as a dependency.

As intuition would tell, a juvix module belongs to a juvix project if it is
placed in the subtree hanging from the root directory. This rule has two
exceptions:
1. Modules in a hidden (or hanging from a hidden) directory are not part of the
   project. E.g., if the root of a project is `dir`, then the module
   `dir/.d/Lib.juvix` does not belong to the project rooted in `dir`.
1. A `juvix.yaml` file shadows other `juvix.yaml` files in parent
   directories. E.g. if the root of a project is `dir` and the files
   `dir/juvix.yaml` and `dir/nested/juvix.yaml` exist, then the module
   `dir/nested/Lib.juvix` would belong to the project in `dir/nested`.

## Module naming convention

Modules that belong to a project must follow a naming convention. That is, if
`dir` is the root of a project, then the module in `dir/Path/To/lib.juvix` must
be named `Path.To.lib`.

