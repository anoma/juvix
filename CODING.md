# Juvix coding style guidelines

## General rules

- always use the formatter
- don't use unicode, except perhaps for judoc
- avoid `Pair`
- prefer enumarations with meaningful names over booleans, e.g., instead of using `Bool` define:

```
type Transferability := Transferable | NonTransferable
```

## Imports

- all imports at the beginning of the file
- explicit names for imported identifiers (`import X open using {iden1; iden2}`)
  - exception: `import Stdlib.Prelude open`, `import Anoma open` are allowed

## Names

- types: upper camel case (`Nat`, `PublicKey`)
- functions, constructors, etc: lower camel case (`tokenLogic`, `transferLogic`)
- instances: lower came case with `I` at the end (`instance eqNatI : Eq Nat := ..`)
- boolean check functions: start with `is` (`isWhatever`)
- record constructors: `mk` + type name (`mkResource`)
- meaningful descriptive long names for arguments of public functions, e.g., `element`, `list`, `initialValue`
  - exception: common abbreviations allowed: `fun`, `acc`
- short names like `x` are okay for local definitions

## Function signatures

- put everything to the left of `:` in type signatures, with _full_ meaningful names for arguments
  - temporary exception: when needed for termination, argument can be moved to the right of `:`
  - even when the argument type is moved to the right, give a descriptive argument name in the type:

```
isMember {A} (testEq : A -> A -> Bool) (element : A) : (list : List A) -> Bool
```

## Type variables

- type variables uppercase (upper camel case, like with types)
- implicit type variables: use short version `{A}` in function signatures on the left
- prefer `{A B}` over `{A} {B}` in function signatures
- meaningful type variable names for more "high-level" functions where this makes sense

Example:

```
split
  {ResourceType}
  {{Convertable ResourceType}}
  {{HasOwner ResourceType}}
  {{HasQuantity ResourceType}}
  {{HasTransferability ResourceType}}
  {{TransactionConvertable ResourceType ResourceType}}
  (self : KeyPair)
  (toSplit : ResourceType)
  (quantitiesAndReceivers : List (Pair Nat PublicKey))
  : Result StandardError Transaction :=
  ...
```

But we can use a short name `A` in:

```
find {A} (predicate : A -> Bool) : (list : List A) -> Maybe A
```

## Pattern matching

- use `case` whenever possible; matching on multiple values handled with pairs
- don't use pattern matching with side conditions when there is an `else` side condition -- convert to an `if` in the body instead
- don't pattern match on `Bool` -- use `if` instead

## Application

- don't use "pipes" (i.e., `|>`, `<|`, `>>`, `<<`, etc.)
  - weak exception: there is a "natural" data processing pipeline in several steps
- use iterators `for` , `map`, etc., instead of the function application syntax with `fold`, etc.
- use named application when reasonable

## Documentation

- the documentation string should be an English sentence
- mention the meaningful argument names in the function's judoc
