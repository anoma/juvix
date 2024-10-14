# Juvix coding style guidelines for the standard library and Anoma apps

## Definitions

- _enumeration type_ is an inductive type whose all constructors have zero
  arguments

## General rules

- always use the formatter
- don't use unicode, except perhaps for judoc
- avoid `Pair`
- prefer enumeration types with meaningful constructor names over booleans,
  e.g., instead of using `Bool` define:

```
type Transferability :=
  | Transferable
  | NonTransferable;
```

## Imports

- all imports at the beginning of the file
- explicit names for imported identifiers (`import X open using {iden1; iden2}`)
  - exception: `import Stdlib.Prelude open` and `import Anoma open` are allowed

## Names

- types: upper camel case (`Nat`, `PublicKey`)
- functions, constructors, etc: lower camel case (`tokenLogic`, `transferLogic`)
  - exception: constructors of enumeration types: upper camel case

```
type BalanceFactor :=
  | --- Left child is higher.
    LeanLeft
  | --- Equal heights of children.
    LeanNone
  | --- Right child is higher.
    LeanRight;

type Ordering :=
  | LessThan
  | Equal
  | GreaterThan;
```

- instances: lower camel case with `I` at the end (`instance eqNatI : Eq Nat := ..`)
- boolean check functions: start with `is` (`isWhatever`)
- record constructors: `mk` + type name (`mkResource`)
- meaningful descriptive long names for arguments of public functions, e.g., `element`, `list`, `initialValue`
  - exception: common abbreviations allowed: `fun`, `acc`, `elem`
  - exception: generic functions whose arguments have no specific meaning, e.g.,

```
id {A} (x : A) : A := x
```

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
- higher-order type variables also upper camel case (`F : Type -> Type`)
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
  - exception: there is a "natural" data processing pipeline in several steps, e.g.,

```
map (+ 1) >> filter (< 0) >> sum
```

- use iterators `for` , `map`, etc., instead of the function application syntax with `fold`, etc.
- use named application when reasonable

## Type definitions

- use ADT syntax
- every constructor on a separate line
- give meaningful names to constructor arguments

Example:

```
type BinaryTree (A : Type) :=
  | leaf
  | node {
      left : BinaryTree A;
      element : A;
      right : BinaryTree A
    };
```

## Documentation

- the documentation string should be an English sentence
- mention the meaningful argument names in the function's judoc
