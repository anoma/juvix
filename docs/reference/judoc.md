# Judoc syntax reference
Judoc is used to document parts of your code. You can attach *Judoc
blocks* to the following entities:

1. A module.
2. A type definition.
3. A constructor definition.
4. A type signature of a function.
5. An axiom definition.

In order to attach documentation to any of these entities, write *blocks* of
documentation before them:

1. For modules:
   ```
   --- This module is cool
   module Cool;
   ..
   ```
2. For type definitions:
   ```
   --- Unary representation of natural numbers
   type Nat : Type :=
     | --- Nullary constructor representing number 0
       zero : Nat;
     | --- Unary constructor representing the successor of a natural number
       suc : Nat -> Nat;
   ```
3. For type signatures (and likewise for axioms):
   ```
   --- The polymorphic identity function
   id : {A : Type} -> A -> A;
   ```
   
   Next we define the syntax of Judoc *blocks*.

## Block

A *block* can be one of these:
1. A *paragraph*.
2. An *example*.

*Blocks* are separated by a line with only `---`.
For instance, this is a sequence of two *blocks*:
```
--- First block
---
--- Second block
```

Note that the following is a single block since it lacks the `---` separator:
```
--- First block

--- Still first block
```

### Paragraph
A *paragraph* is a non-empty sequence of *lines*.

For instance, the following is a paragraph with two *lines*:
```
--- First line
--- Second line
```

Note that a rendered paragraph will have have no line breaks. If you want to
have line breaks, you will need to split the paragraph. Hence, the paragraph
above will be rendered as
```
First line Second line
```

##### line
A *line* starts with `---` and is followed by a non-empty sequence of
*atoms*.

For instance, the following is a valid *line*:
```
--- A ;Pair Int Bool; contains an ;Int; and a ;Bool;
```

##### Atom
An *atom* is either:
1. A string of text (including spaces but not line breaks).
2. An inline Juvix expression surrounded by `;`.

For instance, the following are valid *atoms*:
1. `I am some text.`
2. `;Pair Int Bool;`

### Example
An example is of the following form
```
--- >>> someExpression ;
```
The `someExpression` can span multiple lines and it must be ended with a `;`.
