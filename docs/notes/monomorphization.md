---
author: Jan Mas Rovira
---

# Monomorphization

(Removed in v0.2.5)

Monomorphization refers to the process of converting polymorphic code to
monomorphic code (no type variables) through static analysis.

Example:

```juvix
id : (A : Type) → A → A;
id _ a := a;

b : Bool;
b := id Bool true;

n : Nat;
n := id Nat zero;
```

Is translated into:

```juvix
id_Bool : Bool → Bool;
id_Bool a := a;

id_Nat : Nat → Nat;
id_Nat a := a;
```

# More examples

## Mutual recursion

```juvix
type List (A : Type) :=
  nil : List A |
  cons : A → List A → List A;

even : (A : Type) → List A → Bool;
even A nil := true ;
even A (cons _ xs) := not (odd A xs) ;

odd : (A : Type) → List A → Bool;
odd A nil := false ;
odd A (cons _ xs) := not (even A xs) ;

-- main := even Bool ..  odd Nat;
```

# Collection algorithm

This section describes the algorithm to collect the list of all concrete
functions and inductive definitions that need to be generated.

## Assumptions:

1.  Type abstractions only appear at the leftmost part of a type
    signature.
2.  All functions and constructors are fully type-applied: i.e. currying
    for types is not supported.
3.  There is at least one function with a concrete type signature.
4.  All axioms are monomorphic.
5.  No module parameters.

## Definitions

1.  **Application**. An application is an expression of the form
    `t₁ t₂ … tₙ` with n > 0.

2.  **Sub application**. If `t₁ t₂ … tₙ` is an application then for
    every `0<i<n` `t₁ t₂ … tᵢ` is a sub application.

Fix a juvix program `P`. Let `𝒲` be the set of all applications that
appear in `P`.

1.  **Maximal application**. A maximal application is an application
    `A∈𝒲` such that for every `A'∈𝒲` we have that `A` is **not** a sub
    application of `A'`.

2.  **Type application**. If

    1.  `t a₁ a₂ … aₙ` is a maximal application; and
    2.  `t` is either a function or an inductive type; and
    3.  `a₁, …, aₘ` are types; and
    4.  `aₘ₊₁` is not a type or `m = n`.

    Then `t a₁, …, aₘ` is a type application.

3.  **Concrete type**. A type is concrete if it involves no type
    variables.

4.  **Concrete type application**. A type application `t a₁ a₂ … aₙ` if
    `a₁, a₂, …,
aₙ` are concrete types.

## Option 1

Let `S₀` be the set of functions with a concrete type signature. Gather
all type applications (both in the type and in the body) in each of the
functions in `S₀`. Clearly the collected type applications are all
concrete. We now have a stack `c₁, c₂, …, cₙ` of concrete type
applications.

1.  If the stack is empty, we are done.
2.  Otherwise pop `c₁` from the stack. It will be of the form
    `t a₁ … aₘ`, where `t` is either an inductive or a function and
    `a₁, …, aₘ` are concrete types.
3.  If the instantiation `t a₁ … aₘ` has already been registered go to
    step 1. Otherwise continue to the next step.
4.  Register the instantiation `t a₁ … aₘ`.
5.  If `t` is a function, then it has type variables `v₁, …, vₘ`.
    Consider the substitution `σ = {v₁ ↦ a₁, …, vₘ ↦ aₘ}`. Consider the
    list of type applications in the body of `t`: `d₁, …, dᵣ`. Add
    `σ(d₁), …, σ(dᵣ)` to the stack and continue to step 1. It is easy to
    see that for any `i`, `σ(dᵢ)` is a concrete type application.
6.  If `t` is an inductive type, let `d₁, …, dᵣ` be the type
    applications that appear in the type of its constructors, then
    proceed as before.

### Correctness

It should be clear that the algorithm terminates and registers all
instantiations of constructors and functions.

# Generation algorithm

The input of this algorithm is the list of concrete type applications,
name it `ℒ`, produced by the collection algorithm. Example:

```juvix
List String
Pair Int Bool
Maybe String
Maybe Int
if (Maybe String)
if (Maybe Int)
if (Pair Int Bool)
```

## Name generation

Let `f â` be an arbitrary element of `ℒ`, where `â` is a list of
concrete types.

- If `f` is a function, assign a fresh name to `f â`, call it
  `⋆(f â)`.
- If `f` is an inductive type, assign a fresh name to `f â`, call it
  `⋆(f â)`. Then, for each constructor `cᵢ` of `f`, where `i` is the
  index of the constructor, assign a fresh name to it and call it
  `⋆ᵢ(f â)`.

## Function generation

Consider an arbitrary function `f` in the original program. Then
consider the list of concrete type applications involving `f`:
`f â₁, …, f âₙ`.

- If `n = 0`, then either:
  1.  `f` has a concrete type signature, in that case we proceed as
      expected.
  2.  `f` is never called from the functions with a concrete type. In
      this case we can safely ignore it.
- If `n > 1`. For each `âᵢ` we proceed as follows in the next
  sections. Fix `m` to be the lenght of `âᵢ` with `m > 0`.

### Function name

The name of the monomorphized function is `⋆(f âᵢ)`.

### Type signature

Let `𝒮` be the type signature of `f`. Then `𝒮` has to be of the form
`(A₁ :
 Type) → … → (Aₘ : Type) → Π`, where `Π` is a type with no type
abstractions. Now consider the substitution
`σ = {A₁ ↦ âᵢ[1], …, Aₘ ↦ âᵢ[m]}`. Since `âᵢ` is a list of concrete
types, it is clear that `σ(Π)` is a concrete type. Then proceed as
described in <span class="spurious-link" target="Types">_Types_</span>.

### Function clause

Let `𝒞` be a function clause of `f`. Let `p₁ … pₖ` with `k ≥ m` be the
list of patterns in `𝒞`. Clearly the first `m` patterns must be either
variables or wildcards. Wlog assume that the first `m` patterns are all
variables, namely `v₁, …, vₘ`. Let `σ = {v₁ ↦ âᵢ[1], …, Aₘ ↦ âᵢ[m]}` be
a substitution. Let `e` be the body of `𝒞`, then clearly `σ(e)` has no
type variables in it. Now, since each name id must be bound at most
once, we need to generate new ones for the local variables bound in the
patterns `pₘ₊₁, …, pₖ`. Let `w₁, …, wₛ` be the variables bound in
`pₘ₊₁, …, pₖ`. Let `w'₁, …, w'ₛ` be fresh variables. Then let
`δ = {w₁ ↦ w'₁, …, wₛ ↦ w'ₛ}`.

Now let `𝒞'` have patterns `δ(pₘ₊₁), …, δ(pₖ)` and let `e' :` (σ ∪
δ)(e)=. It should be clear that `e'` has no type variables in it and
that all local variable references in `e'` are among `w'₁, …, w'ₛ`. Note
that `e'` is not yet monomorphized. Proceed to the next step to achieve
that.

### Expressions

The input is an expression `e` that has no type variables in it. The
goal is to replace the concrete type applications by the corresponding
monomorphized expression.

The only interesting case is when we find an application. Consider the
unfolded view of the application: `f a₁ … aₘ`. Then, if `f` is either a
constructor, or a function, let `A₁, …, Aₖ` with `k ≤ m` be the list of
type parameters of `f`.

- If `f` is a function and `f a₁ … aₖ ∉ ℒ` then recurse normally,
  otherwise, let `â :` a₁ … aₖ= and replace the original expression
  `f a₁ … aₘ`, by `⋆(f â)
aₖ₊₁' … aₘ'` where `aₖ₊₁' … aₘ'` are the monomorphization of
  `aₖ₊₁ … aₘ` respectively.
- If `f` is a constructor, let `d` be its inductive type. Then check
  `d a₁ … aₖ
∈ ℒ`. Proceed analogously as before.

### Types

The input is a type `t` that has no type variables in it. The goal is to
replace the concrete type applications by the corresponding
monomorphized type. Proceed analogously to the previous section.
