# Juvix tutorial

NOTE: This is a tutorial for Juvix version 0.3. Earlier versions do not
support all the syntax described here.

- [Juvix REPL](./learn.md#juvix-repl)
- [Basic expressions](./learn.md#basic-expressions)
- [Files, modules and
  compilation](./learn.md#files-modules-and-compilation)
- [Output](./learn.md#output)
- [Data types and functions](./learn.md#data-types-and-functions)
- [Pattern matching](./learn.md#pattern-matching)
- [Comparisons and
  conditionals](./learn.md#comparisons-and-conditionals)
- [Local definitions](./learn.md#local-definitions)
- [Recursion](./learn.md#recursion)
- [Partial application and higher-order
  functions](./learn.md#partial-application-and-higher-order-functions)
- [Polymorphism](./learn.md#polymorphism)
- [Tail recursion](./learn.md#tail-recursion)
- [Totality checking](./learn.md#totality-checking)
- [Exercises](./learn.md#exercises)

## Juvix REPL

After [installing Juvix](../howto/installing.md), launch the Juvix REPL:

```shell
juvix repl
```

The response should be similar to:

```juvix
Juvix REPL version 0.3: https://juvix.org. Run :help for help
OK loaded: ./.juvix-build/stdlib/Stdlib/Prelude.juvix
Stdlib.Prelude>
```

Currently, the REPL supports evaluating expressions but it does not yet
support adding new definitions. To see the list of available REPL
commands type `:help`.

## Basic expressions

You can try evaluating simple arithmetic expressions in the REPL:

```juvix
Stdlib.Prelude> 3 + 4
7
Stdlib.Prelude> 1 + 3 * 7
22
Stdlib.Prelude> div 35 4
8
Stdlib.Prelude> mod 35 4
3
Stdlib.Prelude> sub 35 4
31
Stdlib.Prelude> sub 4 35
0
```

By default, Juvix operates on non-negative natural numbers. Natural number
subtraction is implemented by the function `sub`. Subtracting a bigger
natural number from a smaller one yields `0`.

You can also try boolean expressions

```juvix
Stdlib.Prelude> true
true
Stdlib.Prelude> not true
false
Stdlib.Prelude> true && false
false
Stdlib.Prelude> true || false
true
Stdlib.Prelude> if true 1 0
1
```

and strings, pairs and lists:

```juvix
Stdlib.Prelude> "Hello world!"
"Hello world!"
Stdlib.Prelude> (1, 2)
(1, 2)
Stdlib.Prelude> 1 :: 2 :: nil
1 :: 2 :: nil
```

In fact, you can use all functions and types from the
[Stdlib.Prelude](https://anoma.github.io/juvix-stdlib/Stdlib.Prelude.html)
module of the [standard library](https://anoma.github.io/juvix-stdlib),
which is preloaded by default.

```juvix
Stdlib.Prelude> length (1 :: 2 :: nil)
3
Stdlib.Prelude> null (1 :: 2 :: nil)
false
Stdlib.Prelude> swap (1, 2)
(2, 1)
```

## Files, modules and compilation

Currently, the REPL does not support adding new definitions. To define
new functions or data types, you need to put them in a separate file and
either load the file in the REPL with `:load file.juvix` or compile the
file to a binary executable with the shell command
`juvix compile file.juvix`.

To conveniently edit Juvix files, an [Emacs mode](./emacs.md) and a
[VSCode extension](./vscode.md) are available.

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

## Output

In addition to `printStringLn`, the standard library includes the
functions `printString`, `printNat`, `printNatLn`, `printBool`,
`printBoolLn`. The `IO` computations can be sequenced with `>>`, e.g.,

```juvix
printNat 3 >> printString " + " >> printNatLn 4
```

has type `IO` and when executed prints `3 + 4` followed by a newline.

The type `IO` is the type of IO actions, i.e., of data structures
representing IO computations. The functions `printString`, `printNat`,
etc., do not immediately print their arguments, but rather create a data
structure representing an appropriate IO action. The IO actions created
by the `main` function are executed only after the program has been
evaluated.

## Data types and functions

To see the type of an expression, use the `:type` REPL command:

```juvix
Stdlib.Prelude> :type 1
Nat
Stdlib.Prelude> :type true
Bool
```

The types `Nat` and `Bool` are defined in the standard library.

The type `Bool` has two constructors `true` and `false`.

```juvix
type Bool :=
| true : Bool
| false : Bool;
```

The constructors of a data type can be used to build elements of the
type. They can also appear as patterns in function definitions. For
example, the `not` function is defined in the standard library by:

```juvix
not : Bool -> Bool;
not true := false;
not false := true;
```

The first line is the _signature_ which specifies the type of the
definition. In this case, `not` is a function from `Bool` to `Bool`. The
signature is followed by two _function clauses_ which specify the
function result depending on the shape of the arguments. When a function
call is evaluated, the first clause that matches the arguments is used.

In contrast to languages like Python, Java or C/C++, Juvix doesn't
require parentheses for function calls. All the arguments are just
listed after the function. The general pattern for function application
is: `func arg1 arg2 arg3 ...`

A more complex example of a data type is the `Nat` type from the
standard library:

```juvix
type Nat :=
| zero : Nat
| suc : Nat -> Nat;
```

The constructor `zero` represents `0` and `suc` represents the successor
function – `suc n` is the successor of `n`, i.e., `n+1`. For example,
`suc zero` represents `1`. The number literals `0`, `1`, `2`, etc., are
just shorthands for appropriate expressions built using `suc` and
`zero`.

The constructors of a data type specify how the elements of the type can
be constructed. For instance, the above definition specifies that an
element of `Nat` is either:

- `zero`, or
- `suc n` where `n` is an element of `Nat`, i.e., it is constructed by
  applying `suc` to appropriate arguments (in this case the argument
  of `suc` has type `Nat`).

Any element of `Nat` can be built with the constructors in this way –
there are no other elements. Mathematically, this is an inductive
definition, which is why the data type is called _inductive_.

If implemented directly, the above unary representation of natural
numbers would be extremely inefficient. The Juvix compiler uses a binary
number representation under the hood and implements arithmetic
operations using corresponding machine instructions, so the performance
of natural number arithmetic is similar to other programming languages.
The `Nat` type is a high-level presentation of natural numbers as seen
by the user who does not need to worry about low-level arithmetic
implementation details.

One can use `zero` and `suc` in pattern matching, like any other
constructors:

```juvix
infixl 6 +;
+ : Nat -> Nat -> Nat;
+ zero b := b;
+ (suc a) b := suc (a + b);
```

The `infixl 6 +` declares `+` to be an infix left-associative operator
with priority 6. The `+` is an ordinary function, except that function
application for `+` is written in infix notation. The definitions of the
clauses of `+` still need the prefix notation on the left-hand sides.

The `a` and `b` in the patterns on the left-hand sides of the clauses
are _variables_ which match arbitrary values of the corresponding type.
They can be used on the right-hand side to refer to the values matched.
For example, when evaluating

```juvix
(suc (suc zero)) + zero
```

the second clause of `+` matches, assigning `suc zero` to `a` and `zero`
to `b`. Then the right-hand side of the clause is evaluated with `a` and
`b` substituted by these values:

```juvix
suc (suc zero + zero)
```

Again, the second clause matches, now with both `a` and `b` being
`zero`. After replacing with the right-hand side, we obtain:

```juvix
suc (suc (zero + zero))
```

Now the first clause matches and finally we obtain the result

```juvix
suc (suc zero)
```

which is just `2`.

The function `+` is defined like above in the standard library, but the
Juvix compiler treats it specially and generates efficient code using
appropriate CPU instructions.

## Pattern matching

The patterns in function clauses do not have to match on a single
constructor – they may be arbitrarily deep. For example, here is an
(inefficient) implementation of a function which checks whether a
natural number is even:

```juvix
even : Nat -> Bool;
even zero := true;
even (suc zero) := false;
even (suc (suc n)) := even n;
```

This definition states that a natural number `n` is even if either `n`
is `zero` or, recursively, `n-2` is even.

If a subpattern is to be ignored, then one can use a wildcard `_`
instead of naming the subpattern.

```juvix
isPositive : Nat -> Bool;
isPositive zero := false;
isPositive (suc _) := true;
```

The above function could also be written as:

```juvix
isPositive : Nat -> Bool;
isPositive zero := false;
isPositive _ := true;
```

It is not necessary to define a separate function to perform pattern
matching. One can use the `case` syntax to pattern match an expression
directly.

```juvix
Stdlib.Prelude> case (1, 2) | (suc _, zero) := 0 | (suc _, suc x) := x | _ := 19
1
```

## Comparisons and conditionals

To use the comparison operators on natural numbers, one needs to import
the `Stdlib.Data.Nat.Ord` module. The comparison operators are not in
`Stdlib.Prelude` to avoid clashes with user-defined operators for other
data types. The functions available in `Stdlib.Data.Nat.Org` include:
`<`, `<=`, `>`, `>=`, `==`, `/=`, `min`, `max`.

For example, one may define the function `max3` by:

```juvix
open import Stdlib.Data.Nat.Ord;

max3 : Nat -> Nat -> Nat -> Nat;
max3 x y z := if (x > y) (max x z) (max y z);
```

The conditional `if` is a special function which is evaluated lazily,
i.e., first the condition (the first argument) is evaluated, and then
depending on its truth-value one of the branches (the second or the
third argument) is evaluated and returned.

By default, evaluation in Juvix is _eager_ (or _strict_), meaning that
the arguments to a function are fully evaluated before applying the
function. Only `if`, `||` and `&&` are treated specially and evaluated
lazily. These special functions cannot be partially applied (see
[Partial application and higher-order
functions](./learn.md#partial-application-and-higher-order-functions)
below).

## Local definitions

Juvix supports local definitions with let-expressions.

```juvix
f : Nat -> Nat;
f a := let x : Nat := a + 5;
           y : Nat := a * 7 + x
       in
       x * y;
```

The variables `x` and `y` are not visible outside `f`.

One can also use multi-clause definitions in `let`-expressions, with the
same syntax as definitions inside a module. For example:

```juvix
even' : Nat -> Bool;
even' :=
  let
    even : Nat -> Bool;
    odd : Nat -> Bool;

    even zero := true;
    even (suc n) := odd n;

    odd zero := false;
    odd (suc n) := even n;
  in
  even
```

The functions `even` and `odd` are not visible outside `even'`.

## Recursion

Juvix is a purely functional language, which means that functions have
no side effects and all variables are immutable. An advantage of
functional programming is that all expressions are _referentially
transparent_ – any expression can be replaced by its value without
changing the meaning of the program. This makes it easier to reason
about programs, in particular to prove their correctness. No errors
involving implicit state are possible, because the state is always
explicit.

In a functional language, there are no imperative loops. Repetition is
expressed using recursion. In many cases, the recursive definition of a
function follows the inductive definition of a data structure the
function analyses. For example, consider the following inductive type of
lists of natural numbers:

```juvix
type NList :=
| nnil : NList
| ncons : Nat -> NList -> NList;
```

An element of `NList` is either `nnil` (empty) or `ncons x xs` where
`x : Nat` and `xs : NList` (a list with head `x` and tail `xs`).

A function computing the length of a list may be defined by:

```juvix
nlength : NList -> Nat;
nlength nnil := 0;
nlength (ncons _ xs) := nlength xs + 1;
```

The definition follows the inductive definition of `NList`. There are
two function clauses for the two constructors. The case for `nnil` is
easy – the constructor has no arguments and the length of the empty list
is `0`. For a constructor with some arguments, one typically needs to
express the result of the function in terms of the constructor
arguments, usually calling the function recursively on the constructor's
inductive arguments (for `ncons` this is the second argument). In the
case of `ncons _ xs`, we recursively call `nlength` on `xs` and add `1`
to the result.

Let's consider another example – a function which returns the maximum of
the numbers in a list or 0 for the empty list.

```juvix
open import Stdlib.Data.Nat.Ord; -- for `max`

nmaximum : NList -> Nat;
nmaximum nnil := 0;
nmaximum (ncons x xs) := max x (nmaximum xs);
```

Again, there is a clause for each constructor. In the case for `ncons`,
we recursively call the function on the list tail and take the maximum
of the result and the list head.

For an example of a constructor with more than one inductive argument,
consider binary trees with natural numbers in nodes.

```juvix
type Tree :=
| leaf : Nat -> Tree
| node : Nat -> Tree -> Tree -> Tree;
```

The constructor `node` has two inductive arguments (the second and the
third) which represent the left and the right subtree.

A function which produces the mirror image of a tree may be defined by:

```juvix
mirror : Tree -> Tree;
mirror (leaf x) := leaf x;
mirror (node x l r) := node x (mirror r) (mirror l);
```

The definition of `mirror` follows the definition of `Tree`. There are
two recursive calls for the two inductive constructors of `node` (the
subtrees).

## Partial application and higher-order functions

Strictly speaking, all Juvix functions have only one argument.
Multi-argument functions are really functions which return a function
which takes the next argument and returns a function taking another
argument, and so on for all arguments. The function type former `->`
(the arrow) is right-associative. Hence, the type, e.g.,
`Nat -> Nat -> Nat` when fully parenthesised becomes
`Nat -> (Nat -> Nat)`. It is the type of functions which given an
argument of type `Nat` return a function of type `Nat -> Nat` which
itself takes an argument of type `Nat` and produces a result of type
`Nat`. Function application is left-associative. For example, `f a b`
when fully parenthesised becomes `(f a) b`. So it is an application to
`b` of the function obtained by applying `f` to `a`.

Since a multi-argument function is just a one-argument function
returning a function, it can be _partially applied_ to a smaller number
of arguments than specified in its definition. The result is an
appropriate function. For example, `sub 10` is a function which
subtracts its argument from `10`, and `(+) 1` is a function which adds
`1` to its argument. If the function has been declared as an infix
operator (like `+`), then for partial application one needs to enclose
it in parentheses.

A function which takes a function as an argument is a _higher-order
function_. An example is the `nmap` function which applies a given
function to each element in a list of natural numbers.

```juvix
nmap : (Nat -> Nat) -> NList -> NList;
nmap _ nnil := nnil;
nmap f (ncons x xs) := ncons (f x) (nmap f xs);
```

The application

```juvix
nmap \{ x := div x 2 } lst
```

divides every element of `lst` by `2`, rounding down the result. The
expression

```juvix
\{ x := div x 1 }
```

is an unnamed function, or a _lambda_, which divides its argument by
`2`.

## Polymorphism

The type `NList` we have been working with above requires the list
elements to be natural numbers. It is possible to define lists
_polymorphically_, parameterising them by the element type. This is
analogous to generics in languages like Java, C++ or Rust. Here is the
polymorphic definition of lists from the standard library:

```juvix
infixr 5 ::;
type List (A : Type) :=
| nil : List A
| :: : A -> List A -> List A;
```

The constructor `::` is declared as a right-associative infix operator
with priority 5. The definition has a parameter `A` which is the element
type.

Now one can define the `map` function polymorphically:

```juvix
map : {A B : Type} -> (A -> B) -> List A -> List B;
map f nil := nil;
map f (h :: hs) := f h :: map f hs;
```

This function has two _implicit type arguments_ `A` and `B`. These
arguments are normally omitted in function application – they are
inferred automatically during type checking. The curly braces indicate
that the argument is implicit and should be inferred.

In fact, the constructors `nil` and `::` also have an implicit argument:
the type of list elements. All type parameters of a data type definition
become implicit arguments of the constructors.

Usually, the implicit arguments in a function application can be
inferred. However, sometimes this is not possible and then the implicit
arguments need to be provided explicitly by enclosing them in braces:

```juvix
f {implArg1} .. {implArgK} arg1 .. argN
```

For example, `nil {Nat}` has type `List Nat` while `nil` by itself has
type `{A : Type} -> List A`.

## Tail recursion

Any recursive call whose result is further processed by the calling
function needs to create a new stack frame to save the calling function
environment. This means that each such call will use a constant amount
of memory. For example, a function `sum` implemented as follows will use
an additional amount of memory proportional to the length of the
processed list:

```juvix
sum : NList -> Nat;
sum nnil := 0;
sum (ncons x xs) := x + sum xs;
```

This is not acceptable if you care about performance. In an imperative
language, one would use a simple loop going over the list without any
memory allocation. In pseudocode:

```juvix
var sum : Nat := 0;
while (lst /= nnil) {
  sum := sum + head lst;
  lst := tail lst;
};
return sum;
```

Fortunately, it is possible to rewrite this function to use _tail
recursion_. A recursive call is _tail recursive_ if its result is also
the result of the calling function, i.e., the calling function returns
immediately after it without further processing. The Juvix compiler
_guarantees_ that all tail calls will be eliminated, i.e., that they
will be compiled to simple jumps without extra memory allocation. In a
tail recursive call, instead of creating a new stack frame, the old one
is reused.

The following implementation of `sum` uses tail recursion.

```juvix
sum : NList -> Nat;
sum lst :=
  let
    go : Nat -> NList -> Nat;
    go acc nnil := acc;
    go acc (ncons x xs) := go (acc + x) xs;
  in
  go 0 lst;
```

The first argument of `go` is an _accumulator_ which holds the sum
computed so far. It is analogous to the `sum` variable in the imperative
loop above. The initial value of the accumulator is 0. The function `go`
uses only constant additional memory overall. The code generated for it
by the Juvix compiler is equivalent to an imperative loop.

Most imperative loops may be translated into tail recursive functional
programs by converting the locally modified variables into accumulators
and the loop condition into pattern matching. For example, here is an
imperative pseudocode for computing the nth Fibonacci number in linear
time. The variables `cur` and `next` hold the last two computed
Fibonacci numbers.

```juvix
var cur : Nat := 0;
var next : Nat := 1;
while (n /= 0) {
  var tmp := next;
  next := cur + next;
  cur := tmp;
  n := n - 1;
};
return cur;
```

An equivalent functional program is:

```juvix
fib : Nat -> Nat;
fib :=
  let go : Nat -> Nat -> Nat -> Nat;
      go cur _ zero := cur;
      go cur next (suc n) := go next (cur + next) n;
  in
  go 0 1;
```

A naive definition of the Fibonacci function runs in exponential time:

```juvix
fib : Nat -> Nat;
fib zero := 0;
fib (suc zero) := 1;
fib (suc (suc n)) := fib n + fib (suc n);
```

Tail recursion is less useful when the function needs to allocate memory
anyway. For example, one could make the `map` function from the previous
section tail recursive, but the time and memory use would still be
proportional to the length of the input because of the need to allocate
the result list.

## Totality checking

By default, the Juvix compiler requires all functions to be total.
Totality consists of:

- [termination](../explanations/totality/termination.md),
- [coverage](../explanations/totality/coverage.md),
- [strict positivity](../explanations/totality/positive.md).

The termination check ensures that all functions are structurally
recursive, i.e., all recursive call are on structurally smaller value –
subpatterns of the matched pattern. For example, the termination checker
rejects the definition

```juvix
fact : Nat -> Nat;
fact x := if (x == 0) 1 (x * fact (sub x 1));
```

because the recursive call is not on a subpattern of a pattern matched
on in the clause. One can reformulate this definition so that it is
accepted by the termination checker:

```juvix
fact : Nat -> Nat;
fact zero := 1;
fact x@(suc n) := x * fact n;
```

Sometimes, such a reformulation is not possible. Then one can use the
`terminating` keyword to forgoe the termination check.

```juvix
terminating
log2 : Nat -> Nat;
log2 n := if (n <= 1) 0 (suc (log2 (div n 2)));
```

Coverage checking ensures that there are no unhandled patterns in
function clauses or `case` expressions. For example, the following
definition is rejected because the case `suc zero` is not handled:

```juvix
even : Nat -> Bool;
even zero := true;
even (suc (suc n)) := even n;
```

NOTE: Coverage checking will be implemented only in Juvix version 0.4.
Earlier versions of Juvix accept non-exhaustive patterns.

## Exercises

You have now learnt the very basics of Juvix. To consolidate your
understanding of Juvix and functional programming, try doing some of the
following exercises. To learn how to write more complex Juvix programs,
see the [advanced
tutorial](https://docs.juvix.org/examples/html/Tutorial/Tutorial.html)
and the [Juvix program examples](../reference/examples.md).

1.  Define a function `prime : Nat -> Nat` which checks if a given
    natural number is prime.

2.  What is wrong with the following definition?

    ```juvix
    half : Nat -> Nat;
    half n := if (n < 2) 0 (half (n - 2) + 1);
    ```

    How can you reformulate this definition so that it is accepted by
    Juvix?

3.  Define a polymorphic function which computes the last element of a
    list. What is the result of your function on the empty list?

4.  A _suffix_ of a list `l` is any list which can be obtained from `l`
    by removing some initial elements. For example, the suffixes of
    `1 :: 2 :: 3 :: nil` are: `1 :: 2 :: 3 :: nil`, `2 :: 3 :: nil`,
    `3 :: nil` and `nil`.

    Define a function which computes the list of all suffixes of a given
    list in the order of decreasing length.

5.  Recall the `Tree` type from above.

    ```juvix
    type Tree :=
    | leaf : Nat -> Tree
    | node : Nat -> Tree -> Tree -> Tree;
    ```

    Analogously to the `map` function for lists, define a function

    ```juvix
    tmap : (Nat -> Nat) -> Tree -> Tree
    ```

    which applies a function to all natural numbers stored in a tree.

6.  Make the `Tree` type polymorphic in the element type and repeat the
    previous exercise.

7.  Write a tail recursive function which reverses a list.

8.  Write a tail recursive function which computes the factorial of a
    natural number.

9.  Define a function `comp : {A : Type} -> List (A -> A) -> A -> A`
    which composes all functions in a list. For example,

    ```juvix
    comp (suc :: (*) 2 :: \{x := sub x 1} :: nil)
    ```

    should be a function which given `x` computes `2(x - 1) + 1`.
