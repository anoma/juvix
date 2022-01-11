module Parsing.Test where

import MiniJuvix.Syntax.Concrete.ParserQQ

m :: Module 'Parsed 'ModuleTop
m =
  [mjuvixMod|
module FirstMilestone;

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

module M;  -- This creates a module called M.
end;       -- This closes the current module in scope.

--------------------------------------------------------------------------------
-- Import definitions from existing modules
--------------------------------------------------------------------------------

import Primitives;

{- The line above will import to the local scope all the
   public names qualified in the module called
   Primitives.
-}

open Primitives;

{- The line above will import to the local scope all the
   public names unqualified in the module called
   Prelude.
-}

import Backend;

-- Additionally, one can only import unqualified names by means of
-- the keyword "using".
open Backend using { LLVM };  -- this imports to the local scope only the
                              -- variable called LLVM.
-- One can use ---in combination with `using`--- the keyword `hiding`
-- to avoid importing undesirable names.

import Prelude;
open Prelude hiding { Nat ; Unit ; Empty } ;

--------------------------------------------------------------------------------
-- Inductive type declarations
--------------------------------------------------------------------------------

-- An inductive type named Empty without data constructors.
inductive Empty {};

-- An inductive type named Unit with only one constructor.
inductive Unit { tt : Unit; };

inductive Nat' : Type
{ zero : Nat' ;
  suc : Nat' -> Nat' ;
};

-- The use of the type `Type` below is optional.
-- The following declaration is equivalent to Nat'.

inductive Nat {
  zero : Nat ;
  suc : Nat -> Nat ;
};

-- A term definition uses the symbol (:=) instead of the traditional
-- symbol (=). The symbol (===) is reserved for def. equality. The
-- symbols (=) and (==) are not reserved.

zero' : Nat;
zero' := zero;


-- * Inductive type declarations with paramenters.

-- The n-point type.
inductive Fin (n : Nat) {
  zero : Fin zero;
  suc  : (n : Nat) -> Fin (suc n);
};

-- The type of sized vectors.
inductive Vec (n : Nat) (A : Type)
{
  zero : Vec Nat.zero A;
  succ : A -> Vec n A -> Vec (Nat.succ n) A;
};

-- * Indexed inductive type declarations.

-- A very interesting data type.
inductive Id (A : Type) (x : A) : A -> Type
{
  refl : Id A x x;
};

--------------------------------------------------------------------------------
-- Unicode, whitespaces, newlines
--------------------------------------------------------------------------------

-- Unicode symbols are permitted.
ℕ : Type;
ℕ := Nat;

-- Whitespaces and newlines are optional. The following term
-- declaration is equivalent to the previous one.
ℕ'
  : Type;
ℕ'
:=
  Nat;

-- Again, whitespaces are optional in declarations. For example,
-- `keyword nameID { content ; x := something; };` is equivalent to
-- `keyword nameID{content;x:=something;};`. However, we must strive
-- for readability and therefore, the former expression is better.

--------------------------------------------------------------------------------
-- Axioms/definitions
--------------------------------------------------------------------------------

axiom A : Type;
axiom a : A;
axiom a' : A;

--------------------------------------------------------------------------------
-- Pattern-matching
--------------------------------------------------------------------------------

f : Nat -> A;
f := \x -> match x  -- \x  or λ x to denote a lambda abstraction.
  {
    zero ↦ a   ; -- case declaration uses the mapsto symbol or the normal arrow.
    suc  -> a' ;
  };

-- We can use qualified names to disambiguate names for
-- pattern-matching. For example, imagine the case where there are
-- distinct matches of the same constructor name for different
-- inductive types (e.g. zero in Nat and Fin), AND the function type
-- signature is missing.

g : Nat -> A;
g Nat.zero    := a;
g (Nat.suc t) := a';

-- For pattern-matching, the symbol `_` is the wildcard pattern as in
-- Haskell or Agda. The following function definition is equivalent to
-- the former.

g' : Nat -> A;
g' zero := a;
g' _    := a';

-- Note that the function `g` will be transformed to a function equal
-- to the function f above in the case-tree compilation phase.

-- The absurd case for patterns.

exfalso : (A : Type) -> Empty -> A;
exfalso A e := match e {};

neg : Type -> Type;
neg := A -> Empty;

-- An equivalent type for sized vectors.

Vec' : Nat -> Type -> Type;
Vec' Nat.zero A    := Unit;
Vec' (Nat.suc n) A := A -> Vec' n A;

--------------------------------------------------------------------------------
-- Fixity notation similarly as in Agda or Haskell.
--------------------------------------------------------------------------------

infixl 10 + ;
+ : Nat → Nat → Nat ;
+ Nat.zero m    := m;
+ (Nat.suc n) m := Nat.suc (n + m) ;

--------------------------------------------------------------------------------
-- Quantities for variables.
--------------------------------------------------------------------------------

-- A quantity for a variable in MiniJuvix can be either 0,1, or Any.
-- If the quantity n is not explicit, then it is Any.

-- The type of functions that uses once its input of type A to produce a number.
axiom funs : (x :1 A) -> Nat;

axiom B : (x :1 A) -> Type;   -- B is a type family.
axiom em : (x :1 A) -> B;

--------------------------------------------------------------------------------
-- Where
--------------------------------------------------------------------------------

a-is-a : Id A a a;
a-is-a := refl;

a-is-a' : Id A a a;
a-is-a' := helper
  where {
    open somemodule;
    helper : Id A a a;
    helper := a-is-a;
    };

--------------------------------------------------------------------------------
-- Let
--------------------------------------------------------------------------------

-- `let` can appear in term and type level definitions.

a-is-a'' : Id A a a;
a-is-a'' := let { helper : Id A a a;
                  helper := a-is-a; }
            in helper;

a-is-a''' : let { typeId : (M : Type) -> (x : M) -> Type;
                  typeId M x := Id M x x;
                } in typeId A a;
a-is-a''' := a-is-a;

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

e : Nat;
e : suc zero + suc zero;

two : Nat;
two := suc (suc zero);

e-is-two : Id Nat e two;
e-is-two := refl;

-- print out the internal representation for e without normalising it.
print e;

-- compute e and print e.
eval e;

--------------------------------------------------------------------------------

end;
  |]
