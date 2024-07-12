theory Program
imports Main
begin

definition id0 :: "nat ⇒ nat" where
  "id0 = id"

definition id1 :: "nat list ⇒ nat list" where
  "id1 = id"

definition id2 :: "'A ⇒ 'A" where
  "id2 = id"

fun add_one :: "nat list ⇒ nat list" where
  "add_one [] = []" |
  "add_one (x # xs) = ((x + 1) # add_one xs)"

fun sum :: "nat list ⇒ nat" where
  "sum [] = 0" |
  "sum (x # xs) = (x + sum xs)"

fun f :: "nat ⇒ nat ⇒ nat ⇒ nat" where
  "f x y z = ((z + 1) * x + y)"

fun g :: "nat ⇒ nat ⇒ bool" where
  "g x y = (if x = y then False else True)"

fun inc :: "nat ⇒ nat" where
  "inc x = (Suc x)"

fun dec :: "nat ⇒ nat" where
  "dec 0 = 0" |
  "dec (Suc x) = x"

fun dec' :: "nat ⇒ nat" where
  "dec' x =
    (case x of
       0 ⇒ 0 |
       (Suc y) ⇒ y)"

fun optmap :: "('A ⇒ 'A) ⇒ 'A option ⇒ 'A option" where
  "optmap f' None = None" |
  "optmap f' (Some x) = (Some (f' x))"

fun pboth :: "('A ⇒ 'A') ⇒ ('B ⇒ 'B') ⇒ 'A × 'B ⇒ 'A' × 'B'" where
  "pboth f' g' (x, y) = (f' x, g' y)"

fun bool_fun :: "bool ⇒ bool ⇒ bool ⇒ bool" where
  "bool_fun x y z = (x ∧ (y ∨ z))"

fun bool_fun' :: "bool ⇒ bool ⇒ bool ⇒ bool" where
  "bool_fun' x y z = (x ∧ y ∨ z)"

datatype 'A Queue
  = queue "'A list" "'A list"

fun qfst :: "'A Queue ⇒ 'A list" where
  "qfst (queue x ω) = x"

fun qsnd :: "'A Queue ⇒ 'A list" where
  "qsnd (queue ω x) = x"

fun pop_front :: "'A Queue ⇒ 'A Queue" where
  "pop_front q =
    (let
       q' = queue (tl (qfst q)) (qsnd q)
     in case qfst q' of
          [] ⇒ queue (rev (qsnd q')) [] |
          ω ⇒ q')"

fun push_back :: "'A Queue ⇒ 'A ⇒ 'A Queue" where
  "push_back q x =
    (case qfst q of
       [] ⇒ queue [x] (qsnd q) |
       q' ⇒ queue q' (x # qsnd q))"

fun is_empty :: "'A Queue ⇒ bool" where
  "is_empty q =
    (case qfst q of
       [] ⇒
         (case qsnd q of
            [] ⇒ True |
            ω ⇒ False) |
       ω ⇒ False)"

definition empty :: "'A Queue" where
  "empty = queue [] []"

fun funkcja :: "nat ⇒ nat" where
  "funkcja n =
    (let
       nat1 = 1;
       nat2 = 2;
       plusOne = λ x0 . case x0 of
                          n' ⇒ n' + 1
     in plusOne n + nat1 + nat2)"

datatype ('A, 'B) Either'
  = Left' 'A |
    Right' 'B

record R =
  r1 :: nat
  r2 :: nat

fun r1 :: "R ⇒ nat" where
  "r1 (mkR a b) = a"

fun r2 :: "R ⇒ nat" where
  "r2 (mkR a b) = b"

end
