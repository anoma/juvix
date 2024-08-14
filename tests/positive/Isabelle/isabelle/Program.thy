theory Program
imports Main
begin

definition id0 :: "nat \<Rightarrow> nat" where
  "id0 = id"

definition id1 :: "nat list \<Rightarrow> nat list" where
  "id1 = id"

definition id2 :: "'A \<Rightarrow> 'A" where
  "id2 = id"

fun add_one :: "nat list \<Rightarrow> nat list" where
  "add_one [] = []" |
  "add_one (x # xs) = ((x + 1) # add_one xs)"

fun sum :: "nat list \<Rightarrow> nat" where
  "sum [] = 0" |
  "sum (x # xs) = (x + sum xs)"

fun f :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "f x y z = ((z + 1) * x + y)"

fun g :: "nat \<Rightarrow> nat \<Rightarrow> bool" where
  "g x y = (if x = y then False else True)"

fun inc :: "nat \<Rightarrow> nat" where
  "inc x = (Suc x)"

fun dec :: "nat \<Rightarrow> nat" where
  "dec 0 = 0" |
  "dec (Suc x) = x"

fun dec' :: "nat \<Rightarrow> nat" where
  "dec' x =
    (case x of
       0 \<Rightarrow> 0 |
       (Suc y) \<Rightarrow> y)"

fun optmap :: "('A \<Rightarrow> 'A) \<Rightarrow> 'A option \<Rightarrow> 'A option" where
  "optmap f' None = None" |
  "optmap f' (Some x) = (Some (f' x))"

fun pboth :: "('A \<Rightarrow> 'A') \<Rightarrow> ('B \<Rightarrow> 'B') \<Rightarrow> 'A \<times> 'B \<Rightarrow> 'A' \<times> 'B'" where
  "pboth f' g' (x, y) = (f' x, g' y)"

fun bool_fun :: "bool \<Rightarrow> bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bool_fun x y z = (x \<and> (y \<or> z))"

fun bool_fun' :: "bool \<Rightarrow> bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bool_fun' x y z = (x \<and> y \<or> z)"

datatype 'A Queue
  = queue "'A list" "'A list"

fun qfst :: "'A Queue \<Rightarrow> 'A list" where
  "qfst (queue x v) = x"

fun qsnd :: "'A Queue \<Rightarrow> 'A list" where
  "qsnd (queue v v') = v'"

fun pop_front :: "'A Queue \<Rightarrow> 'A Queue" where
  "pop_front q =
    (let
       q' = queue (tl (qfst q)) (qsnd q)
     in case qfst q' of
          [] \<Rightarrow> queue (rev (qsnd q')) [] |
          v \<Rightarrow> q')"

fun push_back :: "'A Queue \<Rightarrow> 'A \<Rightarrow> 'A Queue" where
  "push_back q x =
    (case qfst q of
       [] \<Rightarrow> queue [x] (qsnd q) |
       q' \<Rightarrow> queue q' (x # qsnd q))"

fun is_empty :: "'A Queue \<Rightarrow> bool" where
  "is_empty q =
    (case qfst q of
       [] \<Rightarrow>
         (case qsnd q of
            [] \<Rightarrow> True |
            v \<Rightarrow> False) |
       v \<Rightarrow> False)"

definition empty :: "'A Queue" where
  "empty = queue [] []"

fun funkcja :: "nat \<Rightarrow> nat" where
  "funkcja n =
    (let
       nat1 = 1;
       nat2 = 2;
       plusOne = \<lambda> x0 . case x0 of
                                  n' \<Rightarrow> n' + 1
     in plusOne n + nat1 + nat2)"

datatype ('A, 'B) Either'
  = Left' 'A |
    Right' 'B

fun bf :: "bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bf b1 b2 = (\<not> (b1 \<and> b2))"

fun nf :: "int \<Rightarrow> int \<Rightarrow> bool" where
  "nf n1 n2 = (n1 - n2 \<ge> n1 \<or> n2 \<le> n1 + n2)"

end
