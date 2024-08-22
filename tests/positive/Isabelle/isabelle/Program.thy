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
  "qfst (queue x v') = x"

fun qsnd :: "'A Queue \<Rightarrow> 'A list" where
  "qsnd (queue v' v'0) = v'0"

fun pop_front :: "'A Queue \<Rightarrow> 'A Queue" where
  "pop_front q =
    (let
       q' = queue (tl (qfst q)) (qsnd q)
     in case qfst q' of
          [] \<Rightarrow> queue (rev (qsnd q')) [] |
          v' \<Rightarrow> q')"

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
            v' \<Rightarrow> False) |
       v' \<Rightarrow> False)"

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

record R =
  r1 :: nat
  r2 :: nat

fun r1 :: "R \<Rightarrow> nat" where
  "r1 (| R.r1 = r1'0, R.r2 = r2'0 |) = r1'0"

fun r2 :: "R \<Rightarrow> nat" where
  "r2 (| R.r1 = r1'0, R.r2 = r2'0 |) = r2'0"

definition r :: R where
  "r = (| R.r1 = 0, R.r2 = 1 |)"

definition v :: nat where
  "v = 0"

fun funR :: "R \<Rightarrow> R" where
  "funR r' =
    (case r' of
       v' \<Rightarrow>
         (\<lambda> x0 . case x0 of
                           v'0 \<Rightarrow> (| R.r1 = R.r1 v'0 + R.r2 v'0, R.r2 = R.r2 v'0 |)) r')"

fun funRR :: "R \<Rightarrow> R" where
  "funRR r'0 =
    (case (r'0) of
       (r') \<Rightarrow>
         (case (r') of
            (v') \<Rightarrow>
              (\<lambda> x0 . case x0 of
                                v'0 \<Rightarrow>
                                  (| R.r1 = R.r1 v'0 + R.r2 v'0, R.r2 = R.r2 v'0 |)) r'))"

fun funR' :: "R \<Rightarrow> R" where
  "funR' (| R.r1 = rr1, R.r2 = rr2 |) =
    (let
       r1'0 = rr1 + rr2;
       r2'0 = rr2
     in (| R.r1 = r1'0, R.r2 = r2'0 |))"

fun funR1 :: "R \<Rightarrow> R" where
  "funR1 (| R.r1 = 0, R.r2 = r2'0 |) =
    (let
       r1'0 = r2'0;
       r2'1 = r2'0
     in (| R.r1 = r1'0, R.r2 = r2'1 |))" |
  "funR1 (| R.r1 = rr1, R.r2 = rr2 |) =
    (let
       r1'0 = rr2;
       r2'0 = rr1
     in (| R.r1 = r1'0, R.r2 = r2'0 |))"

fun funR2 :: "R \<Rightarrow> R" where
  "funR2 r' =
    (case r' of
       v'0 \<Rightarrow>
         (case v'0 of
            v' \<Rightarrow>
              (case (R.r1 v') of
                 (0) \<Rightarrow>
                   let
                     r1'0 = R.r2 v';
                     r2'0 = R.r2 v'
                   in (| R.r1 = r1'0, R.r2 = r2'0 |) |
                 _ \<Rightarrow>
                   (case v'0 of
                      v'1 \<Rightarrow>
                        let
                          r1'0 = R.r2 v'1;
                          r2'0 = R.r1 v'1
                        in (| R.r1 = r1'0, R.r2 = r2'0 |)))))"

fun funR3 :: "(R, R) Either' \<Rightarrow> R" where
  "funR3 er =
    (case er of
       v'0 \<Rightarrow>
         (case v'0 of
            (Left' v') \<Rightarrow>
              (case (R.r1 v') of
                 (0) \<Rightarrow>
                   let
                     r1'0 = R.r2 v';
                     r2'0 = R.r2 v'
                   in (| R.r1 = r1'0, R.r2 = r2'0 |) |
                 _ \<Rightarrow>
                   (case v'0 of
                      (Left' v'1) \<Rightarrow>
                        let
                          r1'0 = R.r2 v'1;
                          r2'0 = R.r1 v'1
                        in (| R.r1 = r1'0, R.r2 = r2'0 |) |
                      (Right' r') \<Rightarrow>
                        (\<lambda> x0 . case x0 of
                                          v'1 \<Rightarrow> (| R.r1 = 2, R.r2 = R.r2 v'1 |)) r')) |
            (Right' r') \<Rightarrow>
              (\<lambda> x0 . case x0 of
                                v'1 \<Rightarrow> (| R.r1 = 2, R.r2 = R.r2 v'1 |)) r'))"

fun bf :: "bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bf b1 b2 = (\<not> (b1 \<and> b2))"

fun nf :: "int \<Rightarrow> int \<Rightarrow> bool" where
  "nf n1 n2 = (n1 - n2 \<ge> n1 \<or> n2 \<le> n1 + n2)"

end
