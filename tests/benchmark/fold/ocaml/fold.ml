(* fold a list of N integers *)

let plusMod x y = (x + y) mod 268435456

let rec run n acc lst =
  if n == 0 then
    List.fold_left plusMod acc lst
  else
    run (n - 1) (List.fold_left plusMod acc lst) lst

let rec gen n acc =
  if n == 0 then
    acc
  else
    gen (n - 1) (n :: acc)

;;

let k = 1000
and n = 100000
in

print_int (run k 0 (gen n []));
print_newline ();
