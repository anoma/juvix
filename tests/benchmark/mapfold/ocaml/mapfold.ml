(* map and fold a list of N integers K times *)

let rec run n acc lst =
  if n == 0 then
    acc
  else
    run (n - 1) (List.fold_left (+) (-acc) lst) (List.map ((+) 1) lst)

let rec gen k n =
  if k == n then
    [k]
  else
    k :: gen (k + 1) n

;;

let k = 10000
and n = 10000
in

print_int (run k 0 (gen 1 n));
print_newline ()
