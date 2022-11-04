(* count combinations of numbers 1 to N having sum N *)

let combinations n =
  let rec go n s =
    if s == 0 then
      1
    else if n == 0 then
      0
    else if s < 0 then
      0
    else
      go n (s - n) + go (n - 1) s
  in
  go n n

;;

print_int (combinations 100);
print_newline ();
