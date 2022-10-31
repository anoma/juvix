(* Ackermann function (higher-order definition) *)

let rec iter f n x =
  if n = 0 then
    x
  else
    f (iter f (n - 1) x)

let ackermann m =
  iter (fun f n -> iter f (n + 1) 1) m ((+) 1);;

print_int (ackermann 3 11);;
print_newline ();;
