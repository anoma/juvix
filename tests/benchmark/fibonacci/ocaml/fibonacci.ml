(* compute the Nth Fibonacci number modulo 2^28 *)

let fib k =
  let rec go k n m =
    if k == 0 then
      n
    else
      go (k - 1) m ((n + m) mod 268435456)
  in
  go k 0 1

;;

print_int (fib 100000000);
print_newline ()
