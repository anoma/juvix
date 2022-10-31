(* compute the Nth Fibonacci number modulo 2^28 with CPS *)

let step k n m cont =
  if k == 0 then
    n
  else
    cont (k - 1) m ((n + m) mod 268435456)

let rec go k n m = step k n m go

let fib k = go k 0 1

;;

print_int (fib 100000000);
print_newline ()
