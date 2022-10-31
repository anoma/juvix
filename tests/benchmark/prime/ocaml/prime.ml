(* Compute the Nth prime *)

let prime n =
  let rec check_divisible p lst =
    match lst with
    | [] -> false
    | h :: t -> if p mod h = 0 then true else check_divisible p t
  in
  let rec go n p lst =
    if n = 0 then
      List.hd lst
    else if check_divisible p lst then
      go n (p + 1) lst
    else
      go (n - 1) (p + 1) (p :: lst)
  in
  go n 2 [];;

print_int (prime (1024 * 16));;
print_newline ();;
