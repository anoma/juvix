(* successively map K functions to a list of N integers *)

let rec mapfun fs xs =
  match fs with
  | [] -> xs
  | f :: fs' -> mapfun fs' (List.map f xs)

let rec genfs n =
  if n = 0 then
    []
  else
    ((-) n) :: genfs (n - 1)

let rec genffs n =
  if n = 0 then
    []
  else
    (fun f x -> f (x + n)) :: genffs (n - 1)

let sum lst =
  let rec go lst acc =
    match lst with
    | [] -> acc
    | h :: t -> go t (acc + h)
  in
  go lst 0

let rec gen k n =
  if k = n then
    [k]
  else
    k :: gen (k + 1) n

;;

let k = 100 in
let n = 10000 in

print_int (sum (mapfun (mapfun (genffs k) (genfs k)) (gen 1 n)));
print_newline ();
