(* Merge sort a list of N integers *)

let split xs =
  let rec go xs ys zs =
    match xs with
    | [] -> (ys, zs)
    | x :: xs' -> go xs' zs (x :: ys)
  in
  go xs [] []

let merge xs ys =
  let rec go xs ys acc =
    match (xs, ys) with
    | [], _ -> List.rev_append acc ys
    | _, [] -> List.rev_append acc xs
    | (x :: xs'), (y :: ys') ->
       if x <= y then
         go xs' ys (x :: acc)
       else
         go xs ys' (y :: acc)
  in
  go xs ys []

let rec sort xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | _ ->
     let (l1, l2) = split xs in
     merge (sort l1) (sort l2)

let rec sorted xs =
  match xs with
  | [] -> true
  | [_] -> true
  | x :: ((y :: t) as xs) ->
     if x <= y then
       sorted xs
     else
       false

let rec gen n acc = if n = 0 then acc else gen (n - 1) (n :: acc)

;;

print_endline (if sorted (sort (gen 200000 [])) then "true" else "false");;
