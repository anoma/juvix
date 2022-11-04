(* optionally sum N integers from a binary tree K times *)

type tree = Node of int * tree * tree | Leaf

let rec gen n = if n == 0 then Leaf else let t = gen (n - 1) in Node (n, t, t)

let rec sum n t =
  match t with
  | Leaf -> Some 0
  | Node (x, l, r) ->
     if x == n then
       None
     else
       match sum n l with
       | None -> None
       | Some s1 ->
          match sum n r with
          | None -> None
          | Some s2 -> Some (s1 + s2 + x)

let rec run n t =
  if n == 0 then
    sum 0 t
  else
    match run (n - 1) t with
    | None -> sum n t
    | Some x ->
       match sum n t with
       | None -> None
       | Some y -> Some (y - x)

;;

match run 101 (gen 20) with
| None -> print_endline "None"
| Some x -> print_int x; print_newline ()
