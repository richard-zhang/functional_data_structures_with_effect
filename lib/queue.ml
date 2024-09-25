type 'a t = ('a list * 'a list)

let empty = ([], [])

let is_empty = function
  | ([], _) -> true
  | _ -> false

let snoc queue x =
  match queue with
  | ([], []) -> ([x], [])
  | (a, b) -> (a, x :: b)

(*
1::2::3,
6::5::4
*)
let head =
  function
  | ([], []) -> failwith "the queue is empty"
  | (a::_, _) -> a
  | _ -> failwith "a"

let tail : 'a t -> 'a t= function
  | ([_], r) -> (List.rev r, [])  
  | (_::c, r) -> (c, r)
  | (_, _) -> failwith "the queue is empty"
