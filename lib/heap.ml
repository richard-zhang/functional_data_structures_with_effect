let smoke_test_heap name (module Heap : Interface.Heap) =
  Printf.printf "test %s\n" name;
  let value = [ 6; 4; 5; 3; 9; 2; 0 ] in
  let heap = List.fold_left (Fun.flip Heap.insert) Heap.empty value in
  let action _ heap =
    Printf.printf "%d\n" (Heap.find_min heap);
    Heap.delete_min heap
  in
  let heap = List.fold_right action value heap in
  assert (Heap.is_empty heap)

module LeftistHeap : Interface.Heap = struct
  (*
    Intuition of Leftist Heap 
    - Right spine are in sorted order
    - Merging two Heap is like merging their right spine
    - and maintain the leftist property
  *)
  type 'a t = E | T of int * 'a * 'a t * 'a t

  let empty = E
  let is_empty = function E -> true | _ -> false
  let rank = function E -> 0 | T (r, _, _, _) -> r

  let makeH value heap_a heap_b =
    if rank heap_a >= rank heap_b then T (rank heap_b + 1, value, heap_a, heap_b)
    else T (rank heap_a + 1, value, heap_b, heap_a)

  let rec merge left right =
    match (left, right) with
    | E, h -> h
    | h, E -> h
    | ( (T (_, a_value, a_left, a_right) as h_a),
        (T (_, b_value, b_left, b_right) as h_b) ) ->
        if a_value <= b_value then makeH a_value a_left (merge a_right h_b)
        else makeH b_value b_left (merge b_right h_a)

  let find_min = function E -> failwith "Empty" | T (_, value, _, _) -> value

  let delete_min = function
    | E -> failwith "Empty"
    | T (_, _, left, right) -> merge left right

  (* let rec insert inserted_value = function
     | E -> T (1, inserted_value, E, E)
     | T (_, value, _, _) as t when inserted_value < value ->
         T (1, inserted_value, t, E)
     | T (_, value, left, right) ->
         makeH value left (insert inserted_value right) *)
  let insert value heap = merge (T (1, value, E, E)) heap
end

module WeightLeftistHeap = struct
  (*
    Invariance:
      the size of left child is at least as much the size or right child
  *)
  type 'a t = E | T of int * 'a * 'a t * 'a t

  let size = function E -> 0 | T (s, _, _, _) -> s
  let empty = E
  let is_empty = function E -> true | _ -> false

  let rec merge t1 t2 =
    match (t1, t2) with
    | E, t -> t
    | t, E -> t
    | ( (T (a_size, a, a_left, a_right) as h_a),
        (T (b_size, b, b_left, b_right) as h_b) ) ->
        (* size(b_left) >= size(b_right) *)
        (* size(a_left) >= size(b_right) *)
        (*
          trying to balance as much as possible
        *)
        let new_size = a_size + b_size in
        if a <= b then
          if size a_left >= size a_right + b_size then
            T (new_size, a, a_left, merge h_b a_right)
          else T (new_size, a, merge h_b a_left, a_right)
        else if size b_left >= size b_right + a_size then
          T (new_size, b, b_left, merge h_a b_right)
        else T (new_size, b, merge b_left h_a, b_right)

  let insert a tree = merge (T (1, a, E, E)) tree
  let find_min = function E -> failwith "Empty" | T (_, a, _, _) -> a

  let delete_min = function
    | E -> failwith "Empty"
    | T (_, _, l, r) -> merge l r
end

module ExplicitMin (H : Interface.Heap) : Interface.Heap = struct
  type 'a t = E | NE of 'a * 'a H.t

  let empty = E
  let is_empty = function E -> true | _ -> false
  let find_min = function E -> failwith "Empty find_min" | NE (a, _) -> a
  let wrap heap = if H.is_empty heap then E else NE (H.find_min heap, heap)

  let insert a = function
    | E -> NE (a, H.insert a H.empty)
    | NE (_, heap) -> heap |> H.insert a |> wrap

  let merge a b =
    match (a, b) with
    | E, b -> b
    | a, E -> a
    | NE (_, l), NE (_, r) -> wrap (H.merge l r)

  let delete_min = function
    | E -> failwith "Empty delete_min"
    | NE (_, heap) -> heap |> H.delete_min |> wrap
end

module BinomialHeap : Interface.Heap = struct
  type 'a tree = Node of int * 'a * 'a tree list
  type 'a t = 'a tree list

  let empty = []
  let is_empty = List.is_empty

  let link left right =
    match (left, right) with
    | ( Node (left_rank, left_elem, left_children),
        Node (rigth_rank, right_elem, right_children) )
      when left_rank = rigth_rank ->
        if left_elem < right_elem then
          Node (left_rank + 1, left_elem, right :: left_children)
        else Node (left_rank + 1, right_elem, left :: right_children)
    | _ -> failwith "rank not the same"

  let rank = function Node (r, _, _) -> r
  let root = function Node (_, r, _) -> r
  let children = function Node (_, _, r) -> r

  let rec insTree t = function
    | [] -> [ t ]
    | head :: rest as whole ->
        if rank t < rank head then t :: whole
        else if rank t = rank head then insTree (link t head) rest
        else head :: insTree t rest

  let insert a heap = insTree (Node (0, a, [])) heap

  let rec merge left_trees right_trees =
    match (left_trees, right_trees) with
    | [], right_trees -> right_trees
    | left_trees, [] -> left_trees
    | left_head :: left_rest, right_head :: right_rest ->
        if rank left_head < rank right_head then
          left_head :: merge left_rest right_trees
        else if rank left_head > rank right_head then
          right_head :: merge left_trees right_rest
        else insTree (link left_head right_head) (merge left_rest right_rest)

  let rec remove_min = function
    | [] -> failwith "empty tree"
    | [ t ] -> (t, [])
    | t :: ts ->
        let t', rest = remove_min ts in
        if root t < root t' then (t, ts) else (t', t :: rest)

  let find_min h =
    let t, _ = remove_min h in
    root t

  let delete_min h =
    let t, tree = remove_min h in
    merge (t |> children |> List.rev) tree
end

module BinomialHeap2 = struct
  type 'a tree = Node of 'a * 'a tree list

  (* ascending order *)
  type 'a t = (int * 'a tree) list

  (* link two tree, precondition thost tow tree has the same rank *)
  let link (Node (left_value, left_children) as left)
      (Node (right_value, right_children) as right) =
    if left_value <= right_value then Node (left_value, right :: left_children)
    else Node (right_value, left :: right_children)

  let empty = []
  let is_empty = List.is_empty

  let single_tree a =
    let single_node = Node (a, []) in
    [ (1, single_node) ]

  let rec merge left right =
    match (left, right) with
    | [], _ -> right
    | _, [] -> left
    | left_head :: left_tail, right_head :: right_tail ->
        let left_head_rank = fst left_head in
        let right_head_rank = fst right_head in
        if left_head_rank = right_head_rank then
          let new_tree =
            (left_head_rank + 1, link (snd left_head) (snd right_head))
          in
          merge [ new_tree ] (merge left_tail right_tail)
        else if left_head_rank < right_head_rank then
          left_head :: merge left_tail right
        else right_head :: merge left right_tail

  let insert value tree = merge (single_tree value) tree
  let get_value (_, Node (value, _)) = value

  let rec extract_min_and_rest_tree = function
    | [] -> failwith "empty value"
    | [ x ] -> (x, [])
    | head :: rest ->
        let possible_min, possible_rest = extract_min_and_rest_tree rest in
        if get_value possible_min < get_value head then
          (possible_min, head :: possible_rest)
        else (head, rest)

  let find_min x = x |> extract_min_and_rest_tree |> fst |> get_value

  let delete_min x =
    let (rank, Node (_, children)), rest = extract_min_and_rest_tree x in
    merge rest (children |> List.mapi (fun i a -> (rank - 1 - i, a)) |> List.rev)
end

let%test_unit "test heap" =
  smoke_test_heap "leftist heap" (module LeftistHeap);
  smoke_test_heap "weight balanced heap" (module WeightLeftistHeap);
  smoke_test_heap "ExplicitMin heap" (module ExplicitMin (WeightLeftistHeap));
  smoke_test_heap "Binomial heap" (module BinomialHeap);
  smoke_test_heap "Binomial heap 2" (module BinomialHeap2)
