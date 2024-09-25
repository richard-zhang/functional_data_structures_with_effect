module type Ordered = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module type Heap = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val find_min : 'a t -> 'a
  val delete_min : 'a t -> 'a t
end
