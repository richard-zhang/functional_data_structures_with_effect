type 'a steam_cell =
  | Nil
  | Cons of ('a * 'a steam)
  and
  'a steam = 'a steam_cell Lazy.susp


val (++) : 'a steam -> 'a steam -> 'a steam

val take : int -> 'a steam -> 'a steam

val drop : int -> 'a steam -> 'a steam

val reverse : 'a steam -> 'a steam