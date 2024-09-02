open Lazy

type 'a steam_cell =
  | Nil
  | Cons of ('a * 'a steam)
  and
  'a steam = 'a steam_cell susp
let mk_steam_cell a b = Cons (a, b)
let cons (type a) (a : a) (steam : a steam) : a steam =  suspend_2 mk_steam_cell a steam

let rec (++) (a_steam :'a steam) (b_steam : 'a steam) = let* steam_cell = a_steam in
  match  steam_cell with
  | Nil -> b_steam
  | Cons (a, rest) -> cons a (rest ++ b_steam)

let rec take n (steam : 'a steam) : 'a steam =
  let* steam_cell = steam in
  match (n, steam_cell) with
  | (0, _) | (_, Nil) -> return Nil
  | (n, Cons (head, rest)) -> cons head (take (n - 1) rest)

let rec drop n (steam : 'a steam) : 'a steam =
  let* steam_cell = steam in
  match (n, steam_cell) with
  | (0, _) -> steam
  | (_, Nil) -> return Nil
  | (n, Cons (_, rest)) -> drop (n - 1) rest

let reverse steam =
  let rec reverse_helper a_steam b_steam =
      let* steam_cell = a_steam in
      match  steam_cell with
      | Nil -> b_steam
      | Cons (x, xs) -> reverse_helper xs (cons x b_steam)
  in reverse_helper steam (return Nil)