open Effect.Deep
open Effect

(* variable to save my ass *)

type 'b susp_t = Pending of (unit -> 'b) | Done of 'b
type 'b susp = 'b susp_t ref

type _ Effect.t +=
  | Suspend : ('a -> 'b) * 'a -> 'b susp t
  | Force : 'b susp -> 'b t

(*
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

take 10 fib 
*)

(*
What's the effect involved?

- A suspended expression is delayed until expression is needed
- the result is cached

- force
- lazy f x

val s = $(primes 100000)
val $x = s
val $y = s

104729
104743
*)

let sieve () =
  let rec sieve_inner numbers =
    match numbers with
    | [] -> []
    | p :: rest ->
        p :: sieve_inner (List.filter (fun x -> x mod p <> 0) rest)
  in
  sieve_inner (List.init 200000 (fun i -> i + 2))

(* Function to get the nth prime number *)
let nth_prime n =
  let primes = sieve () in
  List.nth primes (n - 1)

let force susp = perform (Force susp)
let suspend f a = perform (Suspend (f, a))

let example () =
  print_newline ();
  let s = suspend nth_prime 10003 in
  print_endline "start the computation first time";
  let x = force s in
  Printf.printf "end the computation %d first time\n" x;
  print_endline "start the computation second time";
  let y = force s in
  Printf.printf "end the computation %d second time\n" y;
  assert (x = y)

let run f =
  try_with f ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Suspend (f, a) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let v = Pending (fun () -> f a) in
                  continue k (ref v))
          | Force susp ->
              Some
                (fun (k : (a, _) continuation) ->
                  let v =
                    match !susp with
                    | Pending action ->
                        let v = action () in
                        susp := Done v;
                        v
                    | Done v -> v
                  in
                  continue k v)
          | _ -> None);
    }
let main = run example

let%test_unit "lazy example: memorised prime" = run example