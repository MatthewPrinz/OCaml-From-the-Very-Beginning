(* Chapter 3 *)

let not boolean = 
  match boolean with
    true -> false
  | false -> true

let rec sumints n = 
  match n with 
    0 -> 0
  |  1 -> 1
  | _ -> n+sumints(n-1)

(* Assuming compute "xn" means to compute their product *)
let rec xn x n =
  match n with 
    _ -> x * n

let foo = 
  match 1 + 1 with 2 ->
  match 2+2 with 3 -> 4 | 4 -> 5

let islower a = 
  match a with 
  'a'..'z' -> true
  |  _ -> false 

let is upper a = 
  match a with 
  'A'..'Z' -> true 
  |  _ -> false