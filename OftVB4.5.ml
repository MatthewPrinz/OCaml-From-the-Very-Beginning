(* For chapter 5, we use the functions length, drop, and take. I'll include
   them here so I don't import the entirety of chapter 4 exercises + other code
   I wrote for it *)

let rec length_inner l n = 
  match l with 
    [] -> n
  | h::t -> length_inner t (n+1)

let length l = 
  length_inner l 0

let rec drop n l = 
  if n=0 then l else
    match l with 
      [] -> []
    | _::t -> drop (n-1) t

let rec take n l =
  if n= 0 then l else
    match l with
      [] -> []
    | h::t -> h :: take (n-1) t