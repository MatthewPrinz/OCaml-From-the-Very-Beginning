let rec map f l = 
  match l with 
    [] -> []
  | h::t -> f h :: map f t

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
  if n = 0 then [] else
    match l with
      [] -> []
    | h::t -> h :: take (n-1) t

let rec merge cmp l1 l2 = 
  match l1, l2 with
    l, [] -> l
  | [], l -> l
  | h1::t1, h2::t2 ->
    if cmp h1 h2 then 
      h1 :: merge cmp t1 (h2::t2)
    else
      h2 :: merge cmp (h1::t1) t2


let rec msort cmp list = 
  match list with
    [] -> []
  | [x] -> [x]
  | _ -> 
    let half_length = (length list) / 2 in
    let left = take half_length list in
    let right = drop half_length list in
    merge cmp (msort cmp left) (msort cmp right)
(* see: msort (fun x y -> x > y) [3; 2; 4; 5; 1; 6];;
   and
   msort (fun x y -> x < y) [3; 2; 4; 5; 1; 6];; *)

(*type: 'a list -> 'a list *)
let rec calm list = 
  match list with 
    [] -> []
  | h::t -> if h = '!' then ('.' :: calm t) else h :: (calm t)


(* Next part: 
   type: (char -> char) -> list -> list
   map (fun x-> if x '!' then '.' else x) list *)

(* One thing I noticed is I'm starting to just autopilot write "let rec", which
   is definitely a bad sign. I'm getting better with recursion, though. *)
let clip x = 
  if x > 10 then 10 else if x < 1 then 1 else x

let cliplist list = 
  map clip list

let cliplist2 list = 
  map (fun x-> if x > 10 then 10 else if x < 1 then 1 else x) list

let rec apply func num arg = 
  if num = 1 then func arg else apply(func) (num-1) (func arg) 

let rec insert cmp x l = 
  match l with 
    [] -> [x]
  | h::t -> 
    if cmp x h then 
      x :: h :: t
    else 
      h :: insert cmp x t

let rec insertion_sort cmp l = 
  match l with 
    [] -> []
  | h::t -> insert cmp h (insertion_sort cmp t)

let rec filter func list = 
  match list with
    [] -> []
  | h::t -> if func h then h :: filter func t else filter func t

let rec for_all_inner func list = 
  match list with
    [] -> true 
  | h::t -> if func h then for_all_inner func t else false 

let rec for_all func list = 
  match list with 
    [] -> false (* no elements to return true on *)
  | h::t -> for_all_inner func list

let rec map1 func list_list = 
  match list_list with
    [] -> [] 
  | h::t -> map func h :: map1 func t