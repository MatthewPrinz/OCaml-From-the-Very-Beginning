(* Chapter 5: Sorting Things *)
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

let rec insert x l = 
  match l with 
    [] -> [x]
  | h::t -> 
    if x <= h then 
      x :: h :: t
    else 
      h :: insert x t

let rec insertion_sort l = 
  match l with 
    [] -> []
  | h::t -> insert h (insertion_sort t)

let rec merge l1 l2 = 
  match l1, l2 with
    l, [] -> l
  | [], l -> l
  | h1::t1, h2::t2 ->
    if h1 <= h2 then 
      h1 :: merge t1 (h2::t2)
    else
      h2 ::  merge (h1::t1) t2

let rec msort list = 
  match list with
    [] -> []
  | [x] -> [x]
  | _ -> 
    let half_length = (length list) / 2 in
    let left = take half_length list in
    let right = drop half_length list in
    merge (msort left) (msort right)


(* 2: The only way take or drop fails is if we call one of the two with an n < 
   0. Since the length of a list is always >0, we know that half of the length
   will never go negative. Also, half_length < length, so we won't remove more
   elements than are inside the list.*)

let rec rev_ins x list = 
  match list with
    [] -> [x]
  | h::t -> 
    if x > h then
      x :: h :: t
    else 
      h :: rev_ins x t


let rec reverse_sort list = 
  match list with 
    [] -> []
  | [x] -> [x]
  | h::t -> rev_ins h (reverse_sort t)

let rec is_sorted list = 
  match list with
    [] -> true
  | [x] -> true
  | h::m::t -> 
    if h < m then is_sorted (m::t) else false

(* 5. Comparison functions on lists compare element by element until they find
   the first difference. For the first difference, the boolean value of 
   that result will be used for the entire list. If the elements are equal but the lists
   have separate lengths, the longer list will be > than the shorter.  So, [1; 2] 
   < [2; 3] would return true. So, the following char_list_list will sort like so:
   ['o'; 'n'; 'e']; ['t'; 'h'; 'r'; 'e'; 'e']; ['t'; 'w'; 'o']) because the 'o' < 
   't' and 'h' < 'w'. *)

(* Will admit, I looked at the hints for this one. *)
let rec insertion_sort_one_func unsorted_list = 
  match unsorted_list with 
    [] -> []
  | x::l -> 
    let rec insert x l = 
      match l with 
        [] -> [x]
      | h::t -> 
        if x <= h then 
          x :: h :: t
        else 
          h :: insert x t in
    insert x (insertion_sort_one_func l)