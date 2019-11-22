(*Chapter 4 *)
(* Some test work here, this chapter was confusing. *)
(* let rec length l =
   match l with 
    [] -> 0
   | h::t -> 1 + length l 

   let rec length2 l =
   match l with 
    [] -> 0
   | _::t -> 1 + length l 

   let rec sum_rec l = 
   match l with
    [] -> 0 
   | h::t -> h + sum_rec l *)

let rec length_inner l n = 
  match l with 
    [] -> n
  | h::t -> length_inner t (n+1)

let length l = 
  length_inner l 0

let rec sum_inner l n = 
  match l with
    [] -> n
  | h::t -> sum_inner t (n+h)

let sum l = 
  sum_inner l 0

(* type: 'a list -> 'a list *)
let rec even_elements l = 
  match l with 
    [] -> []
  | [h] -> []
  | _::h::t -> h :: even_elements t 

(* type bool list -> int -> int *)
let rec count_true_inner_tail_recursive l n = 
  match l with
    [] -> 0
  | [true] -> n+1
  | [false] -> n
  | h::t -> if h then count_true_inner_tail_recursive t (n+1) else count_true_inner_tail_recursive t n 

let count_true_tail_recursive l = count_true_inner_tail_recursive l 0

(* type bool list -> int *)
let rec count_true l =
  match l with 
    [] -> 0
  | [true] -> 1
  | [false] -> 0
  | h::t -> if h then 1 + count_true t else count_true t

let rec rev l = 
  match l with 
    [] -> []
  | h::t -> rev t @ [h]

let rec build_palindrome l =
  match l with
    [] -> []
  | [u] -> [u]
  | h::t -> l @ rev l 

let rec drop_last l = 
  match l with 
    [] -> []
  | [u] -> []
  | h::t -> h :: drop_last t

let rec drop_last_tail_recursive_inner l l2 = 
  match l with
    [] -> []
  | h::t -> drop_last_tail_recursive_inner t (h :: l2)

let drop_last_tail_recursive list = 
  rev(drop_last_tail_recursive_inner list [])

let rec is_palindrome list = 
  match list with
    [] -> true
  | [u] -> true
  | [a; b] -> if a <> b then false else true
  | _::t -> is_palindrome(drop_last t)

let rec member ele list = 
  match list with 
    [] -> false
  | h::t -> if h = ele then true else member ele t

let rec make_set_inner l set = 
  match l with
    [] -> set
  | h::t -> 
    if not (member h set)
    then make_set_inner t (h::set) 
    else make_set_inner t set

let make_set l = make_set_inner l [];

  (* The last question makes inquiries into using O(N) space for a method that 
     reverses a list, which isn't ideal. After some internet searching, an easy 
     reverse algorithm that takes  < O(N) time hasn't been found.
  *)