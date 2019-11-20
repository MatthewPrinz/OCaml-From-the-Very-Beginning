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

(* let rec length_inner l n = 
   match l with 
    [] -> n
   | h::t -> length_inner t (n+1)

   let length l = 
   length_inner l 0 *)

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

let count_true l = count_true_inner_tail_recursive l 0

(* type bool list -> int *)
let rec count_true_inner l =
  match l with 
    [] -> 0
  | [true] -> 1
  | [false] -> 0
  | h::t -> if h then 1 + count_true_inner t else count_true_inner t


