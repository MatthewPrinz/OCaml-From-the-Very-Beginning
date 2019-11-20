(* Chapter 2*)
let mult10 x = x*10
let nonzeros x y = 
  if x <> 0 && y <> 0 
  then true 
  else false
let rec plussum n = 
  if n = 1 
  then 1 
  else  n + plussum(n-1)
let rec power x n =
  if n = 0
  then 1
  else  x * power x (n-1)
(* vs. 
 **let rec power x n =
   if n = 0
   then 1
   else  x * power x n-1** 
   above gives a stack overflow error
   Notable that functions take precedence over arithmetic operations *)
let isconsonant a = 
  a <> 'a' && a <> 'e' && a <> 'i' && a <> 'o' && a <> 'u'

(* I'll just rewrite factorial *)
(* Wikipedia defines factorial only over postive integers, so I will just use 
   an "error" message of -1 *)
let rec factorial n = 
  if n < 0 then -1
  else 
  if n = 0 then 1
  else
    n * factorial(n-1)


