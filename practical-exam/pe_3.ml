type btree =
  | L
  | N of int * btree * btree;;

let demoTree = N(5, N(2, L, N(1,L,L)), N(6,L,L));;
(* yes I know... really big tree *)


(* check the maximum height... you already did this function in the exam
   (hopefully) *)
let rec max_height = function
  | L -> 0
  | N(_,l,r) -> 1+max (max_height l) (max_height r);;


(* check the minimum height, i.e. the first node
   that has at least one child which is a leaf *)
let rec min_height = function
  | L -> 0
  | N(_,_,L)
  | N(_,L,_) -> 1 (* this also applies to N(_,L,L) *)
  | N(_,l,r) -> 1+min (min_height l) (min_height r);;

let rec is_leftist = function
  | L -> true
  | N(_,l,r) ->
    let min=min_height r in
    let max=max_height l in
    min <= max && (min+1) >= max && is_leftist l &&  is_leftist r;;
(* this previous line does some magic... it checks for multiple things:
   if the left side is smaller or equal size as the right AND
   if the left+1 is bigger or equal to the right side AND
   (these two check mean: is the left subtree same size or 1 higher than the right)
   if the same is true for all the left subtree AND
   if the same is true for all the right subtree
   THEN return true... if any of the above is false, then it's false *)

let () =
  let res = is_leftist demoTree in
  if res = true then print_string("is leftist")
  else print_string("is not leftist");;
