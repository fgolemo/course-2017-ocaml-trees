type 'e binary_tree =
  | Empty
  | Node_b of 'e * int * 'e binary_tree * 'e binary_tree;;

let tree_a =
  Node_b(12,1,
    Node_b(8,1,
      Node_b(4,3,Empty,Empty),
      Node_b(9,2,Empty,
        Node_b(11,2,Empty,Empty))),
    Node_b(23,1,Empty,Empty));;

(* This implementation is a little bit more intuitive, but has a bug with
certain subtrees (printing wrong order). This is only here for historic
reasons. *)
let rec list_of_tree_shitty = function
  | Empty -> []
  | Node_b (value, n, left, right) ->
    if n > 1 then
      value :: list_of_tree_shitty (Node_b(value, n-1, left, right))
    else
      list_of_tree_shitty left @ [value] @ list_of_tree_shitty right;;

(* This is at least a correct implementation, but there is still room
for performance improvement (for example by using a 'for' loop to
print multiple elements, instead of creating tons of little lists) *)
let rec list_of_tree = function
  | Empty -> []
  | Node_b (_,0,left,right) ->
    (list_of_tree left) @ (list_of_tree right)
  | Node_b (value,n,left,right) ->
    (list_of_tree left) @
    value::(list_of_tree (Node_b(value,n-1,Empty,Empty))) @
    (list_of_tree right)
;;

(* Again, if you see the 'Utils' module, that is my utils.ml function.
I mostly use it for printing lists. If your compiler can't
find the module, just remove it and manually look at the list,
or copy&paste the code for list printing here, from the utils.ml file *)
let () =
  Utils.print_list (list_of_tree tree_a);;
