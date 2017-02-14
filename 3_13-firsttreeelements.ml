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

let rec smaller_than tree element =
  match tree with
  | Empty -> []
  | Node_b (_,0,left,right) ->
    (smaller_than left element) @ (smaller_than right element)
  | Node_b (value,n,left,right) ->
    if value <= element then
    (smaller_than left element) @
    value::(smaller_than (Node_b(value,n-1,Empty,Empty)) element) @
    (smaller_than right element)
    else
    (smaller_than left element)
;;

let () =
  Utils.print_list (smaller_than tree_a 11);;
