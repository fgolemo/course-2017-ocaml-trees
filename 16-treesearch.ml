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

(* Search for an element in the tree
...and return its count (n), or 0 if it's not found *)
let rec tree_search search tree = match tree with
  | Empty -> 0
  (* in the next line, don't use 'val', use 'value', otherwise weird error *)
  | Node_b (value, n, left, right) ->
    if value == search then n
    else
      if search<value then
        tree_search search left
      else
        tree_search search right;;

let () =
  (* this should return 3 *)
  print_int (tree_search 4 tree_a);
  print_endline " ";
  (* this should return 0 *)
  print_int (tree_search 10 tree_a);;
