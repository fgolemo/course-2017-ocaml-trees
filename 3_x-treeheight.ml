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

let rec tree_height_aux tree depth = match tree with
  | Empty -> depth
  | Node_b (value, _, left, right) ->
    let height_left = tree_height_aux left (depth+1) in
      let height_right = tree_height_aux right (depth+1) in
        if height_left > height_right then height_left
        else height_right;;


let tree_height tree =
  tree_height_aux tree (-1);;

let () =
  print_int (tree_height tree_a);;
