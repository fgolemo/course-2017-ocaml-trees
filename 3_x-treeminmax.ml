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

let rec tree_min tree value = match tree with
  | Empty -> value
  | Node_b(value, _, left, _) -> tree_min left value;;

let rec tree_max tree value = match tree with
  | Empty -> value
  | Node_b(value, _, _, right) -> tree_max right value;;

let tree_min_max tree = match tree with
  | Empty -> -1,-1
  | Node_b (value, n, left, right) ->
    let min = tree_min left value in
      let max = tree_max right value in
        min, max;;

let () =
  let mint, maxt = tree_min_max tree_a in
    begin
      print_int mint;
      print_newline();
      print_int maxt;
    end;;
