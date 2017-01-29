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

let rec tree_insert val_ins tree = match tree with
  | Empty -> Node_b(val_ins,1,Empty,Empty)
  | Node_b(value, n, left, right) ->
    if val_ins = value then
      Node_b(value, n+1, left, right)
    else
      if val_ins < value then
        Node_b(value, n, (tree_insert val_ins left), right)
      else
        Node_b(value, n, left, (tree_insert val_ins right));;

let rec list_of_tree = function
  | Empty -> []
  | Node_b (_,0,left,right) ->
    (list_of_tree left) @ (list_of_tree right)
  | Node_b (value,n,left,right) ->
    (list_of_tree left) @
    value::(list_of_tree (Node_b(value,n-1,Empty,Empty))) @
    (list_of_tree right)
;;

let () =
  Utils.print_list (list_of_tree tree_a);
  print_endline " ";
  let tree_a1 = tree_insert 9 tree_a in begin
    Utils.print_list (list_of_tree tree_a1);
    print_endline " ";
    let tree_a2 = tree_insert 10 tree_a1 in
      Utils.print_list (list_of_tree tree_a2);
  end;;
