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

let spacing depth = String.make depth ' ';;

let print_node value n depth=
  print_string(
    (spacing depth) ^ "-> " ^ (string_of_int value) ^ "(" ^ (string_of_int n) ^ ")\n"
  );;

let rec print_tree_aux tree depth =
  match tree with
    | Empty -> print_string( (spacing depth) ^ "-> Empty\n" );
    | Node_b(value, n, left, right) ->
      begin
        print_tree_aux right (depth+4);
        print_node value n depth;
        print_tree_aux left (depth+4);
      end;;

let print_tree tree =
  begin
    print_string "Printing tree:\n";
    print_tree_aux tree 0;
    print_newline ();
  end;;


let rotate_left = function
  | Empty -> Empty
  | Node_b(value, n, left, Empty) as s -> s
  | Node_b(value, n, left, Node_b(value2, n2, left2, right2)) ->
    Node_b(value2, n2, Node_b(value, n, left, left2), right2);;

let rotate_right = function
  | Empty -> Empty
  | Node_b(value, n, Empty, right) as s -> s
  | Node_b(value, n, Node_b(value2, n2, left2, right2), right) ->
    Node_b(value2, n2, left2, Node_b(value, n, right2, right));;

let () = begin
  print_tree tree_a;
  let tree_left = rotate_left tree_a in
    print_tree tree_left;
  let tree_right = rotate_right tree_a in
    print_tree tree_right;

end;;
