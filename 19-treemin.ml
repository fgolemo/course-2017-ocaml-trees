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

let rec list_of_tree = function
  | Empty -> []
  | Node_b (_,0,left,right) ->
    (list_of_tree left) @ (list_of_tree right)
  | Node_b (value,n,left,right) ->
    (list_of_tree left) @
    value::(list_of_tree (Node_b(value,n-1,Empty,Empty))) @
    (list_of_tree right)
;;

let rec tree_min = function
  | Empty -> Empty,Empty
  | Node_b(value, n, Empty, right) ->
    Node_b(value, n, Empty, Empty), right
  | Node_b(value, n, left, right) ->
    let (mintree, resttree) = tree_min left in
      mintree, Node_b(value, n, resttree, right);;

let rec tree_min_ex = function
  | Node_b(value, n, Empty, _) ->
    value, n
  | Node_b(value, n, left, _) ->
    tree_min_ex left;;

let print_tree tree =
  let rec print_tree_aux tree depth =
    match tree with
      | Empty ->
        let spacing = (String.make depth ' ') in
        print_string( spacing ^ "-> Empty\n" );
      | Node_b(value, n, left, right) ->
        begin
          print_tree_aux right (depth+4);
          let spacing = String.make depth ' ' in
          print_string(
            spacing ^
            "-> " ^
            (string_of_int value) ^
            "("^
            (string_of_int n)^
            ")\n"
          );
          print_tree_aux left (depth+4);
        end;
  in
    begin
      print_string "Printing tree:\n";
      print_tree_aux tree 0;
      print_newline ();
    end;;

let () =
  let min_element, rest_tree = tree_min tree_a in begin
    print_tree min_element;
    print_newline ();
    print_tree rest_tree;
  end;
  print_endline " ";
  let min_value, min_count = tree_min_ex tree_a in
    print_int min_value;
    print_string ",";
    print_int min_count;;
