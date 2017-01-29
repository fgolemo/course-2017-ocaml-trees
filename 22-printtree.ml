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
  print_tree tree_a;;
