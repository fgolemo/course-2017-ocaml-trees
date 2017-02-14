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

let graph_tree tree =
  let rec graph_tree_aux = function
      | Empty -> ()
      | Node_b(value, n, left, right) ->
        begin
          let print_node = ((string_of_int value) ^ ", " ^ (string_of_int n)) in
            let print_prefix = ("\"" ^ print_node ^ "\" -> \"") in
            begin
              begin
              match left with
                | Empty -> print_string (
                  print_prefix ^
                  print_node ^
                  "_l\";\n" ^
                  "\"" ^ print_node ^
                  "_l\" [ style = filled, label = \"\" ];\n");
                | Node_b(left_val, left_n, _, _) ->
                  print_string (
                    print_prefix ^
                    (string_of_int left_val) ^
                    ", " ^
                    (string_of_int left_n) ^
                    "\";\n");
              end;
              (* print right child *)
              begin
              match right with
                | Empty ->
                  print_string (
                    print_prefix ^
                    print_node ^
                    "_r\";\n" ^
                    "\"" ^ print_node ^
                    "_r\" [ style = filled, label = \"\" ];\n");
                | Node_b(right_val, right_n, _, _) ->
                  print_string (
                    print_prefix ^
                    (string_of_int right_val) ^
                    ", " ^
                    (string_of_int right_n) ^
                    "\";\n");
              end;
              graph_tree_aux right;
              graph_tree_aux left;

          end;
        end;
  in
    begin
      print_string "digraph G {\n";
      graph_tree_aux tree;
      print_string "}\n";
    end;;

let () =
  graph_tree tree_a;;
