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

let tree_of_list l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | head :: tail -> aux tail (tree_insert head acc)
  in aux l Empty

let tree_of_list2 l =
  match l with
  | [] -> Empty
  | head :: tail -> tree_insert head (tree_of_list tail)


let () =
  print_tree tree_a;
  print_newline ();
  let tree_b = tree_of_list [4;4;4;8;9;9;11;11;12;23] in
    print_tree tree_b;
  print_newline ();
  let tree_c = tree_of_list [11;4;12;23;9;8;9;4;11;4] in
    print_tree tree_c;;
  let tree_d = tree_of_list2 [11;4;12;23;9;8;9;4;11;4] in
    print_tree tree_d;;
