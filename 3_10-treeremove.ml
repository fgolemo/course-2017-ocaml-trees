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

let rec tree_min = function
  | Empty -> Empty,Empty
  | Node_b(value, n, Empty, right) ->
    Node_b(value, n, Empty, Empty), right
  | Node_b(value, n, left, right) ->
    let (mintree, resttree) = tree_min left in
      mintree, Node_b(value, n, resttree, right);;

let rec tree_max = function
  | Empty -> Empty,Empty
  | Node_b(value, n, left, Empty) ->
    Node_b(value, n, Empty, Empty), left
  | Node_b(value, n, left, right) ->
    let (maxtree, resttree) = tree_max right in
      maxtree, Node_b(value, n, left, resttree);;

let tree_remove_aux value n left right =
  if n>1 then Node_b(value, (n-1), left, right)
  else
    if left = Empty then
      let (min_tree, rest_tree) = tree_min right in
        match min_tree with
        | Empty -> Empty
        | Node_b(value2, n2, _, _) ->
          Node_b(value2, n2, left, rest_tree)
    else
      let (max_tree, rest_tree) = tree_max left in
        match max_tree with
        | Empty -> Empty
        | Node_b(value2, n2, _, _) ->
          Node_b(value2, n2, rest_tree, right)

let rec tree_remove search tree = match tree with
  | Empty -> failwith "element not found"
  | Node_b (value, n, left, right) ->
    if search = value then
      tree_remove_aux value n left right
    else
      if search<value then
        Node_b(value, n, (tree_remove search left),right)
      else
        Node_b(value, n, left, (tree_remove search right))

let () =
    print_tree tree_a;
    print_newline ();
    print_tree (tree_remove 8 tree_a);;
