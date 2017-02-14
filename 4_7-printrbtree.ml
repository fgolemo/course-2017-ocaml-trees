type 'e redblacktree =
| Black of 'e redblacktree * 'e * 'e redblacktree
| Red of 'e redblacktree * 'e * 'e redblacktree
| Nil

let tree_rb =
  Black(
    Red(
      Black(
        Red(Nil, 3, Nil),
        4,
        Red(Nil, 7, Nil)
        ),
      10,
      Black(Nil, 12, Nil)
      ),
    13,
    Red(
      Black(
        Nil,
        15,
        Red(Nil, 20, Nil)
        ),
      30,
      Black(
        Nil,
        35,
        Red(Nil, 40, Nil)
        )
      )
    );;

let spacing depth =
  String.make depth ' ';;

let print_node rb depth value =
  print_string(
    (spacing depth) ^ "-> " ^ rb ^ "(" ^ (string_of_int value) ^ ")\n"
  );;

let rec print_tree_aux tree depth =
  match tree with
    | Nil ->
      print_string( (spacing depth) ^ "-> Nil\n" );
    | Red(left, value, right) ->
      begin
        print_tree_aux right (depth+4);
        print_node "R" depth value;
        print_tree_aux left (depth+4);
      end;
    | Black(left, value, right) ->
      begin
        print_tree_aux right (depth+4);
        print_node "B" depth value;
        print_tree_aux left (depth+4);
      end;;

let print_rb_tree tree =
  begin
    print_string "Printing tree:\n";
    print_tree_aux tree 0;
    print_newline ();
  end;;

let () =
  print_rb_tree tree_rb;;
