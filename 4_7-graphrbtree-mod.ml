type color =
  | Red
  | Black;;

type 'a rbtree =
  | Node of color * 'a * 'a rbtree * 'a rbtree
  | Nil;;

let string_of_color col =
  if col = Black then "black"
  else "red";;

let tree_rb =
  Node(Black, 13,
       Node(Red, 10,
            Node(Black, 4,
                 Node(Red, 3, Nil, Nil),
                 Node(Red, 7, Nil, Nil)
                ),
            Node(Black, 12, Nil, Nil)
           ),
       Node(Red, 30,
            Node(Black, 15,
                 Nil,
                 Node(Red, 20, Nil, Nil)
                ),
            Node(Black, 35,
                 Nil,
                 Node(Red, 40, Nil, Nil)
                )
           )
      );;

let string x = string_of_int x;;

let rec print_nil x leftright =
  begin
    (* create edge *)
    print_string("\"" ^ (string x) ^ "\" -> \"" ^ (string x) ^ leftright ^ "\"\n");
    (*  create node *)
    print_string("\"" ^ (string x) ^ leftright ^ "\" [style=filled, shape=square, label=\"\"]\n");
  end;;

let rec print_child x x2 =
  print_string("\"" ^ (string x) ^ "\" -> \"" ^ (string x2) ^ "\"\n");;

let rec graph_tree_aux2 tree =
  let rec print_node left x right color =
    begin
      (* print node and color *)
      print_string("\"" ^ (string x) ^ "\" [color=\"" ^ (string_of_color color) ^ "\"]\n");

      begin
        match left with
        | Nil -> print_nil x "l"
        | Node(_,x2,_,_) -> print_child x x2
      end;

      begin
        match right with
        | Nil -> print_nil x "r"
        | Node(_,x2,_,_) -> print_child x x2
      end;

      graph_tree_aux2 left;
      graph_tree_aux2 right;
    end;
  in
  match tree with
  | Nil -> ()
  | Node(col, x, left, right) -> print_node left x right col;;

let graph_rb_tree tree =
  begin
    print_string "digraph G {\n";
    graph_tree_aux2 tree;
    print_string "}\n";
  end;;

let () =
  graph_rb_tree tree_rb;;
