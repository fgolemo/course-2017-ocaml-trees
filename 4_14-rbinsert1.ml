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

let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
    Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d);;

let blackify = function
  | Node(_, value, left, right) ->
    Node(Black, value, left, right)
  | Nil -> failwith "this shouldn't happen";;


let insert value tree =
  let rec ins = function
    | Nil -> Node (Red, value, Nil, Nil)
    | Node (color, y, a, b) as node ->
      if value < y then balance (color, y, ins a, b)
      else if value > y then balance (color, y, a, ins b)
      else node
  in
  blackify (ins tree);;


let () =
  begin
    (* BEFORE *)
    (* graph_rb_tree tree_rb; *)

    (* AFTER (a) *)
    let tree_rb2 = (insert 2 tree_rb) in
    graph_rb_tree tree_rb2;
  end;;
