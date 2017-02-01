let rec merge = function
  | list_left,[] -> list_left
  | [],list_right -> list_right
  | head_left::tail_left, head_right::tail_right ->
    if (head_left <= head_right)
    then head_left::merge (tail_left, head_right::tail_right)
    else head_right::merge (head_left::tail_left, tail_right);;

let () =
  Utils.print_list ( merge ([1;3;4;5;7;9],[2;6;8]) );
  print_endline " ";
  Utils.print_list ( merge ([1;3;4;5;7;9],[8;6;2]) );
