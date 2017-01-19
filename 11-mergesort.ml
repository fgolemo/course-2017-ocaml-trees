let rec merge = function
  | list_left,[] -> list_left
  | [],list_right -> list_right
  | head_left::tail_left, head_right::tail_right ->
    if (head_left <= head_right)
    then head_left::merge (tail_left, head_right::tail_right)
    else head_right::merge (head_left::tail_left, tail_right);;

let rec split = function
  | [] -> [],[]
  | [q] -> [q],[]
  | head :: mid :: tail ->
    let list_left, list_right = split tail in
      head :: list_left, mid :: list_right;;

(* let rec mergesort = function
  | [] -> []
  | [x] -> [x]
  | input_list ->
    let list_left, list_right = split input_list in
      merge (mergesort list_left, mergesort list_right) *)

let () =
  Utils.print_list( mergesort [1;6;5;9;2;6;4;8;6;1;8;2] )
