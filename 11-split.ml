let rec split = function
  | [] -> [],[]
  | [q] -> [q],[]
  | head :: mid :: tail ->
    let list_left, list_right = split tail in
      head :: list_left, mid :: list_right;;


let () =
  let list_left, list_right = split [1;2;3;4;5;6;7;8;9] in
    Utils.print_list ( list_left );
    print_endline " ";
    Utils.print_list ( list_right );
