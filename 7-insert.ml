let rec insert input_list element =
  match input_list with
  | [] -> [element]
  | head :: tail ->
    if head < element
    then head :: insert tail element
    else element :: input_list;;

let () =
  Utils.print_list (insert [1;2;3;4;6;7] 5)
