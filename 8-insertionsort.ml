let rec insert input_list element =
  match input_list with
  | [] -> [element]
  | head :: tail ->
    if head < element
    then head :: insert tail element
    else element :: input_list;;

let rec insertionsort input_list =
  let rec insertionsort_rec list_right list_left =
    match list_right with
    | [] -> list_left
    | head :: tail ->
      let list_rest = insert list_left head in
        insertionsort_rec tail list_rest
  in
  insertionsort_rec input_list [];;

let () =
  Utils.print_list (insertionsort [5;8;3;9;2;6;5;9;1])
