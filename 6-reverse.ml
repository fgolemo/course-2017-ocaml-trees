let rec reverse input_list =
  match input_list with
  | [] -> []
  | head :: tail ->
    let rest = reverse tail in
      rest @ [head];;

let () =
  Utils.print_list [1;2;3;4;5];
  print_endline " ";
  Utils.print_list (reverse [1;2;3;4;5])
