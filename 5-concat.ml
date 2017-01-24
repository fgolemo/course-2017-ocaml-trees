let rec concat list1 list2 =
  match list1 with
  | [] -> list2
  | head :: tail ->
    let rest_concat = concat tail list2 in
    head :: rest_concat;;

let () =
  (* will print 95137  *)
  Utils.print_list ([9;5;1] @ [3;7;]);
  print_endline "";
  (* will print 95137  *)
  Utils.print_list (concat [9;5;1] [3;7])
