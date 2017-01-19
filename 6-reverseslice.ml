let rec slice input_list n =
  match input_list,n with
  | _,0 -> []
  | [],_ -> failwith "not enough elements"
  | head :: tail,_ ->
    let rest = slice tail (n-1) in
      head :: rest;;

let rec reverse input_list =
  match input_list with
  | [] -> []
  | head :: tail ->
    let rest = reverse tail in
      rest @ [head];;

(* this returns the last N elements of a list *)
let reverseslice input_list n =
  let list_inversed = reverse input_list in
  let list_sliced = slice list_inversed n in
  reverse list_sliced

let () =
  Utils.print_list(reverseslice [1;2;3;4;5] 3)
  (* should print '345' *)
