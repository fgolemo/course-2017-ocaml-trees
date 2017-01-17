let rec slice input_list n =
  match input_list,n with
  | _,0 -> []
  | [],_ -> failwith "not enough elements"
  | head :: tail,_ ->
    let rest = slice tail (n-1) in
      head :: rest;;

let () =
  Utils.print_list (slice [1;2;3;4;5;6;7] 3)
