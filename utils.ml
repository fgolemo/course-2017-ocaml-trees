let rec print_list input_list =
  match input_list with
  | [] -> ()
  | head :: tail ->
    print_string (string_of_int head);
    print_list tail;;

let rec print_array input_arr =
  let arr_len = Array.length input_arr in
    for i=0 to (arr_len-1) do
      print_string (string_of_int input_arr.(i))
    done;
