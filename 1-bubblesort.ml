let bubblesort arr =
  let arr_len = Array.length arr in begin
    for i = 0 to (arr_len-2) do
      for j = i+1 to (arr_len-1) do
        if (arr.(i) > arr.(j)) then
          let tmp = arr.(j) in begin
            arr.(j) <- arr.(i);
            arr.(i) <- tmp;
          end;
        done;
      done;
    arr;
  end;;


let () =
  Utils.print_array (bubblesort [|4;3;7;3;7;0;9;1|])
