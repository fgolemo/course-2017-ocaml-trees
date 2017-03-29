type htree =
  | L of char
  | N of htree * htree;;

let example_htree =
  N(
    N(
      N(
        L('e'),
        L('a')
      ),
      L('n')
    ),
    L('t')
  );;

let bitstring = "1000011001011";;

let list_of_bits bits =
  let rec aux index acc =
    if index > -1 then
      aux (index-1) ((int_of_char bits.[index] - 48)::acc)
    else
      acc;
  in aux (String.length bits - 1) [];;

let rec htree_read bits htree_in =
  match htree_in, bits with
  | L x, _ -> x, bits
  | N(a0, a1), b::bits' -> htree_read
                             bits'
                             (if b = 1 then a1 else a0)
  | _ -> failwith "no element";;

let string_of_charlist charlist = String.concat "" (List.map (String.make 1) charlist);;

let () =
  (* Utils.print_list (list_of_bits biiits);; *)
  let char1, bitlist1 = htree_read (list_of_bits bitstring) example_htree in
  begin
    print_char(char1);
    print_newline();
    Utils.print_list(bitlist1);
    print_newline();
    let char2, bitlist2 = htree_read bitlist1 example_htree in
    begin
      print_char(char2);
      print_newline();
      Utils.print_list(bitlist2);
      print_newline();
      let char3, bitlist3 = htree_read bitlist2 example_htree in
      begin
        print_char(char3);
        print_newline();
        Utils.print_list(bitlist3);
      end
    end
  end;;
