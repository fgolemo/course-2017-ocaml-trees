type tree234 =
  | L
  | N2 of int * tree234 * tree234
  | N3 of int * int * tree234 * tree234 * tree234
  | N4 of int * int * int * tree234 * tree234 * tree234 * tree234;;

let rec search234 tree s = match tree with
  | L -> (false, -1)
  | N2(item, l, r) -> if s < item then search234 l s
    else if s > item then search234 r s
    else (true, 2)
  | N3(item1, item2, l, m, r) -> if s < item1 then search234 l s
    else if s > item1 && s < item2 then search234 m s
    else if s > item2 then search234 r s
    else (true, 3)
  | N4(item1, item2, item3, l, m1, m2, r) -> if s < item1 then search234 l s
    else if s > item1 && s < item2 then search234 m1 s
    else if s > item2 && s < item3 then search234 m2 s
    else if s > item3 then search234 r s
    else (true, 4);;

(* I feel like there is a more elegant solution to the above
   because a lot of it is just copy&paste.
   But I wrote this implementation at 4am so don't judge me. *)

let demoTree =
  N2(5,N4(1,3,4,L,L,L,L),N3(8,10,L,N2(9,L,L),L));;

let () =
  let res, layer = search234 demoTree 11 in
  if res = true then print_string("found in layer "^(string_of_int layer))
  else print_string("not found");;
