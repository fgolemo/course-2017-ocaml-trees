let binary_search tab x =
  let rec aux tab x left right =
    if left > right then
      false
    else
      let middle = (left + right )/2 in
      let v = tab.(middle) in
      if x = v then true else
      if x < v then aux tab x left ( middle - 1)
      else (* x > v *)
        aux tab x (middle + 1) right
  in aux tab x 0 (Array.length tab - 1);;

(* like i mentioned: this is also possible to implement with Array.sub, 
   but in my opinion way harder than just using the indices.
   If you got a solution working with the Array.sub, then
   props to you and 1 extra point of unnecessary effort :D *)

let () =
  let result = binary_search [|1;2;2;3;4;5;5;7;7;7;9|] 6 in
  if result = true then print_string("yo")
  else print_string("nou");;
