(* This is the definition that we will use for the course *)
(* ...with data in the nodes *)
type 'e binary_tree =
  | Empty
  | Node_b of 'e * int * 'e binary_tree * 'e binary_tree

(* This is the definition from the official ocaml website.*)
(* All the data is in the leafs. So we wont use this one. *)
(* But many other algorithms use this, so it might be useful to you. *)
type 'f binary_tree2 =
   | Leaf of 'f * int
   | Tree_b of 'f binary_tree * 'f binary_tree;;
