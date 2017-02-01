type 'e dtq_tree =
  | Empty (* IRL this would be your data, and 'e would be int32 or int64 *)
  | Node_two of 'e dtq_tree * 'e * 'e dtq_tree
  | Node_three of 'e dtq_tree * 'e * 'e dtq_tree * 'e * 'e dtq_tree
  | Node_four of 'e dtq_tree * 'e * 'e dtq_tree * 'e * 'e dtq_tree * 'e * 'e dtq_tree

type dtq_tree_int =
  | Empty_int
  | Node_two_int of dtq_tree_int * int * dtq_tree_int
  | Node_three_int of dtq_tree_int * int * dtq_tree_int * int * dtq_tree_int
  | Node_four_int of dtq_tree_int * int * dtq_tree_int * int * dtq_tree_int * int * dtq_tree_int
