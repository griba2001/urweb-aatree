structure HS = HString

con dict k v = AATree.tree k v

(*
val eq_dict [k][v] (_ : eq k) (_ : eq v) = AATree.eq_tree
*)

val empty [k][v]: dict k v = AATree.empty

val null [k][v]: (dict k v -> bool) = AATree.null

val singleton [k][v] (k1: k) (v1: v) = AATree.singleton k1 v1

val put [k][v] (_: ord k) (k1: k) (v1: v) (d1: dict k v) = AATree.insert k1 v1 d1

val delete [k][v] (_: ord k) (k1: k) (d1: dict k v) = AATree.delete k1 d1

val lookup [k][v] (_: ord k) (k1: k) (d1: dict k v): option v = AATree.lookup k1 d1
