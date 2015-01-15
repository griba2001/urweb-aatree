structure HS = HString
structure HL = HList
structure HT = HTuple
open HFunction

con dict k v = AATree.tree k v

(*
val eq_dict [k][v] (_ : eq k) (_ : eq v) = AATree.eq_tree
*)

val empty [k][v]: dict k v = AATree.empty

val null [k][v]: (dict k v -> bool) = AATree.null

val singleton [k][v] (k1: k) (v1: v): dict k v = AATree.singleton k1 v1

val put [k][v] (_: ord k) (k1: k) (v1: v) (d1: dict k v): dict k v = AATree.insert k1 v1 d1

val delete [k][v] (_: ord k) (k1: k) (d1: dict k v): dict k v = AATree.delete k1 d1

val lookup [k][v] (_: ord k) (k1: k) (d1: dict k v): option v = AATree.lookup k1 d1

fun toList [k][v] (d1: dict k v): list (k * v) = AATree.toList d1

fun fromList [k][v] (_ : ord k) (li: list (k * v)): dict k v = AATree.fromList li

val mapValues [k][v][w] (f: v -> w) (d1: dict k v): dict k w = AATree.mapValues f d1

val mapKeysMonotonic [k][v][k'] (f: k -> k') (d1: dict k v): dict k' v = AATree.mapKeysMonotonic f d1

fun filter [k][v] (_: ord k) (prop: k -> bool) (d1: dict k v): dict k v =
         fromList (List.filter (compose prop HT.fst) (toList d1))

fun partition [k][v] (_: ord k) (prop: k -> bool) (d1: dict k v): dict k v * dict k v =
         let 
             val (pos, neg) = HL.partition (compose prop HT.fst) (toList d1)
         in
            (fromList pos, fromList neg)
         end
