structure HS = HString
structure HL = HList
structure HT = HTuple
open HFunction

con dict = AATree.tree

(*
val eq_dict [k][v] (_ : eq k) (_ : eq v) = AATree.eq_tree
*)

val empty [k][v]: dict k v = AATree.empty

val null [k][v]: (dict k v -> bool) = AATree.null

val singleton [k][v]: (k -> v -> dict k v) = AATree.singleton

val size [k][v]: (dict k v -> int) = AATree.size

val insert [k][v] (_: ord k): (k -> v -> dict k v -> dict k v) = AATree.insert

val adjust [k][v] (_: ord k): ((v -> v) -> k -> dict k v -> dict k v) = AATree.adjust

val delete [k][v] (_: ord k): (k -> dict k v -> dict k v) = AATree.delete

val lookup [k][v] (_: ord k): (k -> dict k v -> option v) = AATree.lookup

val toList [k][v]: (dict k v -> list (k * v)) = AATree.toList

val fromList [k][v] (_ : ord k): (list (k * v) -> dict k v) = AATree.fromList

val mapValues [k][v][w] (f: v -> w) (d1: dict k v): dict k w =

    AATree.mapValues f d1

val mapKeysMonotonic [k][v][k'] (f: k -> k') (d1: dict k v): dict k' v =

    AATree.mapKeysMonotonic f d1

fun filter [k][v] (_: ord k) (prop: k -> bool) (d1: dict k v): dict k v =

    fromList (List.filter (compose prop HT.fst) (toList d1))

fun partition [k][v] (_: ord k) (prop: k -> bool) (d1: dict k v): dict k v * dict k v =
         let 
             val (pos, neg) = HL.partition (compose prop HT.fst) (toList d1)
         in
            (fromList pos, fromList neg)
         end

fun union [k][v] (_: ord k) (d1: dict k v) (d2: dict k v): dict k v =

    List.foldl (HT.uncurry insert) d2 (toList d1)  (* in collision d1 prevales *)

fun deleteAll [k][v] (_: ord k) (ks: list k) (d1: dict k v): dict k v =

    List.foldl delete d1 ks
