structure HS = HString
structure HL = HList
structure HT = HTuple
open HFunction

con dict k v = AATree.tree k v

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

val findMin [k][v] : (dict k v -> option (k * v)) = AATree.findMin

val findMax [k][v] : (dict k v -> option (k * v)) = AATree.findMax


(* eq instance *)
val eq_dict = fn [k][v] (_ : eq k) (_ : eq v) =>
        let
            fun eq' (t1: dict k v) (t2: dict k v) =
                   let val t1' : AATree.tree k v = t1
                       val t2' : AATree.tree k v = t2
                   in t1' = t2'
                   end
        in
            mkEq eq'
        end

(* show instance *)
val show_dict = fn [k][v] (_ : show (k * v)) =>
        let
            fun show' (t1: dict k v)  = "dict fromList: " ^ show (toList t1)
        in
            mkShow show' : show (dict k v)
        end


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

val union [k][v] (_: ord k): (dict k v -> dict k v -> dict k v) = AATree.union

fun deleteAll [k][v] (_: ord k) (ks: list k) (d1: dict k v): dict k v =

    List.foldl delete d1 ks
