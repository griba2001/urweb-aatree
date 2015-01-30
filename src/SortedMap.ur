structure HS = HString
structure HL = HList
open HTuple
open HFunction

con dict k v = AATree.tree k v

val empty [k][v]: dict k v = AATree.empty

val null [k][v]: (dict k v -> bool) = AATree.null

val singleton [k][v]: (k -> v -> dict k v) = AATree.singleton

val size [k][v]: (dict k v -> int) = AATree.size

val insert [k][v] (_: ord k): (k -> v -> dict k v -> dict k v) = AATree.insert

val insertWith [k][v] (_: ord k): ((v -> v -> v) -> k -> v -> dict k v -> dict k v) = AATree.insertWith

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

fun foldr [k][v][b] (op: k * v -> b -> b) (acc: b) (d1: dict k v): b =

      AATree.foldr op acc d1

fun filterFoldr [k][v][b] (prop: k * v -> bool) (op: k * v -> b -> b) (acc: b) (d1: dict k v): b =

      AATree.filterFoldr prop op acc d1

fun filter [k][v] (_: ord k) (prop: v -> bool) (d1: dict k v): dict k v =
      let fun prop' (p: k * v): bool = prop p.2
      in 
         filterFoldr prop' (uncurry insert) empty d1
      end

fun filterWithKey [k][v] (_: ord k) (prop: k -> v -> bool) (d1: dict k v): dict k v =

      filterFoldr (uncurry prop) (uncurry insert) empty d1


fun partition [k][v] (_: ord k) (prop: v -> bool) (d1: dict k v) : dict k v * dict k v =
    let fun prop' (p: k * v): bool = prop p.2
        fun op (kv: k * v) (pt: dict k v * dict k v): dict k v * dict k v =
                          if prop' kv then (uncurry insert kv pt.1, pt.2)
                          else (pt.1, uncurry insert kv pt.2)
    in foldr op (empty, empty) d1
    end

fun partitionWithKey [k][v] (_: ord k) (prop: k -> v -> bool) (d1: dict k v) : dict k v * dict k v =
    let fun prop' (p: k * v): bool = prop p.1 p.2
        fun op (kv: k * v) (pt: dict k v * dict k v): dict k v * dict k v =
                          if prop' kv then (uncurry insert kv pt.1, pt.2)
                          else (pt.1, uncurry insert kv pt.2)
    in foldr op (empty, empty) d1
    end

val union [k][v] (_: ord k): (dict k v -> dict k v -> dict k v) = AATree.union

fun unionWith [k][v] (_: ord k)(f: v -> v -> v) (d1: dict k v) (d2: dict k v): dict k v =

      foldr (uncurry (insertWith f)) d2 d1

val diff [k][v] (_: ord k): (dict k v -> dict k v -> dict k v) = AATree.difference

fun deleteAll [k][v] (_: ord k) (ks: list k) (d1: dict k v): dict k v =

      List.foldl delete d1 ks
