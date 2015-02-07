(* SortedMap *)

structure T = AATree
structure HS = HString
structure HL = HList
open HTuple
open HFunction
open HOrd

type dict k v = T.tree k v

val empty [k][v]: dict k v = T.empty

val null [k][v]: (dict k v -> bool) = T.null

val singleton [k][v]: (k -> v -> dict k v) = T.singleton

val size [k][v]: (dict k v -> int) = T.size

val insert [k][v] (_: ord k): (k -> v -> dict k v -> dict k v) = T.insert

val insertWith [k][v] (_: ord k): ((v -> v -> v) -> k -> v -> dict k v -> dict k v) = T.insertWith

val adjust [k][v] (_: ord k): ((v -> v) -> k -> dict k v -> dict k v) = T.adjust

val delete [k][v] (_: ord k): (k -> dict k v -> dict k v) = T.delete

val lookup [k][v] (_: ord k): (k -> dict k v -> option v) = T.lookup

val toList [k][v]: (dict k v -> list (k * v)) = T.toList

val fromList [k][v] (_ : ord k): (list (k * v) -> dict k v) = T.fromList

val findMinByKey [k][v] : (dict k v -> option (k * v)) = T.findMin

val findMaxByKey [k][v] : (dict k v -> option (k * v)) = T.findMax
    

(* eq instance *)
val eq_dict = fn [k][v] (_ : eq k) (_ : eq v) =>
        let
            fun eq' (t1: dict k v) (t2: dict k v) =
                   let val t1' : T.tree k v = t1
                       val t2' : T.tree k v = t2
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

      T.mapValues f d1

val mapKeysMonotonic [k][v][k'] (f: k -> k') (d1: dict k v): dict k' v =

      T.mapKeysMonotonic f d1

fun foldr [k][v][b] (op: k * v -> b -> b) (acc: b) (d1: dict k v): b =

      T.foldr op acc d1

fun filterFoldr [k][v][b] (prop: k * v -> bool) (op: k * v -> b -> b) (acc: b) (d1: dict k v): b =

      T.filterFoldr prop op acc d1


fun findMinByVal [k][v] (_: ord v) (d1: dict k v): option (k * v) =
   let val optZ : option (k * v) = T.getAnyPair d1
       fun myop (p: k * v) (acc: k * v): k * v =
            if compare (min acc.2 p.2) acc.2 = EQ then acc else p
   in
       case optZ of
         None => None
         | Some z => Some (foldr myop z d1)
   end

fun findMaxByVal [k][v] (_: ord v) (d1: dict k v): option (k * v) =
   let val optZ : option (k * v) = T.getAnyPair d1
       fun myop (p: k * v) (acc: k * v): k * v =
            if compare (max acc.2 p.2) acc.2 = EQ then acc else p
   in
       case optZ of
         None => None
         | Some z => Some (foldr myop z d1)
   end


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
    let fun op (kv: k * v) (pt: dict k v * dict k v): dict k v * dict k v =
               if uncurry prop kv
                   then (uncurry insert kv pt.1, pt.2)
                   else (pt.1, uncurry insert kv pt.2)
    in foldr op (empty, empty) d1
    end

fun union [k][v] (_: ord k) (d1: dict k v) (d2: dict k v): dict k v = foldr (uncurry insert) d2 d1

fun unionWith [k][v] (_: ord k)(f: v -> v -> v) (d1: dict k v) (d2: dict k v): dict k v =

      foldr (uncurry (insertWith f)) d2 d1

fun diff [k][v] (_: ord k) (d1: dict k v) (d2: dict k v): dict k v = foldr (compose delete fst) d1 d2

fun deleteAll [k][v] (_: ord k) (ks: list k) (d1: dict k v): dict k v =

      List.foldl delete d1 ks

fun keys [k][v] (d1: dict k v): list k = List.mp fst (toList d1)

val values [k][v] (d1: dict k v): list v = List.mp snd (toList d1)

fun all [k][v] (prop: v -> bool) (t: dict k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b && prop pair.2
    in
      foldr myop True t
    end

fun any [k][v] (prop: v -> bool) (t: dict k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b || prop pair.2
    in
      foldr myop False t
    end

fun allWithKey [k][v] (prop: k -> v -> bool) (t: dict k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b && uncurry prop pair
    in
      foldr myop True t
    end

fun anyWithKey [k][v] (prop: k -> v -> bool) (t: dict k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b || uncurry prop pair
    in
      foldr myop False t
    end

fun sumBy [k][v][b] (_:num b) (proj: v -> b) (d1: dict k v): b =
    let
        fun myop (pair: k * v) (acc: b): b = acc + proj pair.2
    in
      foldr myop zero d1
    end

fun intProdBy [k][v] (proj: v -> int) (d1: dict k v): int =
    let
        fun myop (pair: k * v) (acc: int): int = acc * proj pair.2
    in
      foldr myop 1 d1
    end

fun floatProdBy [k][v] (proj: v -> float) (d1: dict k v): float =
    let
        fun myop (pair: k * v) (acc: float): float = acc * proj pair.2
    in
      foldr myop 1.0 d1
    end

val valid [k][v] (_: ord k): (dict k v -> bool) = T.valid