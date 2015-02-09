(* UnordHashMap *)

structure T = UnordHashTree
structure O = Option
structure M = Monad
(* structure HL = HList *)
open HTuple
open HFunction

open Hashable.Hashable

type hashMap k v = T.hashTree k v

(* * Construction *)

val empty [k][v] (_:hashable k): hashMap k v = T.empty

val singleton [k][v] (_:hashable k): (k -> v -> hashMap k v) = T.singleton

(* * Query *)

val null [k][v] (_:hashable k) : hashMap k v -> bool = T.null

val size [k][v] (_:hashable k): (hashMap k v -> int) = T.size

val lookup [k][v] (_:hashable k) (_ : eq k): (k -> hashMap k v -> option v) = T.lookup

(* * Mutation *)

val insertWith [k][v] (_:hashable k) (_ : eq k): ((v -> v -> v) -> k -> v -> hashMap k v -> hashMap k v) = T.insertWith

val insert [k][v] (_:hashable k) (_ : eq k): (k -> v -> hashMap k v -> hashMap k v) = T.insert

val delete [k][v] (_:hashable k) (_ : eq k): (k -> hashMap k v -> hashMap k v) = T.delete

val adjust [k][v] (_:hashable k) (_ : eq k): ((v -> v) -> k -> hashMap k v -> hashMap k v) = T.adjust

(* * *)

val toList [k][v] (_:hashable k): (hashMap k v -> list (k * v)) = T.toList

val fromList [k][v] (_:hashable k) (_ : eq k): (list (k * v) -> hashMap k v) = T.fromList

(* * *)

val findMinByKey [k][v] (_:hashable k) (_: ord k): (hashMap k v -> option (k * v)) = T.findMinByKey

val findMaxByKey [k][v] (_:hashable k) (_: ord k): (hashMap k v -> option (k * v)) = T.findMaxByKey

val findMinByVal [k][v] (_:hashable k) (_: ord v): (hashMap k v -> option (k * v)) = T.findMinByVal

val findMaxByVal [k][v] (_:hashable k) (_: ord v): (hashMap k v -> option (k * v)) = T.findMaxByVal

(* * *)

val foldr [k][v][b] (_:hashable k): ((k * v -> b -> b) -> b -> hashMap k v -> b) = T.foldr

val filterFoldr [k][v][b] (_:hashable k): ((k * v -> bool) -> (k * v -> b -> b) -> b -> hashMap k v -> b) = T.filterFoldr

(* * *)

fun filter [k][v] (_:hashable k) (_: eq k) (prop: v -> bool) (d1: hashMap k v): hashMap k v =
      let fun prop' (p: k * v): bool = prop p.2
      in
         filterFoldr prop' (uncurry insert) empty d1
      end

fun filterWithKey [k][v] (_:hashable k) (_: eq k) (prop: k -> v -> bool) (d1: hashMap k v): hashMap k v =

      filterFoldr (uncurry prop) (uncurry insert) empty d1

fun partition [k][v] (_:hashable k) (_: eq k) (prop: v -> bool) (d1: hashMap k v) : hashMap k v * hashMap k v =
    let fun prop' (p: k * v): bool = prop p.2
        fun op (kv: k * v) (pt: hashMap k v * hashMap k v): hashMap k v * hashMap k v =
                          if prop' kv then (uncurry insert kv pt.1, pt.2)
                          else (pt.1, uncurry insert kv pt.2)
    in foldr op (empty, empty) d1
    end

fun partitionWithKey [k][v] (_:hashable k) (_: eq k) (prop: k -> v -> bool) (d1: hashMap k v) : hashMap k v * hashMap k v =
    let fun op (kv: k * v) (pt: hashMap k v * hashMap k v): hashMap k v * hashMap k v =
               if uncurry prop kv
                   then (uncurry insert kv pt.1, pt.2)
                   else (pt.1, uncurry insert kv pt.2)
    in foldr op (empty, empty) d1
    end

(* * *)

fun union [k][v] (_:hashable k) (_: eq k) (d1: hashMap k v) (d2: hashMap k v): hashMap k v = foldr (uncurry insert) d2 d1

fun unionWith [k][v] (_:hashable k) (_: eq k) (f: v -> v -> v) (d1: hashMap k v) (d2: hashMap k v): hashMap k v =

      foldr (uncurry (insertWith f)) d2 d1

fun diff [k][v] (_:hashable k) (_: eq k) (d1: hashMap k v) (d2: hashMap k v): hashMap k v = foldr (compose delete fst) d1 d2

fun deleteAll [k][v] (_:hashable k) (_: eq k) (ks: list k) (d1: hashMap k v): hashMap k v =

      List.foldl delete d1 ks

(* * *)

fun keys [k][v] (_:hashable k) (d1: hashMap k v): list k = List.mp fst (toList d1)

val values [k][v] (_:hashable k) (d1: hashMap k v): list v = List.mp snd (toList d1)

val valid[k][v] (_:hashable k) (_:eq k): (hashMap k v -> bool) = T.valid

(* * *)

fun all [k][v] (_:hashable k) (prop: v -> bool) (t: hashMap k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b && prop pair.2
    in
      foldr myop True t
    end

fun any [k][v] (_:hashable k) (prop: v -> bool) (t: hashMap k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b || prop pair.2
    in
      foldr myop False t
    end

fun allWithKey [k][v] (_:hashable k) (prop: k -> v -> bool) (t: hashMap k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b && uncurry prop pair
    in
      foldr myop True t
    end

fun anyWithKey [k][v] (_:hashable k) (prop: k -> v -> bool) (t: hashMap k v): bool =
    let
        fun myop (pair: k * v) (b: bool): bool = b || uncurry prop pair
    in
      foldr myop False t
    end

(* * *)

fun sumBy [k][v][b] (_:hashable k) (_:num b) (proj: v -> b) (d1: hashMap k v): b =
    let
        fun myop (pair: k * v) (acc: b): b = acc + proj pair.2
    in
      foldr myop zero d1
    end

fun intProdBy [k][v] (_:hashable k) (proj: v -> int) (d1: hashMap k v): int =
    let
        fun myop (pair: k * v) (acc: int): int = acc * proj pair.2
    in
      foldr myop 1 d1
    end

fun floatProdBy [k][v] (_:hashable k) (proj: v -> float) (d1: hashMap k v): float =
    let
        fun myop (pair: k * v) (acc: float): float = acc * proj pair.2
    in
      foldr myop 1.0 d1
    end
