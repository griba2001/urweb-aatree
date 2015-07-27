(* HashEqTreeMap *)

signature BUCKET_MAP = Common.DSMAP

signature HASHTREE_MAP = sig
  include Common.DSMAP
  val maxBucketSize: item ::: Type -> t item -> int
end

open Hashable.Hashable
open Hashable

(* internal *)
functor MkHashTreeMap(P:sig
                      structure Q: sig
                         con key :: Type
                         val hashable_key: hashable key
                      end
                      structure BktMap: BUCKET_MAP where con key = Q.key
end): HASHTREE_MAP where con key = P.Q.key = struct

structure B = P.BktMap

open HTuple
open HFunction
open HOrd
open Option

open P.Q

type bucket v = B.t v

structure T = AATreeMap.MkAATreeMap(struct
                           type key = int
                           val ord_key = ord_int
                           val eq_key = eq_int
                        end) 

type t v = T.t (bucket v)

val empty [item]: t item = T.empty

fun singleton [item] (k1: key) (v1: item) :t item = T.singleton (hash k1) (B.singleton k1 v1)

val null [item] : (t item -> bool) = T.null

val size [item] (d1:t item) : int =
    let T.foldr (snd >>> myop) 0 d1
    where
      fun myop (bkt: bucket item) (acc: int):int = acc + B.size bkt
    end

fun insertWith [item] (f: item -> item -> item) (k1: key) (v1: item) (d1: t item): t item =
     let
         T.insertWith bucket_insertWith (hash k1) (B.singleton k1 v1) d1
     where
        fun bucket_insertWith (_: bucket item) (bkt: bucket item): bucket item = B.insertWith f k1 v1 bkt
     end

val insert [item]: (key -> item -> t item -> t item) = insertWith const

fun liftBucketToOption [item] (bkt: bucket item) = if B.null bkt then None else Some bkt

(* liftBucketToOption used with T.update ensures that
  the hashTree node will be deleted if bucket is emptied *)

fun delete [item] (k1: key) (d1: t item): t item =
     let
        T.update (bucket_delete >>> liftBucketToOption) (hash k1) d1
     where
        val bucket_delete: bucket item -> bucket item = B.delete k1
     end

fun adjust [item] (f: item -> item) (k1: key) (d1: t item): t item =
     let 
        T.adjust bucket_adjust (hash k1) d1
     where
        val bucket_adjust: bucket item -> bucket item = B.adjust f k1
     end

fun update [item] (f: item -> option item) (k1: key) (d1: t item): t item =
     let
        T.update (bucket_update >>> liftBucketToOption) (hash k1) d1
     where
        val bucket_update: bucket item -> bucket item = B.update f k1
     end


fun mapValues [item] [b] (f: item -> b) (t1: t item): t b =
    let
        T.mapValues bucket_mapValues t1
    where
        val bucket_mapValues: bucket item -> bucket b = B.mapValues f
    end

fun lookup [item] (k1: key) (d1: t item): option item =

    bkt <- T.lookup (hash k1) d1 ; B.lookup k1 bkt

val member [item] (k1: key): (t item -> bool) = lookup k1 >>> isSome

fun fromList [item] (li: list (key * item)): t item =
     List.foldl (uncurry insert) empty li

fun foldr [item] [b] (myop: key * item -> b -> b) (z: b) (d1: t item): b =
     let 
        T.foldr (snd >>> myop') z d1
     where
        fun myop' (p: bucket item) (acc: b): b = B.foldr myop acc p
     end

fun toList [item] (d1: t item): list (key * item) =
     let
        T.foldr (snd >>> myop') [] d1
     where
        fun myop' (p: bucket item) (acc: list (key * item)): list (key * item) =
          B.foldr (curry Cons) acc p
     end

val getAnyPair [item] (t1: t item) : option (key * item) =

       (_, bkt) <- T.getAnyPair t1 ; B.getAnyPair bkt

(* short-circuiting exists *)
fun exists [item] (prop: key * item -> bool) (t1: t item): bool =
    T.exists (snd >>> B.exists prop) t1

(* short-circuiting all *)
fun all [item] (prop: key * item -> bool) (t1: t item): bool =
    T.all (snd >>> B.all prop) t1

fun find [item] (prop: key * item -> bool) (t1: t item): option (key * item) =

    (_, bkt) <- T.find (snd >>> B.find prop >>> isSome) t1 ; B.find prop bkt 
  
(* * Invariants *)

val propBucketsAllValidAndNonEmpty [item] (t1: t item): bool =
    let T.all (snd >>> prop) t1
    where
      fun prop (bkt: bucket item) = (not <<< B.null) bkt && B.valid bkt
    end

val valid [item]: (t item -> bool) = propBucketsAllValidAndNonEmpty

(* * statistics *)

fun maxBucketSize [item] (t1:t item): int =
        let T.foldr (snd >>> myop) 0 t1
        where
          fun myop (bkt: bucket item) (acc: int): int = max acc (B.size bkt)
        end
   
end


functor MkHashEqTreeMap(S: sig
                         con key :: Type
                         val hashable_key: hashable key
                         val eq_key: eq key
                      end) = MkHashTreeMap(struct
                                        structure Q = S
                                        structure BktMap = ListMap.MkListMap( Q)
                                       end)

functor MkHashOrdTreeMap(S: sig
                         con key :: Type
                         val hashable_key: hashable key
                         val ord_key: ord key
                      end) = MkHashTreeMap(struct
                                        structure Q = S
                                        structure BktMap = AATreeMap.MkAATreeMap( Q)
                                       end)
