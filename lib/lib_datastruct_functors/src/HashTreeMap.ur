(* HashEqTreeMap *)

signature BUCKET_MAP = Common.DSMAP

signature HASHTREE_MAP = sig
  include Common.DSMAP
  val maxBucketSize: item ::: Type -> t item -> int
end

structure HC = Hashable.HashableClass
(* open Hashable *)

(* internal *)
functor MkHashTreeMap(P:sig
                      structure Q: sig
                         con key :: Type
                         val hashable_key: HC.hashable key
                      end
                      structure BktMap: (BUCKET_MAP where con key = Q.key)
end): (HASHTREE_MAP where con key = P.Q.key) = struct

structure Bkt = P.BktMap

open HTuple
open HFunction
open HOrd
open Option

open P.Q

type bucket v = Bkt.t v

structure T = AATreeMap.MkAATreeMap(struct
                           type key = int
                           val ord_key = ord_int
                           val eq_key = eq_int
                        end) 

type t v = T.t (bucket v)

val empty [item]: t item = T.empty

fun singleton [item] (k1: key) (v1: item) :t item = T.singleton (HC.hash k1) (Bkt.singleton k1 v1)

val null [item] : (t item -> bool) = T.null

val size [item] (d1:t item) : int =
    let T.foldr (snd >>> myop) 0 d1
    where
      fun myop (bkt: bucket item) (acc: int):int = acc + Bkt.size bkt
    end

fun insertWith [item] (f: item -> item -> item) (k1: key) (v1: item) (d1: t item): t item =
     let
         T.insertWith bucket_insertWith (HC.hash k1) (Bkt.singleton k1 v1) d1
     where
        fun bucket_insertWith (_: bucket item) (bkt: bucket item): bucket item = Bkt.insertWith f k1 v1 bkt
     end

val insert [item]: (key -> item -> t item -> t item) = insertWith const

fun liftBucketToOption [item] (bkt: bucket item) = if Bkt.null bkt then None else Some bkt

(* liftBucketToOption used with T.update ensures that
  the hashTree node will be deleted if bucket is emptied *)

fun delete [item] (k1: key) (d1: t item): t item =
     let
        T.update (bucket_delete >>> liftBucketToOption) (HC.hash k1) d1
     where
        val bucket_delete: bucket item -> bucket item = Bkt.delete k1
     end

fun adjust [item] (f: item -> item) (k1: key) (d1: t item): t item =
     let 
        T.adjust bucket_adjust (HC.hash k1) d1
     where
        val bucket_adjust: bucket item -> bucket item = Bkt.adjust f k1
     end

fun update [item] (f: item -> option item) (k1: key) (d1: t item): t item =
     let
        T.update (bucket_update >>> liftBucketToOption) (HC.hash k1) d1
     where
        val bucket_update: bucket item -> bucket item = Bkt.update f k1
     end


fun mapValues [item] [b] (f: item -> b) (t1: t item): t b =
    let
        T.mapValues bucket_mapValues t1
    where
        val bucket_mapValues: bucket item -> bucket b = Bkt.mapValues f
    end

fun lookup [item] (k1: key) (d1: t item): option item =

    bkt <- T.lookup (HC.hash k1) d1 ; Bkt.lookup k1 bkt

val member [item] (k1: key): (t item -> bool) = lookup k1 >>> isSome

fun fromList [item] (li: list (key * item)): t item =
     List.foldl (uncurry insert) empty li

fun foldr [item] [b] (myop: key * item -> b -> b) (z: b) (d1: t item): b =
     let 
        T.foldr (snd >>> bucket_foldr) z d1
     where
        fun bucket_foldr (bkt: bucket item) (acc: b): b = Bkt.foldr myop acc bkt
     end

fun toList [item] (d1: t item): list (key * item) =

        T.foldr (snd >>> Bkt.toList >>> List.revAppend) [] d1


val getAnyPair [item] (t1: t item) : option (key * item) =

       (_, bkt) <- T.getAnyPair t1 ; Bkt.getAnyPair bkt

(* short-circuiting exists *)
fun exists [item] (prop: key * item -> bool) (t1: t item): bool =
    T.exists (snd >>> Bkt.exists prop) t1

(* short-circuiting all *)
fun all [item] (prop: key * item -> bool) (t1: t item): bool =
    T.all (snd >>> Bkt.all prop) t1

fun find [item] (prop: key * item -> bool) (t1: t item): option (key * item) =

    (_, bkt) <- T.find (snd >>> Bkt.find prop >>> isSome) t1 ; Bkt.find prop bkt 
  
(* * Invariants *)

val propBucketsAllValidAndNonEmpty [item] (t1: t item): bool =
    let T.all (snd >>> prop) t1
    where
      fun prop (bkt: bucket item) = (not <<< Bkt.null) bkt && Bkt.valid bkt
    end

val valid [item]: (t item -> bool) = propBucketsAllValidAndNonEmpty

(* * statistics *)

fun maxBucketSize [item] (t1:t item): int =
        let T.foldr (snd >>> myop) 0 t1
        where
          fun myop (bkt: bucket item) (acc: int): int = max acc (Bkt.size bkt)
        end
   
end


functor MkHashEqTreeMap(S: sig
                         con key :: Type
                         val hashable_key: HC.hashable key
                         val eq_key: eq key
                      end) = MkHashTreeMap(struct
                                        structure Q = S
                                        structure BktMap = ListMap.MkListMap( Q)
                                       end)

functor MkHashOrdTreeMap(S: sig
                         con key :: Type
                         val hashable_key: HC.hashable key
                         val ord_key: ord key
                      end) = MkHashTreeMap(struct
                                        structure Q = S
                                        structure BktMap = AATreeMap.MkAATreeMap( Q)
                                       end)
