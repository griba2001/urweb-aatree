(* HashEqTreeMap *)

open Hashable.Hashable
open Hashable

(* internal *)
functor MkHashTreeMap(P:sig
                      structure Q: sig
                         con key :: Type
                         val hashable_key: hashable key
                      end
                      structure BktMap: sig
                        con t :: Type -> Type

                        val empty : item ::: Type -> t item

                        val null : item ::: Type -> t item -> bool

                        val singleton : item ::: Type -> Q.key -> item -> t item

                        val size : item ::: Type -> t item -> int

                        val insert : item ::: Type -> Q.key -> item -> t item -> t item

                        val insertWith : item ::: Type -> (item -> item -> item) -> Q.key -> item -> t item -> t item

                        val adjust: item ::: Type -> (item -> item) -> Q.key -> t item -> t item

                        val update: item ::: Type -> (item -> option item) -> Q.key -> t item -> t item

                        val delete: item ::: Type -> Q.key -> t item -> t item

                        val lookup: item ::: Type -> Q.key -> t item -> option item

                        val member : item ::: Type -> Q.key -> t item -> bool

                        val foldr : item ::: Type -> b ::: Type -> (Q.key * item -> b -> b) -> b -> t item -> b

                        val getAnyPair : item ::: Type -> t item -> option (Q.key * item)

                        val mapValues : item ::: Type -> item' ::: Type -> (item -> item') -> t item -> t item'

                        val exists : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

                        val all : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

                        val find : item ::: Type -> (Q.key * item -> bool) -> t item -> option (Q.key * item)

                        val valid : item ::: Type -> t item -> bool
                      end
end): sig

        con t :: Type -> Type

        val empty : item ::: Type -> t item

        val singleton : item ::: Type -> P.Q.key -> item -> t item

        val null : item ::: Type -> t item -> bool

        val size : item ::: Type -> t item -> int

        val lookup: item ::: Type -> P.Q.key -> t item -> option item

        val member: item ::: Type -> P.Q.key -> t item -> bool

        val getAnyPair: item ::: Type -> t item -> option (P.Q.key * item)

        val insert: item ::: Type -> P.Q.key -> item -> t item -> t item

        val insertWith: item ::: Type -> (item -> item -> item) -> P.Q.key -> item -> t item -> t item

        val fromList: item ::: Type -> list (P.Q.key * item) -> t item

        val delete: item ::: Type -> P.Q.key -> t item -> t item

        val adjust: item ::: Type -> (item -> item) -> P.Q.key -> t item -> t item

        val update: item ::: Type -> (item -> option item) -> P.Q.key -> t item -> t item

        val mapValues : item ::: Type -> b ::: Type -> (item -> b) -> t item -> t b

        val foldr: item ::: Type -> b ::: Type -> (P.Q.key * item -> b -> b) -> b -> t item -> b

        val toList : item ::: Type -> t item -> list (P.Q.key * item)

        val exists : item ::: Type -> (P.Q.key * item -> bool) -> t item -> bool

        val all : item ::: Type -> (P.Q.key * item -> bool) -> t item -> bool

        val find : item ::: Type -> (P.Q.key * item -> bool) -> t item -> option (P.Q.key * item)

        val valid : item ::: Type ->  t item -> bool

        val maxBucketSize: item ::: Type -> t item -> int

end = struct

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
         case T.lookup hk d1 of
          | None => T.insert hk (B.singleton k1 v1) d1
          | Some _ => T.adjust bucket_insertWith hk d1
     where
        val hk = hash k1
        val bucket_insertWith: bucket item -> bucket item = B.insertWith f k1 v1
     end

val insert [item]: (key -> item -> t item -> t item) = insertWith const

fun liftBucketToOption [item] (bkt: bucket item) = if B.null bkt then None else Some bkt

fun delete [item] (k1: key) (d1: t item): t item =
     let
        T.update bucket_delete (hash k1) d1
     where
        val bucket_delete: bucket item -> option (bucket item) = B.delete k1 >>> liftBucketToOption
     end

fun adjust [item] (f: item -> item) (k1: key) (d1: t item): t item =
     let 
        T.adjust bucket_adjust (hash k1) d1
     where
        val bucket_adjust: bucket item -> bucket item = B.adjust f k1
     end

fun update [item] (f: item -> option item) (k1: key) (d1: t item): t item =
     let
        T.update bucket_update (hash k1) d1
     where
        val bucket_update: bucket item -> option (bucket item) = B.update f k1 >>> liftBucketToOption
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
