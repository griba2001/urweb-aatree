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
    let T.foldr myop 0 d1
    where
      fun myop (pair: int * bucket item) (acc: int):int = acc + B.size pair.2
    end

fun insertWith [item] (f: item -> item -> item) (k1: key) (v1: item) (d1: t item): t item =
     let val hk = hash k1
     in case T.lookup hk d1 of
          None => T.insert hk (B.singleton k1 v1) d1
          | Some _ => T.adjust (B.insertWith f k1 v1) hk d1
     end

val insert [item]: (key -> item -> t item -> t item) = insertWith const

fun delete [item] (k1: key) (d1: t item): t item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket item): option (bucket item) =
                let val bktNew = B.delete k1 bktActual
                in if B.null bktNew
                         then None
                         else Some bktNew
                end
     in T.update f' hk d1
     end

fun adjust [item] (f: item -> item) (k1: key) (d1: t item): t item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket item): bucket item = B.adjust f k1 bktActual
     in T.adjust f' hk d1
     end

fun update [item] (f: item -> option item) (k1: key) (d1: t item): t item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket item): option (bucket item) =
                let val bktNew = B.update f k1 bktActual
                in if B.null bktNew
                         then None
                         else Some bktNew
                end
     in T.update f' hk d1   
     end


fun mapValues [item] [b] (f: item -> b) (t1: t item): t b =
     let val f' = B.mapValues f
     in T.mapValues f' t1
     end 

fun lookup [item] (k1: key) (d1: t item): option item =
     let val hk: int = hash k1
     in case T.lookup hk d1 of
          None => None
          | Some mybucket => B.lookup k1 mybucket
     end

val member [item] (k1: key): (t item -> bool) = lookup k1 >>> isSome

fun fromList [item] (li: list (key * item)): t item =
     List.foldl (uncurry insert) empty li

fun foldr [item] [b] (myop: key * item -> b -> b) (z: b) (d1: t item): b =
     let fun myop' (p: int * (bucket item)) (acc: b): b = B.foldr myop acc p.2
     in T.foldr myop' z d1
     end

fun toList [item] (d1: t item): list (key * item) =
     let fun myop' (p: int * (bucket item)) (acc: list (key * item)): list (key * item) = B.foldr (curry Cons) acc p.2
     in T.foldr myop' [] d1
     end

fun getAnyPair [item] (d1: t item): option (key * item) =
    let val optHPair: option (int * bucket item) = T.getAnyPair d1
    in
       case optHPair of
         None => None
         | Some (_, mybucket) => B.getAnyPair mybucket
    end

(* short-circuiting exists *)
fun exists [item] (prop: key * item -> bool) (t1: t item): bool =
    let T.exists prop' t1
    where 
      fun prop' (p: int * bucket item):bool = B.exists prop p.2
    end

(* short-circuiting all *)
fun all [item] (prop: key * item -> bool) (t1: t item): bool =
    let T.all prop' t1
    where
      fun prop' (p: int * bucket item):bool = B.all prop p.2
    end

fun find [item] (prop: key * item -> bool) (t1: t item): option (key * item) =
    let case T.find prop' t1 of
         None => None
         | Some (_, bkt) => B.find prop bkt
    where
      fun prop' (p: int * bucket item):bool = isSome (B.find prop p.2)
    end
  
(* * Invariants *)

val propBucketsAllValidAndNonEmpty [item] (t1: t item): bool =
    let T.foldr myop True t1
    where
      fun myop (p: int * bucket item) (acc: bool): bool =
                  acc && not (B.null p.2) && B.valid p.2
    end

val valid [item]: (t item -> bool) = propBucketsAllValidAndNonEmpty

(* * statistics *)

fun maxBucketSize [item] (t1:t item): int =
        let T.foldr myop 0 t1
        where
          fun myop (p: int * (bucket item)) (acc: int): int = max acc (B.size p.2)
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
