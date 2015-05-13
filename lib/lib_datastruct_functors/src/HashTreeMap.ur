(* HashEqTreeMap *)

open Hashable.Hashable
open Hashable

(* internal *)
functor MkHashTreeMap(P:sig
                      structure Q: sig
                         con key :: Type
                         con item :: Type
                         val hashable_key: hashable key
                      end
                      structure BktMap: sig
                        con t :: Type -> Type -> Type

                        val empty : t Q.key Q.item

                        val null : t Q.key Q.item -> bool

                        val singleton : Q.key -> Q.item -> t Q.key Q.item

                        val size : t Q.key Q.item -> int

                        val insert : Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

                        val insertWith : (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

                        val adjust: (Q.item -> Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

                        val update: (Q.item -> option Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

                        val delete : Q.key -> t Q.key Q.item -> t Q.key Q.item

                        val lookup : Q.key -> t Q.key Q.item -> option Q.item

                        val member : Q.key -> t Q.key Q.item -> bool

                        val foldr : b ::: Type -> (Q.key * Q.item -> b -> b) -> b -> t Q.key Q.item -> b

                        val getAnyPair : t Q.key Q.item -> option (Q.key * Q.item)

                        val mapValues : item' ::: Type -> (Q.item -> item') -> t Q.key Q.item -> t Q.key item'

                        val exists : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

                        val all : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

                        val find : (Q.key * Q.item -> bool) -> t Q.key Q.item -> option (Q.key * Q.item)

                        val valid : t Q.key Q.item -> bool
                      end
end): sig

        con t :: Type -> Type -> Type

        val empty :  t P.Q.key P.Q.item

        val singleton :  P.Q.key -> P.Q.item -> t P.Q.key P.Q.item

        val null :  t P.Q.key P.Q.item -> bool

        val size :  t P.Q.key P.Q.item -> int

        val lookup:  P.Q.key -> t P.Q.key P.Q.item -> option P.Q.item

        val member:  P.Q.key -> t P.Q.key P.Q.item -> bool

        val getAnyPair :  t P.Q.key P.Q.item -> option (P.Q.key * P.Q.item)

        val insert:  P.Q.key -> P.Q.item -> t P.Q.key P.Q.item -> t P.Q.key P.Q.item

        val insertWith:  (P.Q.item -> P.Q.item -> P.Q.item) -> P.Q.key -> P.Q.item -> t P.Q.key P.Q.item -> t P.Q.key P.Q.item

        val fromList :  list (P.Q.key * P.Q.item) -> t P.Q.key P.Q.item

        val delete:  P.Q.key -> t P.Q.key P.Q.item -> t P.Q.key P.Q.item

        val adjust:  (P.Q.item -> P.Q.item) -> P.Q.key -> t P.Q.key P.Q.item -> t P.Q.key P.Q.item

        val update:  (P.Q.item -> option P.Q.item) -> P.Q.key -> t P.Q.key P.Q.item -> t P.Q.key P.Q.item

        val mapValues : b ::: Type -> (P.Q.item -> b) -> t P.Q.key P.Q.item -> t P.Q.key b

        val foldr:  b ::: Type -> (P.Q.key * P.Q.item -> b -> b) -> b -> t P.Q.key P.Q.item -> b

        val toList :  t P.Q.key P.Q.item -> list (P.Q.key * P.Q.item)

        val exists : (P.Q.key * P.Q.item -> bool) -> t P.Q.key P.Q.item -> bool

        val all : (P.Q.key * P.Q.item -> bool) -> t P.Q.key P.Q.item -> bool

        val find : (P.Q.key * P.Q.item -> bool) -> t P.Q.key P.Q.item -> option (P.Q.key * P.Q.item)

        val valid :  t P.Q.key P.Q.item -> bool

        val maxBucketSize: t P.Q.key P.Q.item -> int

end = struct


open HTuple
open HFunction
open HOrd
open Option

open P.Q
structure B = P.BktMap

type bucket k v = B.t k v

structure T = AATreeMap.MkAATreeMap(struct
                           type item = bucket key item
                           type key = int
                           val ord_key = ord_int 
                        end) 

type t k v = T.t int (bucket k v)

val empty: t key item = T.empty

fun singleton (k1: key) (v1: item) :t key item = T.singleton (hash k1) (B.singleton k1 v1)

val null : (t key item -> bool) = T.null

val size (d1:t key item) : int =
    let T.foldr myop 0 d1
    where
      fun myop (pair: int * bucket key item) (acc: int):int = acc + B.size pair.2
    end

fun insertWith (f: item -> item -> item) (k1: key) (v1: item) (d1: t key item): t key item =
     let val hk = hash k1
     in case T.lookup hk d1 of
          None => T.insert hk (B.singleton k1 v1) d1
          | Some _ => T.adjust (B.insertWith f k1 v1) hk d1
     end

val insert: (key -> item -> t key item -> t key item) = insertWith const

fun delete (k1: key) (d1: t key item): t key item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket key item): option (bucket key item) =
                let val bktNew = B.delete k1 bktActual
                in if B.null bktNew
                         then None
                         else Some bktNew
                end
     in T.update f' hk d1
     end

fun adjust (f: item -> item) (k1: key) (d1: t key item): t key item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket key item): bucket key item = B.adjust f k1 bktActual
     in T.adjust f' hk d1
     end

fun update (f: item -> option item) (k1: key) (d1: t key item): t key item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket key item): option (bucket key item) =
                let val bktNew = B.update f k1 bktActual
                in if B.null bktNew
                         then None
                         else Some bktNew
                end
     in T.update f' hk d1   
     end


fun mapValues [b] (f: item -> b) (t1: t key item): t key b =
     let val f' = B.mapValues f
     in T.mapValues f' t1
     end 

fun lookup (k1: key) (d1: t key item): option item =
     let val hk: int = hash k1
     in case T.lookup hk d1 of
          None => None
          | Some mybucket => B.lookup k1 mybucket
     end

val member (k1: key): (t key item -> bool) = lookup k1 >>> isSome

fun fromList (li: list (key * item)): t key item =
     List.foldl (uncurry insert) empty li

fun foldr [b] (myop: key * item -> b -> b) (z: b) (d1: t key item): b =
     let fun myop' (p: int * (bucket key item)) (acc: b): b = B.foldr myop acc p.2
     in T.foldr myop' z d1
     end

fun toList (d1: t key item): list (key * item) =
     let fun myop' (p: int * (bucket key item)) (acc: list (key * item)): list (key * item) = B.foldr (curry Cons) acc p.2
     in T.foldr myop' [] d1
     end

fun getAnyPair (d1: t key item): option (key * item) =
    let val optHPair: option (int * bucket key item) = T.getAnyPair d1
    in
       case optHPair of
         None => None
         | Some (_, mybucket) => B.getAnyPair mybucket
    end

(* short-circuiting exists *)
fun exists (prop: key * item -> bool) (t1: t key item): bool =
    let T.exists prop' t1
    where 
      fun prop' (p: int * bucket key item):bool = B.exists prop p.2
    end

(* short-circuiting all *)
fun all (prop: key * item -> bool) (t1: t key item): bool =
    let T.all prop' t1
    where
      fun prop' (p: int * bucket key item):bool = B.all prop p.2
    end

fun find (prop: key * item -> bool) (t1: t key item): option (key * item) =
    let case T.find prop' t1 of
         None => None
         | Some (_, bkt) => B.find prop bkt
    where
      fun prop' (p: int * bucket key item):bool = isSome (B.find prop p.2)
    end
  
(* * Invariants *)

val propBucketsAllValidAndNonEmpty (t1: t key item): bool =
    let T.foldr myop True t1
    where
      fun myop (p: int * bucket key item) (acc: bool): bool =
                  acc && not (B.null p.2) && B.valid p.2
    end

val valid: (t key item -> bool) = propBucketsAllValidAndNonEmpty

(* * statistics *)

fun maxBucketSize (t1:t key item): int =
        let T.foldr myop 0 t1
        where
          fun myop (p: int * (bucket key item)) (acc: int): int = max acc (B.size p.2)
        end
   
end


functor MkHashEqTreeMap(S: sig
                         con key :: Type
                         con item :: Type
                         val hashable_key: hashable key
                         val eq_key: eq key
                      end) = MkHashTreeMap(struct
                                        structure Q = S
                                        structure BktMap = ListMap.MkListMap( Q)
                                       end)

functor MkHashOrdTreeMap(S: sig
                         con key :: Type
                         con item :: Type
                         val hashable_key: hashable key
                         val ord_key: ord key
                      end) = MkHashTreeMap(struct
                                        structure Q = S
                                        structure BktMap = AATreeMap.MkAATreeMap( Q)
                                       end)
