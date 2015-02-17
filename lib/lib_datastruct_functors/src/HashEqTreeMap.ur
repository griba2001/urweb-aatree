(* HashEqTreeMap *)

open Hashable.Hashable
open Hashable


functor MkHashEqTreeMap(Q: sig
                         con key :: Type
                         con item :: Type
                         val eq_key: eq key
                         val hashable_key: hashable key 
end):sig

con htree :: Type -> Type -> Type

val empty :  htree Q.key Q.item

val singleton :  Q.key -> Q.item -> htree Q.key Q.item

val null :  htree Q.key Q.item -> bool

val size :  htree Q.key Q.item -> int

val lookup:  Q.key -> htree Q.key Q.item -> option Q.item

val member:  Q.key -> htree Q.key Q.item -> bool

val getAnyPair :  htree Q.key Q.item -> option (Q.key * Q.item)

val insert:  Q.key -> Q.item -> htree Q.key Q.item -> htree Q.key Q.item

val insertWith:  (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> htree Q.key Q.item -> htree Q.key Q.item

val fromList :  list (Q.key * Q.item) -> htree Q.key Q.item

val delete:  Q.key -> htree Q.key Q.item -> htree Q.key Q.item

val adjust:  (Q.item -> Q.item) -> Q.key -> htree Q.key Q.item -> htree Q.key Q.item

val update:  (Q.item -> option Q.item) -> Q.key -> htree Q.key Q.item -> htree Q.key Q.item

val mapValues : b ::: Type -> (Q.item -> b) -> htree Q.key Q.item -> htree Q.key b

val foldr:  b ::: Type -> (Q.key * Q.item -> b -> b) -> b -> htree Q.key Q.item -> b

val toList :  htree Q.key Q.item -> list (Q.key * Q.item)

val exists : (Q.key * Q.item -> bool) -> htree Q.key Q.item -> bool

val all : (Q.key * Q.item -> bool) -> htree Q.key Q.item -> bool

val find : (Q.key * Q.item -> bool) -> htree Q.key Q.item -> option (Q.key * Q.item)

(* * Invariants *)

val valid :  htree Q.key Q.item -> bool

(* stats *)
val maxBucketSize: htree Q.key Q.item -> int

end = struct



open HTuple
open HFunction
open HOrd
open Option

open Q

structure LB = ListMap.MkListMap( Q)

type bucket k v = LB.t k v

structure T = AATreeMap.MkAATreeMap(struct
                           type item = bucket Q.key Q.item
                           type key = int
                           val ord_key = ord_int 
                        end) 

type htree k v = T.tree int (bucket k v)

val empty: htree key item = T.empty

fun singleton (k1: key) (v1: item) :htree key item = T.singleton (hash k1) (LB.singleton k1 v1)

val null : (htree key item -> bool) = T.null

val size (d1:htree key item) : int =
    let fun myop (pair: int * bucket key item) (acc: int):int = acc + LB.size pair.2
    in T.foldr myop 0 d1
    end

fun insertWith (f: item -> item -> item) (k1: key) (v1: item) (d1: htree key item): htree key item =
     let val hk = hash k1
     in case T.lookup hk d1 of
          None => T.insert hk (LB.singleton k1 v1) d1
          | Some _ => T.adjust (LB.insertWith f k1 v1) hk d1
     end

val insert: (key -> item -> htree key item -> htree key item) = insertWith const

fun delete (k1: key) (d1: htree key item): htree key item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket key item): option (bucket key item) =
                let val bktNew = LB.delete k1 bktActual
                in if LB.null bktNew
                         then None
                         else Some bktNew
                end
     in T.update f' hk d1
     end

fun adjust (f: item -> item) (k1: key) (d1: htree key item): htree key item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket key item): bucket key item = LB.adjust f k1 bktActual
     in T.adjust f' hk d1
     end

fun update (f: item -> option item) (k1: key) (d1: htree key item): htree key item =
     let val hk: int = hash k1
         fun f' (bktActual: bucket key item): option (bucket key item) =
                let val bktNew = LB.update f k1 bktActual
                in if LB.null bktNew
                         then None
                         else Some bktNew
                end
     in T.update f' hk d1   
     end


fun mapValues [b] (f: Q.item -> b) (t1: htree Q.key Q.item): htree Q.key b =
     let val f' = LB.mapValues f
     in T.mapValues f' t1
     end 

fun lookup (k1: key) (d1: htree key item): option item =
     let val hk: int = hash k1
     in case T.lookup hk d1 of
          None => None
          | Some mybucket => LB.lookup k1 mybucket
     end

val member (k1: key): (htree key item -> bool) = compose isSome (lookup k1)

fun fromList (li: list (key * item)): htree key item =
     List.foldl (uncurry insert) empty li

fun foldr [b] (myop: key * item -> b -> b) (z: b) (d1: htree key item): b =
     let fun myop' (p: int * (bucket key item)) (acc: b): b = LB.foldr myop acc p.2
     in T.foldr myop' z d1
     end

fun toList (d1: htree key item): list (key * item) =
     let fun myop' (p: int * (bucket key item)) (acc: list (key * item)): list (key * item) = LB.foldr (curry Cons) acc p.2
     in T.foldr myop' [] d1
     end

fun getAnyPair (d1: htree key item): option (key * item) =
    let val optHPair: option (int * bucket key item) = T.getAnyPair d1
    in
       case optHPair of
         None => None
         | Some (_, mybucket) => LB.getAnyPair mybucket
    end

(* short-circuiting exists *)
fun exists (prop: key * item -> bool) (t1: htree key item): bool =
    let fun prop' (p: int * bucket key item):bool = LB.exists prop p.2
    in T.exists prop' t1
    end

(* short-circuiting all *)
fun all (prop: key * item -> bool) (t1: htree key item): bool =
    let fun prop' (p: int * bucket key item):bool = LB.all prop p.2
    in T.all prop' t1
    end

fun find (prop: key * item -> bool) (t1: htree key item): option (key * item) =
    let fun prop' (p: int * bucket key item):bool = isSome (LB.find prop p.2)
    in case T.find prop' t1 of
         None => None
         | Some (_, bkt) => LB.find prop bkt
    end
  
(* * Invariants *)

val propBucketsAllValidAndNonEmpty (t1: htree key item): bool =
    let fun myop (p: int * bucket key item) (acc: bool): bool =
                  acc && not (LB.null p.2) && LB.valid p.2
    in T.foldr myop True t1
    end

val valid: (htree key item -> bool) = propBucketsAllValidAndNonEmpty

(* * statistics *)

fun maxBucketSize (t1:htree key item): int =
        let fun myop (p: int * (bucket key item)) (acc: int): int = max acc (LB.size p.2)
        in T.foldr myop 0 t1
        end
   
end
