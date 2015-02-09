(* UnordHashTree *)

structure T = AATree
structure LB = ListBucket

open HTuple
open HFunction
open HOrd
open Option

open Hashable.Hashable
open Hashable

type bucket k v = LB.t k v

type hashTree k v = T.tree int (bucket k v)

val empty [k][v] (_:hashable k): hashTree k v = T.empty  (* (_:eq k) *)

fun singleton [k][v] (_:hashable k) (k1: k) (v1: v) :hashTree k v = T.singleton (hash k1) (LB.singleton k1 v1)

val null [k][v] (_:hashable k): (hashTree k v -> bool) = T.null

val size [k][v] (_:hashable k) (d1:hashTree k v) : int =
    let fun myop (pair: int * bucket k v) (acc: int):int = acc + LB.size pair.2
    in T.foldr myop 0 d1
    end

fun insertWith [k][v] (_:hashable k) (_:eq k) (f: v -> v -> v) (k1: k) (v1: v) (d1: hashTree k v): hashTree k v =
     let val hk = hash k1
     in case T.lookup hk d1 of
          None => T.insert hk (LB.singleton k1 v1) d1
          | Some _ => T.adjust (LB.insertWith f k1 v1) hk d1
     end

fun insert [k][v] (_:hashable k) (_: eq k): (k -> v -> hashTree k v -> hashTree k v) = insertWith const

fun delete [k][v] (_:hashable k) (_: eq k) (k1: k) (d1: hashTree k v): hashTree k v =
     let val hk: int = hash k1
     in case T.lookup hk d1 of
          None => d1
          | Some mybucket => (let val newBucket: bucket k v = LB.delete k1 mybucket
                            in if LB.null newBucket
                               then T.delete hk d1
                               else T.adjust (const newBucket) hk d1
                            end)
     end

fun adjust [k][v] (_:hashable k) (_: eq k) (f: v -> v) (k1: k) (d1: hashTree k v): hashTree k v =
     let val hk: int = hash k1
     in case T.lookup hk d1 of
          None => d1
          | Some mybucket => (let val newBucket: bucket k v = LB.adjust f k1 mybucket
                            in T.adjust (const newBucket) hk d1
                            end)
     end

fun lookup [k][v] (_:hashable k) (_: eq k) (k1: k) (d1: hashTree k v): option v =
     let val hk: int = hash k1
     in case T.lookup hk d1 of
          None => None
          | Some mybucket => LB.lookup k1 mybucket
     end

val member [k][v] (_:hashable k) (_: eq k) (k1: k): (hashTree k v -> bool) = compose isSome (lookup k1)

fun fromList [k][v] (_:hashable k) (_: eq k) (li: list (k * v)): hashTree k v =
     List.foldl (uncurry insert) empty li

fun foldr [k][v][b] (_:hashable k) (myop: k * v -> b -> b) (z: b) (d1: hashTree k v): b =
     let fun myop' (p: int * (bucket k v)) (acc: b): b = LB.foldr myop acc p.2
     in T.foldr myop' z d1
     end

fun toList [k][v] (_:hashable k) (d1: hashTree k v): list (k * v) =
     let fun myop' (p: int * (bucket k v)) (acc: list (k * v)): list (k * v) = LB.foldr (curry Cons) acc p.2
     in T.foldr myop' [] d1
     end

fun filterFoldr [k][v][b] (_:hashable k) (prop: k * v -> bool) (myop: k * v -> b -> b) (z: b) (d1: hashTree k v): b =
    let fun myop' (p: k * v) (acc: b): b =
            if prop p then myop p acc else acc
    in foldr myop' z d1
    end 

fun getAnyPair [k][v] (_:hashable k) (d1: hashTree k v): option (k * v) =
    let val optHPair: option (int * bucket k v) = T.getAnyPair d1
    in
       case optHPair of
         None => None
         | Some (_, mybucket) => LB.getAnyPair mybucket
    end   

fun findByOrd [k][v][b] (_:hashable k) (_: ord b) (proj: k * v -> b) (f: b -> b -> b) (d1: hashTree k v): option (k * v) =
   let val optZ : option (k * v) = getAnyPair d1

       fun myop (pair: k * v) (acc: k * v): k * v =
            let val acc_p = proj acc
                val pair_p = proj pair
            in if compare (f acc_p pair_p) acc_p = EQ then acc else pair
            end
   in
       case optZ of
         None => None
         | Some z => Some (foldr myop z d1)
   end

val findMinByKey [k][v] (_:hashable k) (_: ord k): (hashTree k v -> option (k * v)) = findByOrd fst min

val findMaxByKey [k][v] (_:hashable k) (_: ord k): (hashTree k v -> option (k * v)) = findByOrd fst max

val findMinByVal [k][v] (_:hashable k) (_: ord v): (hashTree k v -> option (k * v)) = findByOrd snd min

val findMaxByVal [k][v] (_:hashable k) (_: ord v): (hashTree k v -> option (k * v)) = findByOrd snd max

(* * Invariants *)

val propBucketsAllValidAndNonEmpty[k][v] (_:hashable k) (_:eq k) (t1: hashTree k v): bool =
    let fun myop (p: int * bucket k v) (acc: bool): bool =
                  acc && not (LB.null p.2) && LB.valid p.2
    in T.foldr myop True t1
    end

val valid[k][v] (_:hashable k) (_:eq k): (hashTree k v -> bool) = propBucketsAllValidAndNonEmpty

(* * statistics *)

fun maxBucketSize[k][v] (_:hashable k) (_:eq k) (t1:hashTree k v): int =
        let fun myop (p: int * (bucket k v)) (acc: int): int = max acc (LB.size p.2)
        in T.foldr myop 0 t1
        end
   

