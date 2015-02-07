(* UnordHashSet *)

structure T = UnordHashTree
structure O = Option
structure M = Monad
(* structure HL = HList *)
open HTuple
open HFunction

open Hashable.Hashable

type hashSet a = T.hashTree a unit

(* * Construction *)

val empty [a] (_:hashable a): hashSet a = T.empty

val singleton [a] (_:hashable a) (v: a): hashSet a = T.singleton v ()

val fromList [a] (_:hashable a) (_: eq a): (list a -> hashSet a) =
        let val f = fn (v: a) => (v, ())
        in compose T.fromList (List.mp f)
        end

(* * Query *)

val null [a] (_:hashable a) : hashSet a -> bool = T.null

val size [a] (_:hashable a): (hashSet a -> int) = T.size

val member [a] (_:hashable a) (_: eq a) (x: a): (hashSet a -> bool) = compose O.isSome (T.lookup x)

val findMin [a] (_:hashable a) (_: ord a) : (hashSet a -> option a) = compose (M.liftM fst) T.findMinByKey

val findMax [a] (_:hashable a) (_: ord a) : (hashSet a -> option a) = compose (M.liftM fst) T.findMaxByKey

(* * Mutation *)

val insert [a] (_:hashable a) (_ : eq a) (v: a): (hashSet a -> hashSet a) = T.insert v ()

val delete [a] (_:hashable a) (_ : eq a) (v: a): (hashSet a -> hashSet a) = T.delete v

(* * Foldings *)

fun foldr [a][b] (_:hashable a) (myop: a -> b -> b) (acc: b) (t: hashSet a): b =
    let
        fun myop' (p: a * unit): b -> b = myop p.1
    in T.foldr myop' acc t
    end

fun filterFoldr [a][b] (_:hashable a) (prop: a -> bool) (myop: a -> b -> b) (acc: b) : (hashSet a -> b) =
    let fun myop' (x: a) (acc': b) =
             if prop x
                then myop x acc'
                else acc'
    in foldr myop' acc
    end

val toList [a] (_:hashable a): (hashSet a -> list a) = compose (List.mp fst) T.toList

(* * Partition *)

fun filter [a] (_:hashable a) (_ : eq a) (prop: a -> bool) : (hashSet a -> hashSet a) =

      filterFoldr prop insert empty

fun partition [a] (_:hashable a) (_ : eq a) (prop: a -> bool) : (hashSet a -> hashSet a * hashSet a) =
    let
        fun myop (x: a) (pair: hashSet a * hashSet a): hashSet a * hashSet a =
                          if prop x then (insert x pair.1, pair.2)
                          else (pair.1, insert x pair.2)
    in
        foldr myop (empty, empty)
    end

(* * Set ops *)

fun union [a] (_:hashable a) (_ : eq a) (s1: hashSet a) (s2: hashSet a): hashSet a = foldr insert s2 s1

fun diff [a] (_:hashable a) (_ : eq a) (s1: hashSet a) (s2: hashSet a): hashSet a = foldr delete s1 s2

fun intersect [a] (_:hashable a) (_ : eq a) (s1: hashSet a) (s2: hashSet a): hashSet a =
   let
      val memberOf = flip member
   in
      filterFoldr (memberOf s1) insert empty s2
   end

(* * Mappings *)

fun mp [a][b] (_:hashable a) (_:hashable b) (_: eq b) (f: a -> b): hashSet a -> hashSet b =

    andThen toList (andThen (List.mp f) fromList)   (* left to right function composition *)