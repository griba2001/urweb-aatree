
structure O = Option
structure F = HFunction
structure HL = HList
structure HT = HTuple
open HFunction


con set a = AATree.tree a unit

(*
val eq_set [a] (_ : eq a) = AATree.eq_tree
*)

val empty [a]: set a = AATree.empty

val null [a]: set a -> bool = AATree.null

val singleton [a] (v: a): set a = AATree.singleton v ()

val insert [a] (_ : ord a) (v: a): (set a -> set a) = AATree.insert v ()

val delete [a] (_ : ord a) (v: a): (set a -> set a) = AATree.delete v

val member [a] (_ : ord a) (x: a): (set a -> bool) = F.compose O.isSome (AATree.lookup x)

val toList [a] : (set a -> list a) = F.compose (List.mp HT.fst) AATree.toList

val fromList [a] (_ : ord a): (list a -> set a) =
        let val f = fn (v: a) => (v, ())
        in F.compose AATree.fromList (List.mp f)
        end

fun filter [a] (_: ord a) (prop: a -> bool) : (set a -> set a) =
        F.compose fromList (F.compose (List.filter prop) toList)

fun partition [a] (_: ord a) (prop: a -> bool) (s1: set a): set a * set a =
         let
             val (pos, neg) = HL.partition prop (toList s1)
         in
            (fromList pos, fromList neg)
         end

fun union [a] (_: ord a) (s1: set a) (s2: set a): set a =

    List.foldl insert s2 (toList s1)  (* in collision s1 prevales *)

fun difference [a] (_: ord a) (s1: set a) (s2: set a): set a =

    List.foldl delete s1 (toList s2)  (* s1 - s2 *)

fun intersection [a] (_: ord a) (s1: set a) (s2: set a): set a =

    let val common = List.filter (flip member s1) (toList s2)
    in List.foldl insert empty common
    end