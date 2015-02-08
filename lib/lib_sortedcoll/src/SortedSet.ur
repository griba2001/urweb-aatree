(* SortedSet *)

structure T = AATree
structure O = Option
structure M = Monad
structure HL = HList
open HTuple
open HFunction


type sset a = T.tree a unit

(* * Instances *)

val eq_unit: eq (unit) = let
                 fun eq' (x: unit) (y: unit) = True
        in
            mkEq eq'
        end
             

val eq_set = fn [a] (_ : eq a) =>
        let
            fun eq' (t1: sset a) (t2: sset a) =
                   let val t1' : T.tree a unit = t1
                       val t2' : T.tree a unit = t2
                   in t1' = t2'
                   end 
        in
            mkEq eq'
        end

(* * Construction *)

val empty [a]: sset a = T.empty

val singleton [a] (v: a): sset a = T.singleton v ()

(* * Query *)

val null [a]: sset a -> bool = T.null

val size [a] : (sset a -> int) = T.size

val member [a] (_ : ord a) (x: a): (sset a -> bool) = compose O.isSome (T.lookup x)

val findMin [a] : (sset a -> option a) = compose (M.liftM fst) T.findMin

val findMax [a] : (sset a -> option a) = compose (M.liftM fst) T.findMax

(* * Insert / delete *)

val insert [a] (_ : ord a) (v: a): (sset a -> sset a) = T.insert v ()

val delete [a] (_ : ord a) (v: a): (sset a -> sset a) = T.delete v

val fromList [a] (_ : ord a): (list a -> sset a) =
        let val f = fn (v: a) => (v, ())
        in compose T.fromList (List.mp f)
        end

(* * Foldings *)

fun foldr [a][b] (myop: a -> b -> b) (acc: b) (t: sset a): b =
    let
        fun myop' (p: a * unit): b -> b = myop p.1
    in T.foldr myop' acc t
    end

fun filterFoldr [a][b] (prop: a -> bool) (myop: a -> b -> b) (acc: b) : (sset a -> b) =
    let fun myop' (x: a) (acc': b) =
             if prop x
                then myop x acc'
                else acc'
    in foldr myop' acc
    end

val toList [a] : (sset a -> list a) = compose (List.mp fst) T.toList

val show_set = fn [a] (_ : show a) =>
        let
            fun show' (t1: sset a)  = "sset fromList: " ^ show (toList t1)
        in
            mkShow show'
        end

fun filter [a] (_: ord a) (prop: a -> bool) : (sset a -> sset a) =

      filterFoldr prop insert empty

fun partition [a] (_: ord a) (prop: a -> bool) : (sset a -> sset a * sset a) =
    let
        fun myop (x: a) (pair: sset a * sset a): sset a * sset a =
                          if prop x then (insert x pair.1, pair.2)
                          else (pair.1, insert x pair.2)
    in
        foldr myop (empty, empty)
    end

fun union [a] (_: ord a) (s1: sset a) (s2: sset a): sset a = foldr insert s2 s1

fun diff [a] (_: ord a) (s1: sset a) (s2: sset a): sset a = foldr delete s1 s2

fun intersect [a] (_: ord a) (s1: sset a) (s2: sset a): sset a =
   let
      val memberOf = flip member
   in
      filterFoldr (memberOf s1) insert empty s2
   end

(* * Mappings *)

fun mp [a][b] (_: ord b) (f: a -> b): sset a -> sset b =

    andThen toList (andThen (List.mp f) fromList)   (* left to right function composition *)

val mapMonotonic [a][b]: (a -> b) -> sset a -> sset b = T.mapKeysMonotonic

val valid [a] (_: ord a): (sset a -> bool) = T.valid

fun all [a]  (prop: a -> bool) (t1: sset a): bool =
    let
        fun myop (x: a) (b: bool): bool = b && prop x
    in
      foldr myop True t1
    end

fun any [a]  (prop: a -> bool) (t1: sset a): bool =
    let
        fun myop (x: a) (b: bool): bool = b || prop x
    in
      foldr myop False t1
    end

fun sumBy [a][b]  (_:num b) (proj: a -> b) (t1: sset a): b =
    let
        fun myop (x: a) (acc: b): b = acc + proj x
    in
      foldr myop zero t1
    end

(* prod cannot be specified in terms of num,
   because num lacks the product neutral elem. definition
   (zero is defined in class num but not one)
*)
fun intProdBy [a]  (proj: a -> int) (t1: sset a): int =
    let
        fun myop (x: a) (acc: int): int = acc * proj x
    in
      foldr myop 1 t1
    end

fun floatProdBy [a]  (proj: a -> float) (t1: sset a): float =
    let
        fun myop (x: a) (acc: float): float = acc * proj x
    in
      foldr myop 1.0 t1
    end

fun minBy [a][b]  (_:ord b) (proj: a -> b) (z: b) (t1: sset a): b =
    let
        fun myop (x: a) (acc: b): b = min acc (proj x)
    in
        foldr myop z t1
    end

fun maxBy [a][b]  (_:ord b) (proj: a -> b) (z: b) (t1: sset a): b =
    let
        fun myop (x: a) (acc: b): b = max acc (proj x)
    in
        foldr myop z t1
    end
