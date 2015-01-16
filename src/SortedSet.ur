
structure O = Option
structure F = HFunction
structure HL = HList
structure HT = HTuple
open HFunction


con set a = AATree.tree a unit

val eq_unit: eq (unit) = let
                 fun eq' (x: unit) (y: unit) = True
        in
            mkEq eq'
        end
             

val eq_set = fn [a] (_ : eq a) =>
        let
            fun eq' (t1: set a) (t2: set a) =
                   let val t1' : AATree.tree a unit = t1
                       val t2' : AATree.tree a unit = t2
                   in t1 = t2
                   end 
        in
            mkEq eq'
        end


val empty [a]: set a = AATree.empty

val null [a]: set a -> bool = AATree.null

val singleton [a] (v: a): set a = AATree.singleton v ()

val size [a] : (set a -> int) = AATree.size

val insert [a] (_ : ord a) (v: a): (set a -> set a) = AATree.insert v ()

val delete [a] (_ : ord a) (v: a): (set a -> set a) = AATree.delete v

val member [a] (_ : ord a) (x: a): (set a -> bool) = F.compose O.isSome (AATree.lookup x)

val toList [a] : (set a -> list a) = F.compose (List.mp HT.fst) AATree.toList

val fromList [a] (_ : ord a): (list a -> set a) =
        let val f = fn (v: a) => (v, ())
        in F.compose AATree.fromList (List.mp f)
        end

val findMin [a] : (set a -> option a) =
        let val f: (option (a * unit) -> option a) = fn v =>
                        (case v of
                        Some (x, _) => Some x
                        | None => None) 
        in F.compose f AATree.findMin
        end   

val findMax [a] : (set a -> option a) =
        let val f: (option (a * unit) -> option a) = fn v =>
                        (case v of
                        Some (x, _) => Some x
                        | None => None)
        in F.compose f AATree.findMax
        end

val show_set = fn [a] (_ : show a) =>
        let
            fun show' (t1: set a)  = "set fromList: " ^ show (toList t1)
        in
            mkShow show'
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