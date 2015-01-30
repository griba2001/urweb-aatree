
structure O = Option
structure M = Monad
structure HL = HList
open HTuple
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
                   in t1' = t2'
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

val member [a] (_ : ord a) (x: a): (set a -> bool) = compose O.isSome (AATree.lookup x)

val toList [a] : (set a -> list a) = compose (List.mp fst) AATree.toList

val fromList [a] (_ : ord a): (list a -> set a) =
        let val f = fn (v: a) => (v, ())
        in compose AATree.fromList (List.mp f)
        end

val findMin [a] : (set a -> option a) = compose (M.liftM fst) AATree.findMin

val findMax [a] : (set a -> option a) = compose (M.liftM fst) AATree.findMax

val show_set = fn [a] (_ : show a) =>
        let
            fun show' (t1: set a)  = "set fromList: " ^ show (toList t1)
        in
            mkShow show'
        end

fun foldr [a][b] (myop: a -> b -> b) (acc: b) (t: set a): b =
    let
        fun myop' (p: a * unit): b -> b = myop p.1
    in AATree.foldr myop' acc t
    end

fun filterFoldr [a][b] (prop: a -> bool) (myop: a -> b -> b) (acc: b) : (set a -> b) =
    let fun myop' (x: a) (acc': b) =
             if prop x
                then myop x acc'
                else acc'
    in foldr myop' acc
    end

fun filter [a] (_: ord a) (prop: a -> bool) : (set a -> set a) = 

      filterFoldr prop insert empty

fun partition [a] (_: ord a) (prop: a -> bool) : (set a -> set a * set a) = 
    let
        fun myop (x: a) (pair: set a * set a): set a * set a =
                          if prop x then (insert x pair.1, pair.2)
                          else (pair.1, insert x pair.2)
    in
        foldr myop (empty, empty)
    end

val union [a] (_: ord a): (set a -> set a -> set a) = AATree.union

fun diff [a] (_: ord a): (set a -> set a -> set a) = AATree.difference

fun intersect [a] (_: ord a) (s1: set a) (s2: set a): set a =
   let
      val memberOf = flip member
   in
      filterFoldr (memberOf s1) insert empty s2
   end

fun mp [a][b] (_: ord b) (f: a -> b): set a -> set b =

    andThen toList (andThen (List.mp f) fromList)   (* left to right function composition *)

val mapMonotonic [a][b]: (a -> b) -> set a -> set b = AATree.mapKeysMonotonic 
