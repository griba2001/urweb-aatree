
structure O = Option
structure F = HFunction
structure HT = HTuple

con set a = AATree.tree a unit

(*
val eq_set [a] (_ : eq a) = AATree.eq_tree
*)

val empty [a] = AATree.empty

val null [a] = AATree.null

val singleton [a] (v: a) = AATree.singleton v ()

val insert [a] (_ : ord a) (v: a) = AATree.insert v ()

val delete [a] (_ : ord a) (v: a) = AATree.delete v

val member [a] (_ : ord a) (x: a) = F.compose O.isSome (AATree.lookup x)

val toList [a] = F.compose (List.mp HT.fst) AATree.toList

val fromList [a] (_ : ord a) =
        let val f = fn (v: a) => (v, ())
        in F.compose AATree.fromList (List.mp f)
        end

