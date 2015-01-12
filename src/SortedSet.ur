
structure O = Option
structure F = HFunction

type set = AATree.tree

val eq [a] (_ : eq a) = AATree.eq

val empty [a] = AATree.empty

val null [a] = AATree.null

val singleton [a] = AATree.singleton

val insert [a] (_ : ord a) = AATree.insert

val delete [a] (_ : ord a) = AATree.delete

val member [a] (_ : ord a) (x: a) = F.compose O.isSome (AATree.lookup x)

val toList [a] = AATree.toList

val fromList [a] (_ : ord a) = AATree.fromList

