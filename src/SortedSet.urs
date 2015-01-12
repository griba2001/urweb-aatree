con set :: Type -> Type

val eq : a ::: Type -> eq a -> eq (set a)

val empty : a ::: Type -> set a

val null : a ::: Type -> set a -> bool

val singleton : a ::: Type -> a -> set a

val insert: a ::: Type -> eq a -> ord a -> a -> set a -> set a

val delete: a ::: Type -> eq a -> ord a -> a -> set a -> set a

val member: a ::: Type -> eq a -> ord a -> a -> set a -> bool

val toList : a ::: Type -> set a -> list a

(* problem: too-deep unification variable
val fromList : a ::: Type -> eq a -> ord a -> list a -> set a
*)