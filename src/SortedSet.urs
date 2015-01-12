con set :: Type -> Type

val eq : a ::: Type -> eq a -> eq (set a)

val empty : a ::: Type -> set a

val null : a ::: Type -> set a -> bool

val singleton : a ::: Type -> a -> set a

val insert: a ::: Type -> ord a -> a -> set a -> set a

val delete: a ::: Type -> ord a -> a -> set a -> set a

val member: a ::: Type -> ord a -> a -> set a -> bool

val toList : a ::: Type -> set a -> list a

val fromList : a ::: Type -> ord a -> list a -> set a
