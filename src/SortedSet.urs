con set :: Type -> Type

val eq_set : a ::: Type -> eq a -> eq (set a)

val show_set : a ::: Type -> show a -> show (set a)

val empty : a ::: Type -> set a

val null : a ::: Type -> set a -> bool

val singleton : a ::: Type -> a -> set a

val size : a ::: Type -> set a -> int

val insert: a ::: Type -> ord a -> a -> set a -> set a

val delete: a ::: Type -> ord a -> a -> set a -> set a

val member: a ::: Type -> ord a -> a -> set a -> bool

val toList : a ::: Type -> set a -> list a

val fromList : a ::: Type -> ord a -> list a -> set a

val findMin : a ::: Type -> set a -> option a

val findMax : a ::: Type -> set a -> option a

val filter : a ::: Type -> ord a -> (a -> bool) -> set a -> set a

val partition : a ::: Type -> ord a -> (a -> bool) -> set a -> set a * set a

val union: a ::: Type -> ord a -> set a -> set a -> set a

val difference: a ::: Type -> ord a -> set a -> set a -> set a

val intersection: a ::: Type -> ord a -> set a -> set a -> set a
