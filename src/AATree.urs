
con tree :: Type -> Type

val eq_tree : a ::: Type -> eq a -> eq (tree a)

val empty : a ::: Type -> tree a

val null : a ::: Type -> tree a -> bool

val singleton : a ::: Type -> a -> tree a

val insert: a ::: Type -> ord a -> a -> tree a -> tree a

val delete: a ::: Type -> ord a -> a -> tree a -> tree a

val lookup: a ::: Type -> ord a -> a -> tree a -> option a

val toList : a ::: Type -> tree a -> list a

val fromList : a ::: Type -> ord a -> list a -> tree a
