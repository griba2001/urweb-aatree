(* SortedSet *)

con sset :: Type -> Type

val eq_set : a ::: Type -> eq a -> eq (sset a)

val show_set : a ::: Type -> show a -> show (sset a)

val empty : a ::: Type -> sset a

val null : a ::: Type -> sset a -> bool

val singleton : a ::: Type -> a -> sset a

val size : a ::: Type -> sset a -> int

val insert: a ::: Type -> ord a -> a -> sset a -> sset a

val delete: a ::: Type -> ord a -> a -> sset a -> sset a

val member: a ::: Type -> ord a -> a -> sset a -> bool

val toList : a ::: Type -> sset a -> list a

val fromList : a ::: Type -> ord a -> list a -> sset a

val findMin : a ::: Type -> sset a -> option a

val findMax : a ::: Type -> sset a -> option a

val foldr : a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> sset a -> b

val filterFoldr : a ::: Type -> b ::: Type -> (a -> bool) -> (a -> b -> b) -> b -> sset a -> b

val filter : a ::: Type -> ord a -> (a -> bool) -> sset a -> sset a

val partition : a ::: Type -> ord a -> (a -> bool) -> sset a -> sset a * sset a

val union: a ::: Type -> ord a -> sset a -> sset a -> sset a

val diff: a ::: Type -> ord a -> sset a -> sset a -> sset a

val intersect: a ::: Type -> ord a -> sset a -> sset a -> sset a

val mp : a ::: Type -> b ::: Type -> ord b -> (a -> b) -> sset a -> sset b

val mapMonotonic : a ::: Type -> b ::: Type -> (a -> b) -> sset a -> sset b

val valid:  a ::: Type -> ord a -> sset a -> bool

val all : a ::: Type -> (a -> bool) -> sset a -> bool

val any : a ::: Type -> (a -> bool) -> sset a -> bool

val sumBy : a ::: Type -> b ::: Type -> num b -> (a -> b) -> sset a -> b

val intProdBy : a ::: Type -> (a -> int) -> sset a -> int

val floatProdBy : a ::: Type -> (a -> float) -> sset a -> float

val minBy : a ::: Type -> b ::: Type -> ord b -> (a -> b) -> b -> sset a -> b

val maxBy : a ::: Type -> b ::: Type -> ord b -> (a -> b) -> b -> sset a -> b
