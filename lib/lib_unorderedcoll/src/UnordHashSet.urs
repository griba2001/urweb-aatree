(* HashSet *)

con hashSet :: Type -> Type

(* * Construction *)
val empty : a ::: Type -> Hashable.Hashable.hashable a -> hashSet a

val singleton : a ::: Type -> Hashable.Hashable.hashable a -> a -> hashSet a

(* * Query *)

val null : a ::: Type -> Hashable.Hashable.hashable a -> hashSet a -> bool

val size : a ::: Type -> Hashable.Hashable.hashable a -> hashSet a -> int

val member: a ::: Type -> Hashable.Hashable.hashable a -> eq a -> a -> hashSet a -> bool

(* * Mutation *)

val insert: a ::: Type -> Hashable.Hashable.hashable a -> eq a -> a -> hashSet a -> hashSet a

val delete: a ::: Type -> Hashable.Hashable.hashable a -> eq a -> a -> hashSet a -> hashSet a

(* * Interchange *)

val fromList : a ::: Type ->Hashable.Hashable.hashable a -> eq a -> list a -> hashSet a

val toList : a ::: Type -> Hashable.Hashable.hashable a -> hashSet a -> list a

(* * Partition *)

val filter : a ::: Type -> Hashable.Hashable.hashable a -> eq a -> (a -> bool) -> hashSet a -> hashSet a

val partition : a ::: Type -> Hashable.Hashable.hashable a -> eq a -> (a -> bool) -> hashSet a -> hashSet a * hashSet a

(* * Set oops *)

val union: a ::: Type -> Hashable.Hashable.hashable a -> eq a -> hashSet a -> hashSet a -> hashSet a

val diff: a ::: Type -> Hashable.Hashable.hashable a -> eq a -> hashSet a -> hashSet a -> hashSet a

val intersect: a ::: Type -> Hashable.Hashable.hashable a -> eq a -> hashSet a -> hashSet a -> hashSet a

(* * Mappings *)

val mp : a ::: Type -> b ::: Type -> Hashable.Hashable.hashable a -> Hashable.Hashable.hashable b -> eq b -> (a -> b) -> hashSet a -> hashSet b

