val null : a ::: Type -> eq a -> list a -> bool

val singleton : a ::: Type -> a -> list a

val concat : a ::: Type -> list (list a) -> list a

(*
val foldl: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> list a -> b

val foldlOnContainer: a ::: Type -> b ::: Type -> t ::: (Type -> Type) -> (a -> t b -> t b) -> t b -> list a -> t b
*)