(* HList *)

val null : a ::: Type -> list a -> bool

val singleton : a ::: Type -> a -> list a

val concat : a ::: Type -> list (list a) -> list a

val partition : a ::: Type -> (a -> bool) -> list a -> list a * list a

val elem : a ::: Type -> eq a -> a -> list a -> bool

val nub : a ::: Type -> eq a -> list a -> list a

val delete : a ::: Type -> eq a -> a -> list a -> list a

val zip : a ::: Type -> b ::: Type -> list a -> list b -> list (a * b)
