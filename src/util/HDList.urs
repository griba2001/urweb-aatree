
con dlist :: Type -> Type

val unDL : a ::: Type -> dlist a -> list a -> list a

val fromList : a ::: Type -> list a -> dlist a

val toList : a ::: Type -> dlist a -> list a

(* *)

val empty : a ::: Type -> dlist a

val singleton : a ::: Type -> a -> dlist a

val cons : a ::: Type -> a -> dlist a -> dlist a

val snoc :  a ::: Type -> dlist a -> a -> dlist a

val append : a ::: Type -> dlist a -> dlist a -> dlist a

val concat : a ::: Type -> list (dlist a) -> dlist a

