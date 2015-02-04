(* HFunction *)

val id : a:::Type -> a -> a

val const : a:::Type -> b:::Type -> a -> b -> a

val compose : a:::Type -> b:::Type -> c:::Type -> (b -> c) -> (a -> b) -> a -> c

val andThen : a:::Type -> b:::Type -> c:::Type -> (a -> b) -> (b -> c) -> a -> c

val flip : a:::Type -> b:::Type -> c:::Type -> (a -> b -> c) -> b -> a -> c

