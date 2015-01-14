
val id : a:::Type -> a -> a

val const : a:::Type -> b:::Type -> a -> b -> a

val compose : a:::Type -> b:::Type -> c:::Type -> (b -> c) -> (a -> b) -> a -> c

val andThen : a:::Type -> b:::Type -> c:::Type -> (a -> b) -> (b -> c) -> a -> c

val flip : a:::Type -> b:::Type -> c:::Type -> (a -> b -> c) -> b -> a -> c

val curry : a:::Type -> b:::Type -> c:::Type -> (a * b -> c) -> a -> b -> c

val uncurry : a:::Type -> b:::Type -> c:::Type -> (a -> b -> c) -> a * b -> c

val fst: a:::Type -> b:::Type -> a * b -> a

val snd: a:::Type -> b:::Type -> a * b -> b