signature DSMAP = sig

con t :: Type -> Type
con key :: Type

val empty : item ::: Type -> t item
val singleton : item ::: Type -> key -> item -> t item

val null : item ::: Type -> t item -> bool
val size : item ::: Type -> t item -> int

val lookup: item ::: Type -> key -> t item -> option item
val member: item ::: Type -> key -> t item -> bool
val getAnyPair : item ::: Type -> t item -> option (key * item)

val insert: item ::: Type -> key -> item -> t item -> t item
val insertWith: item ::: Type -> (item -> item -> item) -> key -> item -> t item -> t item
val fromList : item ::: Type -> list (key * item) -> t item

val delete: item ::: Type -> key -> t item -> t item
val adjust: item ::: Type -> (item -> item) -> key -> t item -> t item
val update: item ::: Type -> (item -> option item) -> key -> t item -> t item

val mapValues: item ::: Type -> b ::: Type -> (item -> b) -> t item -> t b

val foldr: item ::: Type -> b ::: Type -> (key * item -> b -> b) -> b -> t item -> b
val toList : item ::: Type -> t item -> list (key * item)

val exists : item ::: Type -> (key * item -> bool) -> t item -> bool
val all : item ::: Type -> (key * item -> bool) -> t item -> bool
val find: item ::: Type -> (key * item -> bool) -> t item -> option (key * item)

val valid : item ::: Type -> t item -> bool

end