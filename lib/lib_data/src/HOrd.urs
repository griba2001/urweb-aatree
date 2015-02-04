(* HOrd *)

datatype ordering = LT | EQ | GT

val eq_ordering: eq ordering

val compare: a ::: Type -> ord a -> a -> a -> ordering

val comparing : a ::: Type -> b ::: Type -> ord b -> (a -> b) -> a -> a -> ordering

val gtBy: a ::: Type -> b ::: Type -> ord b -> (a -> b) -> a -> a -> bool


