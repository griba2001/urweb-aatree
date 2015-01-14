
datatype ordering = LT | EQ | GT

val compare: a ::: Type -> ord a -> a -> a -> ordering

val comparing : a ::: Type -> b ::: Type -> ord b -> (a -> b) -> a -> a -> ordering

