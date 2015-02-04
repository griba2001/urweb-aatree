(* HTuple *)

val eq_pair: a:::Type -> b:::Type -> eq a -> eq b -> eq (a * b)

val show_pair: a:::Type -> b:::Type -> show a -> show b -> show (a * b)

val curry : a:::Type -> b:::Type -> c:::Type -> (a * b -> c) -> a -> b -> c

val uncurry : a:::Type -> b:::Type -> c:::Type -> (a -> b -> c) -> a * b -> c

val fst: a:::Type -> b:::Type -> a * b -> a

val snd: a:::Type -> b:::Type -> a * b -> b

val swap: a:::Type -> b:::Type -> a * b -> b * a

val fmap: a:::Type -> b:::Type -> c:::Type -> (b -> c) -> a * b -> a * c
