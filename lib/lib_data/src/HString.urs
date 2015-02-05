(* HString *)

val null : string -> bool

val concat : list string -> string

val foldr : b ::: Type -> (char -> b -> b) -> b -> string -> b
