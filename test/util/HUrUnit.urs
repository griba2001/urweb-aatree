(* HUrUnit *)

val assertFailure : string -> transaction xbody

val assertBool : string -> bool -> transaction xbody

val assertEqual : a ::: Type -> eq a -> show a -> string -> a -> a -> transaction xbody

