(* HSRandom *)

val getSysRandomIntList: int -> int -> int -> transaction (list int)

val getRandString: int -> transaction string

val getRand_UniqueKey_StringXIntList: unit -> transaction (list (string * int))

val getRand_UniqueKey_IntXStringList: unit -> transaction (list (int * string))