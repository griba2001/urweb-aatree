(* HOption *)

val show_option: a ::: Type -> show a -> show (option a)

val optTest: a ::: Type -> (a -> bool) -> a -> option a

val optAlternative : a ::: Type -> option a -> option a -> option a