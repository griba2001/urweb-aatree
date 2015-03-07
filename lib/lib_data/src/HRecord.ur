(* HRecord: Record functions*)

(* record updating, thanks to Adam Chlipala *)

fun overwrite [a] [b] [b'] [a ~ b] [a ~ b'] (r : $(a ++ b)) (r' : $b') : $(a ++ b') =
     r --- b ++ r'