(* HFunction *)

fun id [a] (x: a) = x

fun const [a][b] (x: a) (_: b) = x

fun flip [a][b][c] (f:a -> b -> c) (x: b) (y: a) =  f y x

