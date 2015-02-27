(* HFunction *)

fun id [a] (x: a) = x

fun const [a][b] (x: a) (_: b) = x

(* compose already defined in Top

fun compose [a][b][c] (f:b -> c) (g:a -> b) (x:a) = f (g x) *)

fun flip [a][b][c] (f:a -> b -> c) (x: b) (y: a) =  f y x

val andThen [a][b][c]: (a -> b) -> (b -> c) -> a -> c = flip compose

