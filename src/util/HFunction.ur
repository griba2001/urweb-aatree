
fun id [a] (x: a) = x

fun const [a][b] (x: a) (_: b) = x

fun compose [a][b][c] (f:b -> c) (g:a -> b) (x:a) = f (g x)

fun andThen [a][b][c] (g:a -> b) (f:b -> c) (x:a) = f (g x)

fun flip [a][b][c] (f:a -> b -> c) (x: b) (y: a) =  f y x

