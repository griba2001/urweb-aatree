
fun id [a] (x: a) = x

fun const [a][b] (x: a) (_: b) = x

fun compose [a][b][c] (f:b -> c) (g:a -> b) (x:a) = f (g x)

fun andThen [a][b][c] (g:a -> b) (f:b -> c) (x:a) = f (g x)

fun flip [a][b][c] f x y =  f y x

fun curry [a][b][c] f x y = f (x, y)

fun uncurry [a][b][c] f (x, y) = f x y