fun null [a] (_:eq a) (li: list a) = li = Nil

(*
fun foldl [a] [b] (f : a -> b -> b) (acc: b) (ls: list a): b =
    case ls of
        [] => acc
        | x :: ls => foldl (f x acc) ls


fun foldlOnContainer [a] [b] [t] (f : a -> t b -> t b) (acc: t b) (ls: list a): t b =
    case ls of
        [] => acc
        | x :: ls => foldl (f x acc) ls
*)