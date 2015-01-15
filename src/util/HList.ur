fun null [a] (_:eq a) (li: list a) = li = Nil

fun singleton [a] (x: a) = x :: []

fun concat [a] (xss: list (list a)) = List.foldr List.append [] xss
