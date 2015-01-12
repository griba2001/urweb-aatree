(* dlist following haskell's dlist *)

open HFunction

datatype dlist a = DL of (list a -> list a)

fun unDL [a] (dl: dlist a) (x: list a) = let val DL f = dl
                                           in f x
                                           end

fun fromList [a] (li: list a) = DL (List.append li)

fun toList [a] (dl: dlist a) = unDL dl Nil
                               

val empty [a] : dlist a = DL id

fun singleton [a] (x: a) = DL (curry Cons x)

fun cons [a] (x: a) (dl: dlist a) = DL (compose (curry Cons x) (unDL dl))

fun snoc [a] (dl: dlist a) (x:a) = DL (compose (unDL dl) (curry Cons x))

fun append [a] (xs: dlist a) (ys: dlist a) = DL (compose (unDL xs) (unDL ys))

fun concat [a] (li: list (dlist a)) : dlist a = List.foldr append empty li

