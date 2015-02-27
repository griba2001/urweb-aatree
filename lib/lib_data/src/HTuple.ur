(* HTuple *)

structure HS = HString
structure HO = HOrd

val eq_pair [a][b] (_:eq a) (_:eq b): eq (a * b) =
        let fun eq' (p1: (a * b)) (p2: (a * b)) = p1.1 = p2.1 && p1.2 = p2.2
        in mkEq eq'
        end

val show_pair [a][b] (_:show a) (_:show b): show (a * b) =
        let fun show' (p: a * b) = HS.concat( "(" :: show p.1 :: "," :: show p.2 :: ")" :: [])
        in mkShow show'
        end

fun curry [a][b][c] (f: a * b -> c) (x: a) (y: b) = f (x, y)

fun uncurry [a][b][c] (f: a -> b -> c) (p: a * b) = f p.1 p.2

(* fst, snd defined in Top are type functions
*)
fun fst [a][b] (p: a * b) = p.1

fun snd [a][b] (p: a * b) = p.2

fun swap [a][b] (p: a * b): b * a = (p.2, p.1)

fun fmap [a][b][c] (f: b -> c) (p: a * b): a * c = (p.1, f p.2)
