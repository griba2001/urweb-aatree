structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString

val eq_pair [a][b] (_:eq a) (_:eq b): eq (a * b) =
        let fun eq' (p1: (a * b)) (p2: (a * b)) = p1.1 = p2.1 && p1.2 = p2.2
        in mkEq eq'
        end 

val show_pair [a][b] (_:show a) (_:show b): show (a * b) =
        let fun show' (p: a * b) = HS.concat( "(" :: show p.1 :: "," :: show p.2 :: ")" :: [])
        in mkShow show'
        end  

val testdata : list (int * string) = (1, "a") :: (2, "b") :: []

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList

fun xmlDltest1 (): transaction xbody = U.assertEqual "test1:" testdata (toFromList testdata)

fun main () = test1 <- xmlDltest1 () ;
              return <xml><body>Failed tests: {test1}<br/>
                                Result: {[toFromList testdata]}</body></xml>