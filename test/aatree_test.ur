structure T = AATree
structure F = HFunction
structure U = HUrUnit

val testdata : list (int * string) = (1, "a") :: (2, "b") :: []

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList

fun xmlDltest1 (): transaction xbody = U.assertEqual "test1:" testdata (toFromList testdata)

fun main () = test1 <- xmlDltest1 () ;
              return <xml><body>Failed tests: {test1}<br/>
                                Result: {[toFromList testdata]}</body></xml>