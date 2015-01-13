structure T = AATree
structure F = HFunction
structure U = HUrUnit

val testdata = Cons (1, Cons( 2, Nil))

val toFromList [a] (_ : ord a): (list a -> list a) = F.compose T.toList T.fromList

fun xmlDltest1 (): transaction xbody = U.assertEqual "test1:" testdata (toFromList testdata)

fun main () = test1 <- xmlDltest1 () ;
              return <xml><body>Failed tests: {test1}<br/>Result: {[toFromList testdata]}</body></xml>