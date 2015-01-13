structure D = HDList
structure F = HFunction
structure U = HUrUnit

val testdata = Cons (1, Cons( 2, Nil))

val toFromList [a]: (list a -> list a) = F.compose D.toList D.fromList

fun xmlDltest1 () = U.assertEqual "test1:" testdata (toFromList testdata)  (* (D.toList (D.fromList testdata)) *)

fun main () = test1 <- xmlDltest1 () ;
              return <xml><body>Failed tests: {test1}<br/>Result: {[toFromList testdata]}</body></xml>