structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString
structure SM = SortedMap
structure HL = HList

open HTuple

structure HM = HMonad
structure HNR = HNowRandom

fun getTestData (): transaction (list (int * string)) =
    let fun f (i: int): int * string = (i, str1 (chr (i + 48)))
    in li <- HNR.getRandomIntList 20 0 50 ;
       return (List.mp f (HL.nub li))
    end

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList

fun xmlDltest1 (): transaction (xbody * list(int*string)) =
    testdata <- getTestData () ;
    res <- U.assertEqual "test1:" testdata (toFromList testdata) ;
    return (res, testdata)

fun xmlDltest2 (): transaction (xbody * list(int*string)) =
    testdata <- getTestData () ;
    res <- U.assertEqual "test2:" (SM.fromList testdata) (SM.fromList testdata) ;
    return (res, testdata)


fun main () =
              (test1, td1) <- xmlDltest1 () ;
              (test2, td2) <- xmlDltest2 () ;
              let val tests = join test1 test2
              in return <xml><body>Failed tests: {tests}<br/>
                                Data1: {[td1]}<br/>
                                Data2: {[td2]}<br/>
                                Result: {[toFromList td1]}<br/>
                     </body></xml>
              end
(*

fun main () = let val exp : tree int string = empty
                  val exp2 = insert 2 "b" (insert 1 "a" exp)
                  val exp3 = insert 3 "c" (delete 2 (fromList testdata))
                  val exp4 = (lookup 3 exp3, lookup 2 exp3) 
              in  
              return <xml><body>{[exp2]}<br/> {[exp3]}<br/> {[exp4]}</body></xml>
              end
*)
