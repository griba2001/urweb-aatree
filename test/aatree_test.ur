structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString
structure SM = SortedMap
structure HL = HList

open HTuple

structure HM = HMonad
structure HR = HRandom
open HOrd


fun getTestData (): transaction (list (int * string)) =
    let fun f (i: int): int * string = (i, str1 (chr (i + 48)))
    in li <- HR.getRandomIntList 20 0 50 ;
       return (List.mp f (HL.nub li))
    end

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList


fun gtByFst[a][b] (_:ord a) (x: a * b) (y: a * b): bool = comparing fst x y = GT

fun xmlDltest1 (): transaction (xbody * list(int*string)) =
    testdata <- getTestData () ;
    res <- U.assertEqual "test1:" (List.sort gtByFst testdata) (toFromList testdata) ;
    return (res, testdata)
  

fun main () =
              (test1, td1) <- xmlDltest1 () ;
              let val tests = test1 (* join test1 test2 // Data2: {[td2]}<br/> *)
              in return <xml><body>Failed tests: {tests}<br/>
                                Data1: {[td1]}<br/>
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
