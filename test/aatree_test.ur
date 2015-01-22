structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString
structure SM = SortedMap

open HTuple

val show_option [a] (_:show a): show (option a) =
        let fun show' (opt: option a) =
               case opt of
                  None => "None"
                  | Some v => HS.concat( "Some " :: show v :: [])
        in mkShow show'
        end


val testdata : list (int * string) = (1, "a") :: (2, "b") :: []

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList

fun xmlDltest1 (): transaction xbody = U.assertEqual "test1:" testdata (toFromList testdata)

fun xmlDltest2 (): transaction xbody = U.assertEqual "test2:" (SM.fromList testdata) (SM.fromList testdata)


fun main () = test1 <- xmlDltest1 () ;
              test2 <- xmlDltest2 () ;
              let val tests = join test1 test2
              in return <xml><body>Failed tests: {tests}<br/>
                                Result: {[toFromList testdata]}<br/>
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
