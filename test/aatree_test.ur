structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString

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

fun main () = test1 <- xmlDltest1 () ;
              return <xml><body>Failed tests: {test1}<br/>
                                Result: {[toFromList testdata]}</body></xml>
(*

fun main () = let val exp : tree int string = empty
                  val exp2 = insert 2 "b" (insert 1 "a" exp)
                  val exp3 = insert 3 "c" (delete 2 (fromList testdata))
                  val exp4 = (lookup 3 exp3, lookup 2 exp3) 
              in  
              return <xml><body>{[exp2]}<br/> {[exp3]}<br/> {[exp4]}</body></xml>
              end
*)
