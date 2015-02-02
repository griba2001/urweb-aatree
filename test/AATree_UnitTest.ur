(* AATree UnitTest *)

structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HL = HList
structure HT = HTuple
structure HO = HOrd

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList

fun unitTest (testdata: list (int * string)): transaction (xbody * list (int * string)) =

        let val keys: list int = List.mp HT.fst testdata
            val sortedInput : list (int * string) = List.sort (HO.gtBy HT.fst) testdata
            val inputFromTree : list (int * string) = toFromList testdata
            val treeData: T.tree int string = T.fromList testdata
            val (keysToDel, keysNotToDel): list int * list int = List.splitAt (List.length keys / 2) keys
            val treeWithdeletions: T.tree int string = List.foldl T.delete treeData keysToDel
            val memberOf = F.flip T.member
            val propDeletedAreNotMember: bool =
                       List.all (F.compose not (memberOf treeWithdeletions)) keysToDel
            val propNonDeletedAreMember: bool =
                       List.all (memberOf treeWithdeletions) keysNotToDel
        in
                tst0 <- U.assertEqual "through Tree conversion fails: " sortedInput inputFromTree ;
                tst1 <- U.assertBool "aaTreeProps fails" (T.aaTreeProps treeData) ;
                tst2 <- U.assertBool "propDeletedAreNotMember fails" propDeletedAreNotMember ;
                tst3 <- U.assertBool "propNonDeletedAreMember fails" propNonDeletedAreMember ;
                tst4 <- U.assertBool "propBST fails" (T.propBST treeData) ;
                let val testsResults = tst0 :: tst1 :: tst2 :: tst3 :: tst4 :: Nil
                    val xmlJoinedResults = List.foldr join <xml/> testsResults
                in return (xmlJoinedResults, inputFromTree)
                end
        end
