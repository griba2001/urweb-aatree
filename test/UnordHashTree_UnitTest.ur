(* AATree UnitTest *)

structure T = UnordHashTree
structure F = HFunction
structure U = HUrUnit
structure HL = HList
structure HT = HTuple
structure HO = HOrd

val hashable_int: Hashable.Hashable.hashable int = Hashable.hashable_int

fun unitTest (testdata: list (int * string)): transaction (xbody * list (int * string)) =

   (* Assert: testdata must not have repeated keys *)
   let val keys: list int = List.mp HT.fst testdata
   in if List.length keys <> List.length (HL.nub keys) 
      then error <xml>Error: Invalid test data (repeated keys)</xml>
      else
        let 
            val sortedInput : list (int * string) = List.sort (HO.gtBy HT.fst) testdata
            val treeData: T.hashTree int string = T.fromList testdata
            val listOutput: list (int * string) = T.toList treeData
            val sortedOutput : list (int * string) = List.sort (HO.gtBy HT.fst) listOutput
            val (keysToDel, keysNotToDel): list int * list int = List.splitAt (List.length keys / 2) keys
            val treeWithdeletions: T.hashTree int string = List.foldl T.delete treeData keysToDel
            val memberOf = F.flip T.member
            val propDeletedAreNotMember: bool =
                       List.all (F.compose not (memberOf treeWithdeletions)) keysToDel
            val propNonDeletedAreMember: bool =
                       List.all (memberOf treeWithdeletions) keysNotToDel
        in
                tst0 <- U.assertEqual "through Tree conversion fails: " sortedInput sortedOutput ;
                tst1 <- U.assertBool "UnordHashTree validity fails" (T.valid treeData) ;
                tst2 <- U.assertBool "propDeletedAreNotMember fails" propDeletedAreNotMember ;
                tst3 <- U.assertBool "propNonDeletedAreMember fails" propNonDeletedAreMember ;
                let val testsResults = tst0 :: tst1 :: tst2 :: tst3 :: Nil
                    val xmlJoinedResults = List.foldr join <xml/> testsResults
                in return (xmlJoinedResults, sortedOutput)
                end
        end
    end
