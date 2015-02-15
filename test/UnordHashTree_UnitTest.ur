(* HashEqTree UnitTest *)

structure F = HFunction
structure U = HUrUnit
structure HL = HList
structure HT = HTuple
structure HO = HOrd

val hashable_int: Hashable.Hashable.hashable int = Hashable.hashable_int

val hashable_string: Hashable.Hashable.hashable string = Hashable.hashable_string

structure T = HashEqTreeMap.HashEqTreeMap(struct
  type key = string
  type item = int
  val eq_key = eq_string
  val hashable_key = hashable_string
end)


fun unitTest (testdata: list (string * int)): transaction (xbody * list (string * int) * int) =

   (* Assert: testdata must not have repeated keys *)
   let val keys: list string = List.mp HT.fst testdata
   in if List.length keys <> List.length (HL.nub keys) 
      then error <xml>Error: Invalid test data (repeated keys)</xml>
      else
        let 
            val sortedInput : list (string * int) = List.sort (HO.gtBy HT.fst) testdata
            val treeData: T.htree string int  = T.fromList testdata
            val listOutput: list (string * int) = T.toList treeData
            val sortedOutput : list (string * int) = List.sort (HO.gtBy HT.fst) listOutput
            val (keysToDel, keysNotToDel): ((list string) * (list string)) = List.splitAt (List.length keys / 2) keys
            val treeWithdeletions: T.htree string int = List.foldl T.delete treeData keysToDel
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
                in return (xmlJoinedResults, sortedOutput, T.maxBucketSize treeData)
                end
        end
    end
