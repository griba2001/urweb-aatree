structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString
structure HL = HList

structure HM = HMonad
structure HR = HRandom
structure HT = HTuple

fun getTestData (): transaction (list (int * string)) =
    let fun f (i: int): int * string = (i, str1 (chr (i + 48)))
    in li <- HR.getRandomIntList 20 0 50 ;
       return (List.mp f (HL.nub li))
    end

val toFromList [k][v] (_ : ord k): (list (k * v) -> list (k * v)) = F.compose T.toList T.fromList

fun xmlTest1 (): transaction (xbody * list(int*string)) =
        testdata <- getTestData () ;
        let val keys: list int = List.mp HT.fst testdata
            val expected : list(int*string) = List.sort HT.gtByFst testdata 
            val actual : list(int*string) = toFromList testdata
            val treeData: T.tree int string = T.fromList testdata
            val (keysToDel, keysNotToDel): list int * list int = List.splitAt (List.length keys / 2) keys
            val treeWithdeletions: T.tree int string = List.foldl T.delete treeData keysToDel
            val propDeletedAreNotMember: bool = List.all (F.compose not (F.flip T.member treeWithdeletions)) keysToDel
            val propNonDeletedAreMember: bool = List.all (F.flip T.member treeWithdeletions) keysNotToDel
        in   
                tst0 <- U.assertEqual "test1:" expected actual ;
                tst1 <- U.assertBool "prop1 fails" (T.prop1 treeData) ;
                tst2 <- U.assertBool "prop2 fails" (T.prop2 treeData) ;
                tst3 <- U.assertBool "prop3 fails" (T.prop3 treeData) ;
                tst4 <- U.assertBool "prop4 fails" (T.prop4 treeData) ;
                tst5 <- U.assertBool "prop5 fails" (T.prop5 treeData) ;
                tst6 <- U.assertBool "propDeletedAreNotMember fails" propDeletedAreNotMember ; 
                tst7 <- U.assertBool "propNonDeletedAreMember fails" propNonDeletedAreMember ;
                let val tsts = List.foldr join <xml/> (tst0 :: tst1 :: tst2 :: tst3 :: tst4 :: tst5 :: tst6 :: tst7 :: Nil)  
                in return (tsts, testdata)
                end  
        end


fun main () =
        (test1, td1) <- xmlTest1 () ;
        return <xml>
<body>Failed tests: {test1}<br/>
<p>
         Data1       : {[td1]}<br/>
         Through tree: {[toFromList td1]}<br/>
</p>
</body></xml>

