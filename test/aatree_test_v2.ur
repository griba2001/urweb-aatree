(* aatree_test_v2 *)

structure HL = HList
structure HSR = HSRandom

structure ATUT = AATree_UnitTest

fun getTestData (): transaction (list (int * string)) =
    let fun f (i: int): int * string = (i, str1 (chr (i + 48)))
    in li <- HSR.getSysRandomIntList 20 0 50 ;
       return (List.mp f (HL.nub li))
    end


fun main () =
        testdata <- getTestData () ;
        (failedResults, listFromTree) <- ATUT.unitTest (testdata) ;
        return <xml>
<body><br/>
<p>
         Data1       : {[testdata]}<br/>
         Through tree: {[listFromTree]}<br/>
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

