(* unordHashTree_test *)

structure HSR = HSRandom

structure T = ListMap_UnitTest

fun main () =
        testdata <- HSR.getRand_UniqueKey_StringXIntList () ;
        (failedResults, listFromTree) <- T.unitTest (testdata) ;
        return <xml>
<body><br/>
<p>
         Data1       : {[testdata]}<br/><br/>
         Through tree: {[listFromTree]}<br/>
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

