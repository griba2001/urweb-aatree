(* aatree_test_v2 *)

structure HSR = HSRandom

structure T = AATreeMap_UnitTest

fun main () =
        testdata <- HSR.getRand_UniqueKey_IntXStringList () ;
        (failedResults, listFromTree) <- T.unitTest (testdata) ;
        return <xml>
<body><br/>
<p>
         Data1       : {[testdata]}<br/>
         Through tree: {[listFromTree]}<br/>
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

