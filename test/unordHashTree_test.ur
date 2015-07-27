(* unordHashTree_test *)

structure HL = HList
structure HSR = HSRandom
structure HS = HString
structure S = String

structure T = HashEqTreeMap_UnitTest

fun main () =
        testdata <- HSR.getRand_UniqueKey_StringXIntList () ;
        (failedResults, listFromTree, maxBucketSize) <- T.unitTest (testdata) ;
        return <xml>
<body><br/>
<p>
         Data1       : {[testdata]}<br/><br/>
         Through tree: {[listFromTree]}<br/><br/>
         MaxBucketSize: {[maxBucketSize]}
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

