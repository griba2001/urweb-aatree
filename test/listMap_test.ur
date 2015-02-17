(* unordHashTree_test *)

structure HL = HList
structure HSR = HSRandom
structure HS = HString

structure T = ListMap_UnitTest


fun getRandString (topLen: int): transaction string =
    let fun f (i: int): string = str1 (chr (i + ord( strsub "A" 0)))
    in li <- HSR.getSysRandomIntList topLen 0 27 ;
       return (HS.concat (List.mp f li))
    end

fun getTestData (): transaction (list (string * int)) =
     li <- HSR.getSysRandomIntList 40 0 10 ;
     strs <- List.mapM getRandString li ;
     return (HL.zip (HL.nub strs) li)


fun main () =
        testdata <- getTestData () ;
        (failedResults, listFromTree) <- T.unitTest (testdata) ;
        return <xml>
<body><br/>
<p>
         Data1       : {[testdata]}<br/><br/>
         Through tree: {[listFromTree]}<br/>
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

