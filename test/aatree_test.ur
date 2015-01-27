structure T = AATree
structure F = HFunction
structure U = HUrUnit
structure HS = HString
structure SM = SortedMap
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
        let val expected : list(int*string) = List.sort HT.gtByFst testdata 
            val actual : list(int*string) = toFromList testdata
        in   
                tst <- U.assertEqual "test1:" expected actual ;
                return (tst, testdata)
        end

fun main () =
        (test1, td1) <- xmlTest1 () ;
        return <xml>
<body>Failed tests: {test1}<br/>
                Data1: {[td1]}<br/>
                Result: {[toFromList td1]}<br/>
</body></xml>

