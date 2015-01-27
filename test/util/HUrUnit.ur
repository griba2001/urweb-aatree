structure L = HList
structure S = String
structure HS = HString
structure HM = HMonad

fun assertFailure (msg: string) = return <xml><p>{[msg]}</p></xml>

fun assertFailureX (xmsg: xbody) = return <xml><p>{xmsg}</p></xml>

fun str2xml (msg: string): xbody = <xml>{[msg]}</xml>

fun assertBool msg b = HM.xunless b (assertFailure msg)

fun assertEqual [a] (_: eq a) (_: show a) (preface: string) (expected: a) (actual: a) =
     let val pref : string = if HS.null preface then "" else S.append preface "\n"
         val xmsg : xbody = (let val x1 = str2xml (HS.concat (pref :: "expected: " :: show expected :: []))
                                 val x2 = <xml><br/></xml>
                                 val x3 = str2xml (HS.concat ( "but got: " :: show actual :: []))
                            in
                              join x1 (join x2 x3)
                            end)   
     in
        HM.xunless (actual = expected) (assertFailureX xmsg)
     end
