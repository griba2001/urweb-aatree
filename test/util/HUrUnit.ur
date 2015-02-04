(* HUrUnit *)

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
         val xmsg : xbody = (let val sep = <xml><br/></xml>
                                 val x1 = str2xml (HS.concat (pref :: "expected: " :: show expected :: Nil))
                                 val x2 = str2xml (HS.concat ( "but got: " :: show actual :: Nil))
                            in
                              List.foldr join <xml/> (x1 :: sep :: x2 :: sep :: Nil)
                            end)   
     in
        HM.xunless (actual = expected) (assertFailureX xmsg)
     end
