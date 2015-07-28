(* HUrUnit *)

structure HM = HMonad

fun assertFailure (msg: string) = return <xml><p>{[msg]}</p></xml>

fun assertFailureX (xmsg: xbody) = return <xml><p>{xmsg}</p></xml>

fun str2xml (msg: string): xbody = <xml>{[msg]}</xml>

fun assertBool msg b = HM.xunless b (assertFailure msg)

fun assertEqual [a] (_: eq a) (_: show a) (preface: string) (expected: a) (actual: a) =
     let val xmsg : xbody = <xml>{txt preface}<br/>expected: {[expected]}<br/>actual: {[actual]}</xml>
     in
        HM.xunless (actual = expected) (assertFailureX xmsg)
     end
