structure L = HList
structure S = String
structure HS = HString
structure HM = HMonad

fun assertFailure (msg: string) = return <xml><p>{[msg]}</p></xml>

fun assertBool msg b = HM.xunless b (assertFailure msg)

fun assertEqual [a] (_: eq a) (_: show a) (preface: string) (expected: a) (actual: a) =
     let val pref : string = if HS.null preface then "" else S.append preface "\n"
         val msg : string = S.append pref
                   (S.append "expected: "  (S.append (show expected)
                   (S.append "\n but got: " (show actual))))
     in
        HM.xunless (actual = expected) (assertFailure msg)
     end
