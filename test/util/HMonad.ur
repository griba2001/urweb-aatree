
fun xunless [m](_:monad m) (p: bool) (action: m xbody): m xbody = if p then return <xml/> else action

fun xwhen [m](_:monad m) (p: bool) (action: m xbody): m xbody = if p then action else return <xml/>

