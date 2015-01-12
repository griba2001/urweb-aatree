
fun xunless [m](_:monad m) p action = if p then return <xml/> else action

fun xwhen [m](_:monad m) p action = if p then action else return <xml/>

