(* HMonad *)

fun xunless [m](_:monad m) (p: bool) (action: m xbody): m xbody = if p then return <xml/> else action

fun xwhen [m](_:monad m) (p: bool) (action: m xbody): m xbody = if p then action else return <xml/>

fun unfoldrM [m][a][b] (_:monad m) (f: b -> m (option (a * b))) (seed: b) : m (list a) =
    res <- f seed ;
    case res of
       Some (a, new_b) => rest <- unfoldrM f new_b ;
                          return (a :: rest)
       | None => return []

(*
fun unfoldrOptionM [m][a][b] (_:monad m) (f: b -> m (option ((option a) * b))) (seed: b) : m (list a) =
    res <- f seed ;
    case res of
       Some (optA, new_b) => rest <- unfoldrOptionM f new_b ;
                          (case optA : option a of
                            Some a => return (a :: rest)
                            | None => return rest
                          )
       | None => return []
*)
