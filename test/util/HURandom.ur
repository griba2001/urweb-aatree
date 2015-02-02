structure M = Monad
structure R = Random
structure HN = HNum
structure HM = HMonad

fun rndToRange (from:int) (to:int) (rnd: int): int = mod (HN.abs rnd) (to - from) + from

fun getRandomPosInt (from:int) (to:int): transaction int =
    if not (from >= 0 && to >= from) then error <xml>getRandomPosInt: illegal values</xml>
    else
        rnd <- R.urandom () ;
        case rnd of
                Some v => return (rndToRange from to v)
                | None => return (-1)

fun nextp (from:int) (to:int) (cnt: int): transaction (option (option int * int)) =
    if cnt > 0 then optRnd <- R.urandom () ;
                    return (Some (M.liftM (rndToRange from to) optRnd, cnt -1))
               else return None

fun getRandomIntList (topSize:int) (from:int) (to:int): transaction (list int) =
     len <- getRandomPosInt 0 topSize ;
     if len < 0 then return [] 
     else HM.unfoldrOptionM (nextp from to) len

(* --- with system provided "rand" function which I discovered later *)

fun sysNextp (from:int) (to:int) (cnt: int): transaction (option (int * int)) =
    if cnt > 0 then rnd <- rand ;
                    return (Some (rndToRange from to rnd, cnt -1))
               else return None

fun getSysRandomIntList (topSize:int) (from:int) (to:int): transaction (list int) =
     rnd <- rand ;
     let val len = rndToRange 0 topSize rnd
     in HM.unfoldrM (sysNextp from to) len
     end
