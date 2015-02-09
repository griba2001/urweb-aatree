(* HSRandom *)

structure HN = HNum
structure HM = HMonad

fun rndToRange (from:int) (to:int) (rnd: int): int = mod (HN.abs rnd) (to - from) + from

(* --- with system provided "rand" function which I discovered later *)

fun sysNextp (from:int) (to:int) (cnt: int): transaction (option (int * int)) =
    if cnt > 0 then rnd <- rand ;
                    return (Some (rndToRange from to rnd, cnt -1))
               else return None

fun getSysRandomIntList (topSize:int) (from:int) (to:int): transaction (list int) =
     rnd <- rand ;
     if topSize = 0 then return []
     else
        let val len = rndToRange 0 topSize rnd
        in HM.unfoldrM (sysNextp from to) len
        end
