(* HSRandom *)

structure HN = HNum
structure HM = HMonad
structure HL = HList
structure HS = HString
structure S = String

fun rndToRange (from:int) (to:int) (rnd: int): int = mod (HN.abs rnd) (to - from) + from

(* --- with system provided "rand" function which I discovered later *)

fun sysNextp (from:int) (to:int) (cnt: int): transaction (option (int * int)) =
    if cnt > 0 then rnd <- rand ;
                    return (Some (rndToRange from to rnd, cnt -1))
               else return None

fun getSysRandomIntList (topSize:int) (from:int) (to:int): transaction (list int) =
     
     if topSize = 0 then return []
     else
        rnd <- rand ;
        let val len = rndToRange 0 topSize rnd
        in HM.unfoldrM (sysNextp from to) len
        end

fun getRandString (topLen: int): transaction string =
    let fun f (i: int): string = S.str (chr (i + ord( S.sub "A" 0)))
    in li <- getSysRandomIntList topLen 0 26 ;
       letter_list <- return (List.mp f li) ;
       return (HS.concat letter_list)
    end

fun getRand_UniqueKey_StringXIntList (): transaction (list (string * int)) =
     li <- getSysRandomIntList 40 0 10 ;
     strs <- List.mapM getRandString li ;
     strs' <- return (HL.nub strs) ;
     return (HL.zip strs' li)

fun getRand_UniqueKey_IntXStringList (): transaction (list (int * string)) =
    let fun f (i: int): int * string = (i, S.str (chr (i + ord( S.sub "A" 0))))
    in li <- getSysRandomIntList 20 0 50 ;
       return (List.mp f (HL.nub li))
    end
