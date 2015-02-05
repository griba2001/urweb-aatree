(* HString *)

structure S = String

fun null (s: string) = not (S.lengthGe s 1)

fun concat (li: list string) = List.foldr S.append "" li

fun foldr [b] (myop: char -> b -> b) (z: b) (s: string): b =
    let fun foldr' (idx: int) (z: b): b =
      if idx = 0 then myop (S.sub s idx) z
      else let val new_z = myop (S.sub s idx) z
           in foldr' (idx-1) new_z
           end
       val len = S.length s
    in if len > 0 then foldr' (len - 1) z
       else z
    end
