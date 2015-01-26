datatype ordering = LT | EQ | GT

val eq_ordering = let fun eq' (x: ordering) y =
                          case (x, y) of
                            (LT, LT) => True
                            | (EQ, EQ) => True
                            | (GT, GT) => True
                            | _ => False
                  in mkEq eq'
                  end   

fun compare [a] (_: ord a) (x: a) (y: a) =
  if x < y then LT
  else if x <= y then EQ
  else GT

fun comparing [a][b] (_: ord b) (p: a -> b) (x: a) (y: a) = compare (p x) (p y)