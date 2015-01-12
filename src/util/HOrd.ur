datatype ordering = LT | EQ | GT

fun compare [a] (_: ord a) (x: a) (y: a) =
  if x < y then LT
  else if x <= y then EQ
  else GT