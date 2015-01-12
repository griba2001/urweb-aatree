datatype ordering = LT | EQ | GT

fun compare [a] (_: eq a) (_: ord a) (x: a) (y: a) =
  if x = y then EQ
  else if x < y then LT
  else GT