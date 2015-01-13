open String

fun null (s: string) = "" = s

fun concat (li: list string) = List.foldr append "" li