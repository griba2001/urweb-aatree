open String

fun null (s: string) = "" = substring s {Start = 0, Len = 1}

fun concat (li: list string) = List.foldr append "" li