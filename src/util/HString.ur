open String

fun null (s: string) = String.lengthGe s 1

fun concat (li: list string) = List.foldr append "" li