(* SortedMap *)

con dict :: Type -> Type -> Type

val eq_dict : k ::: Type -> v ::: Type -> eq k -> eq v -> eq (dict k v)

val show_dict : k ::: Type -> v ::: Type -> show (k * v) -> show (dict k v)

val empty : k ::: Type -> v ::: Type -> dict k v

val null : k ::: Type -> v ::: Type -> dict k v -> bool

val singleton : k ::: Type -> v ::: Type -> k -> v -> dict k v

val size : k ::: Type -> v ::: Type -> dict k v -> int

val insert : k ::: Type -> v ::: Type -> ord k -> k -> v -> dict k v -> dict k v

val insertWith : k ::: Type -> v ::: Type -> ord k -> (v -> v -> v) -> k -> v -> dict k v -> dict k v

val adjust: k ::: Type -> v ::: Type -> ord k -> (v -> v) -> k -> dict k v -> dict k v

val delete : k ::: Type -> v ::: Type -> ord k -> k -> dict k v -> dict k v

val lookup : k ::: Type -> v ::: Type -> ord k -> k -> dict k v -> option v

val toList : k ::: Type -> v ::: Type -> dict k v -> list (k * v)

val fromList : k ::: Type -> v ::: Type -> ord k -> list (k * v) -> dict k v

val mapValues :  k ::: Type -> v ::: Type -> w ::: Type -> (v -> w) -> dict k v -> dict k w

val mapKeysMonotonic : k ::: Type -> v ::: Type -> k' ::: Type -> (k -> k') -> dict k v -> dict k' v

val findMinByKey : k ::: Type -> v ::: Type -> dict k v -> option (k * v)

val findMaxByKey : k ::: Type -> v ::: Type -> dict k v -> option (k * v)

val findMinByVal : k ::: Type -> v ::: Type -> ord v -> dict k v -> option (k * v)

val findMaxByVal : k ::: Type -> v ::: Type -> ord v -> dict k v -> option (k * v)

val foldr: k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> b -> b) -> b -> dict k v -> b

val filterFoldr: k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> bool) -> (k * v -> b -> b) -> b -> dict k v -> b

val filter : k ::: Type -> v ::: Type -> ord k -> (v -> bool) -> dict k v -> dict k v

val filterWithKey : k ::: Type -> v ::: Type -> ord k -> (k -> v -> bool) -> dict k v -> dict k v

val partition : k ::: Type -> v ::: Type -> ord k -> (v -> bool) -> dict k v -> dict k v * dict k v

val partitionWithKey : k ::: Type -> v ::: Type -> ord k -> (k -> v -> bool) -> dict k v -> dict k v * dict k v

val union : k ::: Type -> v ::: Type -> ord k -> dict k v -> dict k v -> dict k v

val diff : k ::: Type -> v ::: Type -> ord k -> dict k v -> dict k v -> dict k v

val unionWith : k ::: Type -> v ::: Type -> ord k -> (v -> v -> v) -> dict k v -> dict k v -> dict k v

val deleteAll : k ::: Type -> v ::: Type -> ord k -> list k -> dict k v -> dict k v

val keys : k ::: Type -> v ::: Type -> dict k v -> list k

val values : k ::: Type -> v ::: Type -> dict k v -> list v

val all : k ::: Type -> v ::: Type -> (v -> bool) -> dict k v -> bool

val any : k ::: Type -> v ::: Type -> (v -> bool) -> dict k v -> bool

val allWithKey : k ::: Type -> v ::: Type -> (k -> v -> bool) -> dict k v -> bool

val anyWithKey : k ::: Type -> v ::: Type -> (k -> v -> bool) -> dict k v -> bool

val sum : k ::: Type -> v ::: Type -> b ::: Type -> num b -> (v -> b) -> dict k v -> b

val intProd : k ::: Type -> v ::: Type -> (v -> int) -> dict k v -> int

val floatProd : k ::: Type -> v ::: Type -> (v -> float) -> dict k v -> float

val valid : k ::: Type -> v ::: Type -> ord k -> dict k v -> bool