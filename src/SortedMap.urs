con dict :: Type -> Type -> Type

(*
val eq_dict : k ::: Type -> v ::: Type -> eq k -> eq v -> eq (dict a)
*)

val empty : k ::: Type -> v ::: Type -> dict k v

val null : k ::: Type -> v ::: Type -> dict k v -> bool

val singleton : k ::: Type -> v ::: Type -> k -> v -> dict k v

val put : k ::: Type -> v ::: Type -> ord k -> k -> v -> dict k v -> dict k v

val delete : k ::: Type -> v ::: Type -> ord k -> k -> dict k v -> dict k v

val lookup : k ::: Type -> v ::: Type -> ord k -> k -> dict k v -> option v

val toList : k ::: Type -> v ::: Type -> dict k v -> list (k * v)

val fromList : k ::: Type -> v ::: Type -> ord k -> list (k * v) -> dict k v

val mapValues :  k ::: Type -> v ::: Type -> w ::: Type -> (v -> w) -> dict k v -> dict k w

val mapKeysMonotonic : k ::: Type -> v ::: Type -> k' ::: Type -> (k -> k') -> dict k v -> dict k' v

val filter : k ::: Type -> v ::: Type -> ord k -> (k -> bool) -> dict k v -> dict k v

val partition : k ::: Type -> v ::: Type -> ord k -> (k -> bool) -> dict k v -> dict k v * dict k v