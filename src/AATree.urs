
con tree :: Type -> Type -> Type

val eq_tree : k ::: Type -> v ::: Type -> eq k -> eq v -> eq (tree k v)

val show_tree : k ::: Type -> v ::: Type -> show k -> show v -> show (tree k v)

val empty : k ::: Type -> v ::: Type -> tree k v

val null : k ::: Type -> v ::: Type -> tree k v -> bool

val singleton : k ::: Type -> v ::: Type -> k -> v -> tree k v

val size : k ::: Type -> v ::: Type -> tree k v -> int

val insert: k ::: Type -> v ::: Type -> ord k -> k -> v -> tree k v -> tree k v

val insertWith: k ::: Type -> v ::: Type -> ord k -> (v -> v -> v) -> k -> v -> tree k v -> tree k v

val adjust: k ::: Type -> v ::: Type -> ord k -> (v -> v) -> k -> tree k v -> tree k v

val delete: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> tree k v

val lookup: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> option v

val member: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> bool

val foldr: k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> b -> b) -> b -> tree k v -> b

val filterFoldr: k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> bool) -> (k * v -> b -> b) -> b -> tree k v -> b

val filter : k ::: Type -> v ::: Type -> ord k -> (k -> bool) -> tree k v -> tree k v

val partition : k ::: Type -> v ::: Type -> ord k -> (k -> bool) -> tree k v -> tree k v * tree k v

val toList : k ::: Type -> v ::: Type -> tree k v -> list (k * v)

val fromList : k ::: Type -> v ::: Type -> ord k -> list (k * v) -> tree k v

val mapValues :  k ::: Type -> v ::: Type -> w ::: Type -> (v -> w) -> tree k v -> tree k w

val mapKeysMonotonic : k ::: Type -> v ::: Type -> k' ::: Type -> (k -> k') -> tree k v -> tree k' v

val findMin : k ::: Type -> v ::: Type -> tree k v -> option (k * v)

val findMax : k ::: Type -> v ::: Type -> tree k v -> option (k * v)

val union : k ::: Type -> v ::: Type -> ord k -> tree k v -> tree k v -> tree k v

(*
val difference : k ::: Type -> v ::: Type -> ord k -> tree k v -> tree k v -> tree k v

val intersection : k ::: Type -> v ::: Type -> ord k -> tree k v -> tree k v -> tree k v
*)

val prop1 : k ::: Type -> v ::: Type -> tree k v -> bool
val prop2 : k ::: Type -> v ::: Type -> tree k v -> bool
val prop3 : k ::: Type -> v ::: Type -> tree k v -> bool
val prop4 : k ::: Type -> v ::: Type -> tree k v -> bool
val prop5 : k ::: Type -> v ::: Type -> tree k v -> bool
