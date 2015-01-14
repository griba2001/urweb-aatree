
con tree :: Type -> Type -> Type

val eq_tree : k ::: Type -> v ::: Type -> eq k -> eq v -> eq (tree k v)

val show_tree : k ::: Type -> v ::: Type -> show k -> show v -> show (tree k v)

val empty : k ::: Type -> v ::: Type -> tree k v

val null : k ::: Type -> v ::: Type -> tree k v -> bool

val singleton : k ::: Type -> v ::: Type -> k -> v -> tree k v

val insert: k ::: Type -> v ::: Type -> ord k -> k -> v -> tree k v -> tree k v

val delete: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> tree k v

val lookup: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> option v

(* errors: Anonymous function remains at code generation
*)
val toList : k ::: Type -> v ::: Type -> tree k v -> list (k * v)

val fromList : k ::: Type -> v ::: Type -> ord k -> list (k * v) -> tree k v
