(* HashTree *)

con hashTree :: Type -> Type -> Type


val empty : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashTree k v

val null : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashTree k v -> bool

val size : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashTree k v -> int

val singleton : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> k -> v -> hashTree k v

val insertWith : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> v -> v) -> k -> v -> hashTree k v -> hashTree k v

val insert : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> v -> hashTree k v -> hashTree k v

val delete : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> hashTree k v -> hashTree k v

val adjust : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> v) -> k -> hashTree k v -> hashTree k v

val lookup : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> hashTree k v -> option v

val member : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> hashTree k v -> bool

val fromList : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> list (k * v) -> hashTree k v

val toList : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashTree k v -> list (k * v)

val foldr: k ::: Type -> v ::: Type -> b ::: Type -> Hashable.Hashable.hashable k -> (k * v -> b -> b) -> b -> hashTree k v -> b

val filterFoldr: k ::: Type -> v ::: Type -> b ::: Type -> Hashable.Hashable.hashable k -> (k * v -> bool) -> (k * v -> b -> b) -> b -> hashTree k v -> b

val findMinByVal : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord v -> hashTree k v -> option (k * v)
val findMaxByVal : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord v -> hashTree k v -> option (k * v)
val findMinByKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord k -> hashTree k v -> option (k * v)
val findMaxByKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord k -> hashTree k v -> option (k * v)

val valid: k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> hashTree k v -> bool

val maxBucketSize: k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> hashTree k v -> int