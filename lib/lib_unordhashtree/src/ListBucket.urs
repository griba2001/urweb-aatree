(* ListBucket *)

con t :: Type -> Type -> Type

val empty : k ::: Type -> v ::: Type -> t k v  (* eq k -> *)

val null : k ::: Type -> v ::: Type -> t k v -> bool

val singleton : k ::: Type -> v ::: Type -> k -> v -> t k v

val size : k ::: Type -> v ::: Type -> t k v -> int

val insert : k ::: Type -> v ::: Type -> eq k -> k -> v -> t k v -> t k v

val insertWith : k ::: Type -> v ::: Type -> eq k -> (v -> v -> v) -> k -> v -> t k v -> t k v

val adjust: k ::: Type -> v ::: Type -> eq k -> (v -> v) -> k -> t k v -> t k v

val delete : k ::: Type -> v ::: Type -> eq k -> k -> t k v -> t k v

val lookup : k ::: Type -> v ::: Type -> eq k -> k -> t k v -> option v

val fromList : k ::: Type -> v ::: Type -> eq k -> list (k * v) -> t k v

val toList : k ::: Type -> v ::: Type -> eq k -> t k v -> list (k * v)

val foldr : k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> b -> b) -> b -> t k v -> b

val getAnyPair : k ::: Type -> v ::: Type -> t k v -> option (k * v)

val mapValues : k ::: Type -> v ::: Type -> w ::: Type -> (v -> w) -> t k v -> t k w

val valid : k ::: Type -> v ::: Type -> eq k -> t k v -> bool