(* Arne Anderson Tree *)

con tree :: Type -> Type -> Type


(* * Instances *)

val eq_tree : k ::: Type -> v ::: Type -> eq k -> eq v -> eq (tree k v)

val show_tree : k ::: Type -> v ::: Type -> show k -> show v -> show (tree k v)


(* * Construction *)

val empty : k ::: Type -> v ::: Type -> tree k v

val singleton : k ::: Type -> v ::: Type -> k -> v -> tree k v


(* * Query *)

val null : k ::: Type -> v ::: Type -> tree k v -> bool

val size : k ::: Type -> v ::: Type -> tree k v -> int

val lookup: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> option v

val member: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> bool

val findMin : k ::: Type -> v ::: Type -> tree k v -> option (k * v)

val findMax : k ::: Type -> v ::: Type -> tree k v -> option (k * v)

(* getAnyPair to start min/max value foldings *)
val getAnyPair : k ::: Type -> v ::: Type -> tree k v -> option (k * v)


(* * Insert / delete *)

val insert: k ::: Type -> v ::: Type -> ord k -> k -> v -> tree k v -> tree k v

val insertWith: k ::: Type -> v ::: Type -> ord k -> (v -> v -> v) -> k -> v -> tree k v -> tree k v

val fromList : k ::: Type -> v ::: Type -> ord k -> list (k * v) -> tree k v

val delete: k ::: Type -> v ::: Type -> ord k -> k -> tree k v -> tree k v


(* * Adjust and mappings *)

val adjust: k ::: Type -> v ::: Type -> ord k -> (v -> v) -> k -> tree k v -> tree k v

val mapValues :  k ::: Type -> v ::: Type -> w ::: Type -> (v -> w) -> tree k v -> tree k w

val mapKeysMonotonic : k ::: Type -> v ::: Type -> k' ::: Type -> (k -> k') -> tree k v -> tree k' v


(* * Foldings *)

val foldr: k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> b -> b) -> b -> tree k v -> b

val filterFoldr: k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> bool) -> (k * v -> b -> b) -> b -> tree k v -> b

val toList : k ::: Type -> v ::: Type -> tree k v -> list (k * v)


(* * Invariants *)

val propBST : k ::: Type -> v ::: Type -> ord k -> tree k v -> bool
val aaTreeProps : k ::: Type -> v ::: Type -> tree k v -> bool
val valid : k ::: Type -> v ::: Type -> ord k -> tree k v -> bool