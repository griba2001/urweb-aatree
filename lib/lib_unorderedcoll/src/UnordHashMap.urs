(* HashMap *)

con hashMap :: Type -> Type -> Type

(* * Construction *)

val empty : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashMap k v

val singleton : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> k -> v -> hashMap k v

(* * Query *)

val null : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashMap k v -> bool

val size : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashMap k v -> int

(* * *)

val insert : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> v -> hashMap k v -> hashMap k v

val insertWith : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> v -> v) -> k -> v -> hashMap k v -> hashMap k v

val adjust: k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> v) -> k -> hashMap k v -> hashMap k v

val delete : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> hashMap k v -> hashMap k v

val lookup : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> k -> hashMap k v -> option v

(* * *)

val toList : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashMap k v -> list (k * v)

val fromList : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> list (k * v) -> hashMap k v

val keys : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashMap k v -> list k

val values : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> hashMap k v -> list v

(*****)

val findMinByKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord k -> hashMap k v -> option (k * v)

val findMaxByKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord k -> hashMap k v -> option (k * v)

val findMinByVal : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord v -> hashMap k v -> option (k * v)

val findMaxByVal : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> ord v -> hashMap k v -> option (k * v)

(*****)

val foldr: k ::: Type -> v ::: Type -> b ::: Type -> Hashable.Hashable.hashable k -> (k * v -> b -> b) -> b -> hashMap k v -> b

val filterFoldr: k ::: Type -> v ::: Type -> b ::: Type -> Hashable.Hashable.hashable k -> (k * v -> bool) -> (k * v -> b -> b) -> b -> hashMap k v -> b

(*****)

val filter : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> bool) -> hashMap k v -> hashMap k v

val filterWithKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (k -> v -> bool) -> hashMap k v -> hashMap k v

val partition : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> bool) -> hashMap k v -> hashMap k v * hashMap k v

val partitionWithKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (k -> v -> bool) -> hashMap k v -> hashMap k v * hashMap k v

(*****)

val union : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> hashMap k v -> hashMap k v -> hashMap k v

val diff : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> hashMap k v -> hashMap k v -> hashMap k v

val unionWith : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> (v -> v -> v) -> hashMap k v -> hashMap k v -> hashMap k v

val deleteAll : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> eq k -> list k -> hashMap k v -> hashMap k v

(*****)

val all : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> (v -> bool) -> hashMap k v -> bool

val any : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> (v -> bool) -> hashMap k v -> bool


val allWithKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> (k -> v -> bool) -> hashMap k v -> bool

val anyWithKey : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> (k -> v -> bool) -> hashMap k v -> bool

(*****)

val sumBy : k ::: Type -> v ::: Type -> b ::: Type -> Hashable.Hashable.hashable k -> num b -> (v -> b) -> hashMap k v -> b

val intProdBy : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> (v -> int) -> hashMap k v -> int

val floatProdBy : k ::: Type -> v ::: Type -> Hashable.Hashable.hashable k -> (v -> float) -> hashMap k v -> float


(*****)
