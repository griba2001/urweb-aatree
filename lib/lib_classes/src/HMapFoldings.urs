(* HMapFoldings : foldings on mapFoldables (foldables on types of arity 2) *)

val all : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> HClasses.MapFoldable.mapFoldable t -> (v -> bool) -> t k v -> bool

val any : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> HClasses.MapFoldable.mapFoldable t -> (v -> bool) -> t k v -> bool

val allWithKey : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> HClasses.MapFoldable.mapFoldable t -> (k -> v -> bool) -> t k v -> bool

val anyWithKey : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> HClasses.MapFoldable.mapFoldable t -> (k -> v -> bool) -> t k v -> bool

val sumBy : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> b ::: Type -> HClasses.MapFoldable.mapFoldable t -> num b -> (v -> b) -> t k v -> b

val intProdBy : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> HClasses.MapFoldable.mapFoldable t -> (v -> int) -> t k v -> int

val floatProdBy : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> HClasses.MapFoldable.mapFoldable t -> (v -> float) -> t k v -> float
