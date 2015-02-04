
val all : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val any : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val sumBy : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> num b -> (a -> b) -> t a -> b

val intProdBy : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> int) -> t a -> int

val floatProdBy : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> float) -> t a -> float

val minBy : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> ord b -> (a -> b) -> b -> t a -> b

val maxBy : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> ord b -> (a -> b) -> b -> t a -> b

val concat : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> HClasses.Monoid.monoid a -> t a -> a

val foldMap : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->  HClasses.Foldable.foldable t -> HClasses.Monoid.monoid b -> (a -> b) -> t a -> b
