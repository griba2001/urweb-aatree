
val all : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val any : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val sum : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> num b -> (a -> b) -> t a -> b

val intProd : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> int) -> t a -> int

val floatProd : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> float) -> t a -> float

val concat : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> HClasses.Monoid.monoid a -> t a -> a

val minFold : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> ord b -> (a -> b) -> b -> t a -> b

val maxFold : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> ord b -> (a -> b) -> b -> t a -> b