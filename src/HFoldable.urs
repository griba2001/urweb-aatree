
val all : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val any : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val sum : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> num b -> (a -> b) -> t a -> b

val prodInt : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> int) -> t a -> int

val prodFloat : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> float) -> t a -> float

val concat : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> HClasses.Monoid.monoid a -> t a -> a
