
val all : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val any : t ::: (Type -> Type) -> a ::: Type -> HClasses.Foldable.foldable t -> (a -> bool) -> t a -> bool

val sum : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HClasses.Foldable.foldable t -> num b -> (a -> b) -> t a -> b
