structure HC = HClasses

val all : t ::: (Type -> Type) -> a ::: Type -> HC.Foldable.foldable t -> (a -> bool) -> t a -> bool

val any : t ::: (Type -> Type) -> a ::: Type -> HC.Foldable.foldable t -> (a -> bool) -> t a -> bool

val sum : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> HC.Foldable.foldable t -> num b -> (a -> b) -> t a -> b
