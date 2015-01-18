class semigroup :: Type -> Type
val mkSemigroup : a ::: Type -> (a -> a -> a) -> semigroup a
val sAssoc : a:::Type -> semigroup a -> (a -> a -> a)

val semigroup_int: semigroup int
val semigroup_list: a:::Type -> semigroup (list a)

class monoid :: Type -> Type
val mkMonoid : a ::: Type -> a -> monoid a

val mempty: a ::: Type -> monoid a -> a
val mappend: a ::: Type -> monoid a -> (semigroup a -> a -> a -> a)

val monoid_int: monoid int
val monoid_list: a:::Type -> monoid (list a)

class foldable :: (Type -> Type) -> Type -> Type -> Type
val mkFoldable: t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> {Foldr: (a -> b -> b) -> b -> t a -> b} -> foldable t a b
val ffoldr: t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> foldable t a b -> (a -> b -> b) -> b -> t a -> b

val foldable_list: a ::: Type -> b ::: Type -> foldable list a b

