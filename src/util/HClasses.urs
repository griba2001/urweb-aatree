structure Semigroup : sig
        class semigroup :: Type -> Type
        val mkSemigroup : a ::: Type -> (a -> a -> a) -> semigroup a
        val sassoc : a:::Type -> semigroup a -> (a -> a -> a)
end

val semigroup_int: Semigroup.semigroup int
val semigroup_list: a:::Type -> Semigroup.semigroup (list a)

structure Monoid : sig
        class monoid :: Type -> Type
        val mkMonoid : a ::: Type -> Semigroup.semigroup a -> a -> monoid a
        val mempty: a ::: Type -> monoid a -> a
        val mappend: a ::: Type -> monoid a -> a -> a -> a
end

val monoid_int: Monoid.monoid int
val monoid_list: a:::Type -> Monoid.monoid (list a)

structure Foldable : sig
    class foldable :: (Type -> Type) -> Type -> Type -> Type
    val mkFoldable : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> ((a -> b -> b) -> b -> t a -> b) -> foldable t a b
    val foldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> foldable t a b -> (a -> b -> b) -> b -> t a -> b
end

val foldable_list: a:::Type -> b:::Type -> Foldable.foldable list a b
