signature SEMIGROUP = sig
        class semigroup :: Type -> Type
        val mkSemigroup : a ::: Type -> (a -> a -> a) -> semigroup a
        val sassoc : a:::Type -> semigroup a -> (a -> a -> a)
end

structure Semigroup : SEMIGROUP

val semigroup_int: Semigroup.semigroup int
val semigroup_string: Semigroup.semigroup string
val semigroup_list: a:::Type -> Semigroup.semigroup (list a)

signature MONOID = sig
        class monoid :: Type -> Type
        val mkMonoid : a ::: Type -> Semigroup.semigroup a -> a -> monoid a
        val mempty: a ::: Type -> monoid a -> a
        val mappend: a ::: Type -> monoid a -> a -> a -> a
end

structure Monoid : MONOID

val monoid_int: Monoid.monoid int
val monoid_string: Monoid.monoid string
val monoid_list: a:::Type -> Monoid.monoid (list a)

signature FOLDABLE = sig
     class foldable :: (Type -> Type) -> Type
     val mkFoldable : t ::: (Type -> Type) -> (a ::: Type -> b ::: Type ->
                                               ((a -> b -> b) -> b -> t a -> b)) -> foldable t
     val foldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->
                 foldable t -> (a -> b -> b) -> b -> t a -> b
end

structure Foldable : FOLDABLE

val foldable_list: Foldable.foldable list
