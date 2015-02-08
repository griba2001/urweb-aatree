(* HClasses *)

structure Semigroup : sig
        class semigroup :: Type -> Type
        val mkSemigroup : a ::: Type -> (a -> a -> a) -> semigroup a
        val sassoc : a:::Type -> semigroup a -> (a -> a -> a)
end

val semigroup_int: Semigroup.semigroup int
val semigroup_string: Semigroup.semigroup string
val semigroup_list: a:::Type -> Semigroup.semigroup (list a)

structure Monoid : sig
        class monoid :: Type -> Type
        val mkMonoid : a ::: Type -> Semigroup.semigroup a -> a -> monoid a
        val mempty: a ::: Type -> monoid a -> a
        val mappend: a ::: Type -> monoid a -> a -> a -> a
end

val monoid_int: Monoid.monoid int
val monoid_string: Monoid.monoid string
val monoid_list: a:::Type -> Monoid.monoid (list a)

structure Foldable : sig
     class foldable :: (Type -> Type) -> Type
     val mkFoldable : t ::: (Type -> Type) -> (a ::: Type -> b ::: Type ->
                                               ((a -> b -> b) -> b -> t a -> b)) -> foldable t
     val foldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->
                 foldable t -> (a -> b -> b) -> b -> t a -> b
     val filterFoldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->
                 foldable t -> (a -> bool) -> (a -> b -> b) -> b -> t a -> b
end

val foldable_list: Foldable.foldable list

(* folding of a structure of mappings *)
structure MapFoldable : sig
     class mapFoldable :: (Type -> Type -> Type) -> Type
     val mkMapFoldable : t ::: (Type -> Type -> Type) -> (k ::: Type -> v ::: Type -> b ::: Type ->
                                               ((k * v -> b -> b) -> b -> t k v -> b)) -> mapFoldable t
     val foldr : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> b ::: Type ->
                 mapFoldable t -> (k * v -> b -> b) -> b -> t k v -> b
end
