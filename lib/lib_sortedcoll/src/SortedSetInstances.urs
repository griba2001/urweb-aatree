(* SortedSetInstances *)

val semigroup_sset: a ::: Type -> ord a -> HClasses.Semigroup.semigroup (SortedSet.sset a)

val monoid_sset: a ::: Type -> ord a -> HClasses.Monoid.monoid (SortedSet.sset a)

val foldable_sset: HClasses.Foldable.foldable SortedSet.sset

