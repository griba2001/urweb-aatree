
val semigroup_set: a ::: Type -> ord a -> HClasses.Semigroup.semigroup (SortedSet.set a)

val monoid_set: a ::: Type -> ord a -> HClasses.Monoid.monoid (SortedSet.set a)

val foldable_set: HClasses.Foldable.foldable SortedSet.set