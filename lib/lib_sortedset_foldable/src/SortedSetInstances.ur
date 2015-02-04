(* SortedSetInstances *)

structure HC = HClasses
structure HCS = HC.Semigroup
open SortedSet

val semigroup_set [a] (_: ord a): HCS.semigroup (set a) = HCS.mkSemigroup union

structure HCM = HC.Monoid

val monoid_set [a] (_: ord a): HCM.monoid (set a) = HCM.mkMonoid empty


structure HCF = HC.Foldable

val foldable_set: HCF.foldable set = HCF.mkFoldable @@foldr

