(* SortedSetInstances *)

structure HC = HClasses
structure HCS = HC.Semigroup
structure SS = SortedSet
open SortedSet


val semigroup_sset [a] (_: ord a): HClasses.Semigroup.semigroup (SS.sset a) = HCS.mkSemigroup union


structure HCM = HC.Monoid

val monoid_sset [a] (_: ord a): HCM.monoid (SS.sset a) = HCM.mkMonoid empty


structure HCF = HC.Foldable

val foldable_sset: HCF.foldable SS.sset = HCF.mkFoldable @@foldr
