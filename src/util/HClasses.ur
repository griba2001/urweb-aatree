
structure Semigroup : sig
        class semigroup :: Type -> Type
        val mkSemigroup : a ::: Type -> (a -> a -> a) -> semigroup a
        val sassoc : a:::Type -> semigroup a -> (a -> a -> a)
end = struct
        type semigroup a = (a -> a -> a)
        fun mkSemigroup [a] (assoc: (a -> a -> a)): semigroup a = assoc
        fun sassoc [a] (s: semigroup a) (x: a) (y: a): a = s x y
end

open Semigroup

(* semigroup instances *)
val semigroup_int: semigroup int = mkSemigroup plus
val semigroup_list [a]: semigroup (list a) = mkSemigroup List.append

(* Monoid *)
structure Monoid : sig
        class monoid :: Type -> Type
        val mkMonoid : a ::: Type -> semigroup a -> a -> monoid a
        val mempty: a ::: Type -> monoid a -> a
        val mappend: a ::: Type -> monoid a -> a -> a -> a
end = struct
        type monoid a = {Append: a -> a -> a,
                         Empty: a}
        fun mkMonoid [a] (_: semigroup a) (empty: a): monoid a = {Append = sassoc, Empty = empty}
        fun mempty [a] (m: monoid a) = m.Empty
        fun mappend [a] (m: monoid a) = m.Append
end

open Monoid

val monoid_int: monoid int = mkMonoid 0
val monoid_list [a]: monoid (list a) = mkMonoid []

(* Foldable *)

structure Foldable : sig
    class foldable :: (Type -> Type) -> Type -> Type -> Type
    val mkFoldable : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> ((a -> b -> b) -> b -> t a -> b) -> foldable t a b
    val foldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type -> foldable t a b -> (a -> b -> b) -> b -> t a -> b
end = struct
    type foldable t = fn a b => (a -> b -> b) -> b -> t a -> b
    fun mkFoldable [t][a][b](f: (a -> b -> b) -> b -> t a -> b) = f
    val foldr [t][a][b] (f: foldable t a b): (a -> b -> b) -> b -> t a -> b = f
end

open Foldable

val foldable_list [a][b]: foldable list a b = mkFoldable List.foldr

