signature SEMIGROUP = sig
        class semigroup :: Type -> Type
        val mkSemigroup : a ::: Type -> (a -> a -> a) -> semigroup a
        val sassoc : a:::Type -> semigroup a -> (a -> a -> a)
end

structure Semigroup : SEMIGROUP = struct
        type semigroup a = (a -> a -> a)
        fun mkSemigroup [a] (assoc: (a -> a -> a)): semigroup a = assoc
        fun sassoc [a] (s: semigroup a) (x: a) (y: a): a = s x y
end

open Semigroup

(* semigroup instances *)
val semigroup_int: semigroup int = mkSemigroup plus
val semigroup_string : semigroup string = mkSemigroup String.append
val semigroup_list [a]: semigroup (list a) = mkSemigroup List.append

(* Monoid *)
signature MONOID = sig
        class monoid :: Type -> Type
        val mkMonoid : a ::: Type -> Semigroup.semigroup a -> a -> monoid a
        val mempty: a ::: Type -> monoid a -> a
        val mappend: a ::: Type -> monoid a -> a -> a -> a
end

structure Monoid : MONOID = struct
        type monoid a = {Append: a -> a -> a,
                         Empty: a}
        fun mkMonoid [a] (_: semigroup a) (empty: a): monoid a = {Append = sassoc, Empty = empty}
        fun mempty [a] (m: monoid a) = m.Empty
        fun mappend [a] (m: monoid a) = m.Append
end

open Monoid

val monoid_int: monoid int = mkMonoid 0
val monoid_string: monoid string = mkMonoid ""
val monoid_list [a]: monoid (list a) = mkMonoid []

(* Foldable *)

signature FOLDABLE = sig
     class foldable :: (Type -> Type) -> Type
     val mkFoldable : t ::: (Type -> Type) -> (a ::: Type -> b ::: Type ->
                                               ((a -> b -> b) -> b -> t a -> b)) -> foldable t
     val foldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->
                 foldable t -> (a -> b -> b) -> b -> t a -> b
end

structure Foldable : FOLDABLE = struct
     type foldable t = a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b
     fun mkFoldable [t] (f: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b) = @@f

     fun foldr [t][a][b] (f: foldable t): (a -> b -> b) -> b -> t a -> b = f
end

open Foldable

val foldable_list : foldable list = mkFoldable @@List.foldr

