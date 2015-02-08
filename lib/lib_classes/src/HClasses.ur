
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
val semigroup_string : semigroup string = mkSemigroup String.append
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
(* HClasses *)

open Monoid

val monoid_int: monoid int = mkMonoid 0
val monoid_string: monoid string = mkMonoid ""
val monoid_list [a]: monoid (list a) = mkMonoid []

(* Foldable

with the help of Adam Chlipala
 *)

structure Foldable : sig
     class foldable :: (Type -> Type) -> Type
     val mkFoldable : t ::: (Type -> Type) -> (a ::: Type -> b ::: Type ->
                                               ((a -> b -> b) -> b -> t a -> b)) -> foldable t
     val foldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->
                 foldable t -> (a -> b -> b) -> b -> t a -> b
     val filterFoldr : t ::: (Type -> Type) -> a ::: Type -> b ::: Type ->
                 foldable t -> (a -> bool) -> (a -> b -> b) -> b -> t a -> b
end = struct
     type foldable t = a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b
     fun mkFoldable [t] (f: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b) = @@f

     fun foldr [t][a][b] (f: foldable t): (a -> b -> b) -> b -> t a -> b = f
     fun filterFoldr [t][a][b] (f: foldable t) (prop: a -> bool) (myop: a -> b -> b) (z: b) (st: t a): b =
                let fun myop' (x: a) (acc: b): b =
                        if prop x
                                then myop x acc
                                else acc
                in f myop' z st
                end
end

open Foldable

val foldable_list : foldable list = mkFoldable @@List.foldr

(* folding of a structure of mappings *)
structure MapFoldable : sig
     class mapFoldable :: (Type -> Type -> Type) -> Type
     val mkMapFoldable : t ::: (Type -> Type -> Type) -> (k ::: Type -> v ::: Type -> b ::: Type ->
                                               ((k * v -> b -> b) -> b -> t k v -> b)) -> mapFoldable t
     val foldr : t ::: (Type -> Type -> Type) -> k ::: Type -> v ::: Type -> b ::: Type ->
                 mapFoldable t -> (k * v -> b -> b) -> b -> t k v -> b
end = struct
     type mapFoldable t = k ::: Type -> v ::: Type -> b ::: Type -> (k * v -> b -> b) -> b -> t k v -> b
     fun mkMapFoldable [t] (f: k ::: Type -> v ::: Type -> b ::: Type -> ((k * v) -> b -> b) -> b -> t k v -> b) = @@f

     fun foldr [t][k][v][b] (f: mapFoldable t): (k * v -> b -> b) -> b -> t k v -> b = f
end
