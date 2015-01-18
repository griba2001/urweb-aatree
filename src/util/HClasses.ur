(* class semigroup *)
con semigroup a = (a -> a -> a)  
fun mkSemigroup [a] (assoc: (a -> a -> a)): semigroup a = assoc
fun sAssoc [a] (s: semigroup a) (x: a) (y: a): a = s x y

(* semigroup instances *)
val semigroup_int: semigroup int = mkSemigroup plus
val semigroup_list [a]: semigroup (list a) = mkSemigroup List.append

con monoid a = {Append: semigroup a -> a -> a -> a, Empty: a}
fun mkMonoid [a] (empty: a): monoid a = {Append = sAssoc, Empty = empty}
fun mempty [a] (m: monoid a) = m.Empty
fun mappend [a] (m: monoid a) = m.Append

val monoid_int: monoid int = mkMonoid 0
val monoid_list [a]: monoid (list a) = mkMonoid []

con foldable t a b = {Foldr: (a -> b -> b) -> b -> t a -> b}
fun mkFoldable [t][a][b] (rc: {Foldr: (a -> b -> b) -> b -> t a -> b}): foldable t a b = rc
val ffoldr [t][a][b] (f: foldable t a b): (a -> b -> b) -> b -> t a -> b = f.Foldr

val foldable_list[a][b]: foldable list a b = {Foldr = List.foldr}

  
