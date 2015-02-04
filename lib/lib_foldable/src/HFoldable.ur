(* HFoldable *)

open HClasses
open HClasses.Semigroup
open HClasses.Monoid
open HClasses.Foldable

fun all [t][a] (_:foldable t) (prop: a -> bool) (t1: t a): bool =
    let
        fun myop (x: a) (b: bool): bool = b && prop x
    in
      foldr myop True t1
    end

fun any [t][a] (_:foldable t) (prop: a -> bool) (t1: t a): bool =
    let
        fun myop (x: a) (b: bool): bool = b || prop x
    in
      foldr myop False t1
    end

fun sumBy [t][a][b] (_:foldable t) (_:num b) (proj: a -> b) (t1: t a): b =
    let
        fun myop (x: a) (acc: b): b = acc + proj x
    in
      foldr myop zero t1
    end

(* prod cannot be specified in terms of num,
   because num lacks the product neutral elem. definition
   (zero is defined in class num but not one)
*)
fun intProdBy [t][a] (_:foldable t) (proj: a -> int) (t1: t a): int =
    let
        fun myop (x: a) (acc: int): int = acc * proj x
    in
      foldr myop 1 t1
    end

fun floatProdBy [t][a] (_:foldable t) (proj: a -> float) (t1: t a): float =
    let
        fun myop (x: a) (acc: float): float = acc * proj x
    in
      foldr myop 1.0 t1
    end

fun minBy [t][a][b] (_:foldable t) (_:ord b) (proj: a -> b) (z: b) (t1: t a): b =
    let
        fun myop (x: a) (acc: b): b = min acc (proj x)
    in
        foldr myop z t1
    end

fun maxBy [t][a][b] (_:foldable t) (_:ord b) (proj: a -> b) (z: b) (t1: t a): b =
    let
        fun myop (x: a) (acc: b): b = max acc (proj x)
    in
        foldr myop z t1
    end

fun concat [t][a] (_:foldable t) (_:monoid a) (t1: t a): a =

        foldr mappend mempty t1

fun foldMap [t][a][b] (_:foldable t) (_:monoid b) (proj: a -> b) (t1: t a): b =

     let fun myop (x: a): b -> b = mappend (proj x)
     in
        foldr myop mempty t1
     end

