(* HFoldable *)
open HClasses.Foldable
open HClasses.Monoid

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

fun sum [t][a][b] (_:foldable t) (_:num b) (proj: a -> b) (t1: t a): b =
    let
        fun myop (x: a) (acc: b): b = acc + proj x
    in
      foldr myop zero t1
    end

(* prod cannot be specified in terms of num,
   because num lacks the product neutral elem. definition
   (zero is defined in class num but not one)
*)
fun prodInt [t][a] (_:foldable t) (proj: a -> int) (t1: t a): int =
    let
        fun myop (x: a) (acc: int): int = acc * proj x
    in
      foldr myop 1 t1
    end

fun prodFloat [t][a] (_:foldable t) (proj: a -> float) (t1: t a): float =
    let
        fun myop (x: a) (acc: float): float = acc * proj x
    in
      foldr myop 1.0 t1
    end

fun concat [t][a] (_:foldable t) (_:monoid a) (t1: t a): a =

        foldr mappend mempty t1

