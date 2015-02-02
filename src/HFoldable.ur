(* HFoldable *)
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

fun sum [t][a][b] (_:foldable t) (_:num b) (proj: a -> b) (t1: t a): b =
    let
        fun myop (x: a) (acc: b): b = acc + proj x
    in
      foldr myop zero t1
    end

(* prod cannot be specified in terms of num,
   because num lacks the product neutral elem. definition
   (zero is defined in class num but not one)
-----

fun prod [t][a] (_:foldable t) (_:num b) (proj: a -> b) (t1: t a): b =
    let
        fun myop (x: a) (acc: b): b = acc * proj x
    in
      foldr myop one t1
    end
*)
