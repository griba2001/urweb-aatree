(* HFoldable *)

(* open HClasses *)
(* open HClasses.Monoid *)
open HClasses.MapFoldable
open HTuple

fun all [t][k][v] (_:mapFoldable t) (prop: v -> bool) (t1: t k v): bool =
    let
        fun myop (p: k * v) (b: bool): bool = b && prop p.2
    in
      foldr myop True t1
    end

fun any [t][k][v] (_:mapFoldable t) (prop: v -> bool) (t1: t k v): bool =
    let
        fun myop (p: k * v) (b: bool): bool = b || prop p.2
    in
      foldr myop False t1
    end

fun allWithKey [t][k][v] (_:mapFoldable t) (prop: k -> v -> bool) (t1: t k v): bool =
    let
        fun myop (p: k * v) (b: bool): bool = b && uncurry prop p
    in
      foldr myop True t1
    end

fun anyWithKey [t][k][v] (_:mapFoldable t) (prop: k -> v -> bool) (t1: t k v): bool =
    let
        fun myop (p: k * v) (b: bool): bool = b || uncurry prop p
    in
      foldr myop False t1
    end

fun sumBy [t][k][v][b] (_:mapFoldable t) (_:num b) (proj: v -> b) (t1: t k v): b =
    let
        fun myop (p: k * v) (acc: b): b = acc + proj p.2
    in
      foldr myop zero t1
    end

fun intProdBy [t][k][v] (_:mapFoldable t) (proj: v -> int) (t1: t k v): int =
    let
        fun myop (p: k * v) (acc: int): int = acc * proj p.2
    in
      foldr myop 1 t1
    end

fun floatProdBy [t][k][v] (_:mapFoldable t) (proj: v -> float) (t1: t k v): float =
    let
        fun myop (p: k * v) (acc: float): float = acc * proj p.2
    in
      foldr myop 1.0 t1
    end

fun minBy [t][k][v][b] (_:mapFoldable t) (_:ord b) (proj: k * v -> b) (z: b) (t1: t k v): b =
    let
        fun myop (p: k * v) (acc: b): b = min acc (proj p)
    in
        foldr myop z t1
    end

fun maxBy [t][k][v][b] (_:mapFoldable t) (_:ord b) (proj: k * v -> b) (z: b) (t1: t k v): b =
    let
        fun myop (p: k * v) (acc: b): b = max acc (proj p)
    in
        foldr myop z t1
    end
