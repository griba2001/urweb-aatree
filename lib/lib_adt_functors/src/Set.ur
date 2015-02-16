(* Set *)

open HFunction
open HTuple

signature FSET = sig
  con t :: Type 
  con item :: Type
  val empty: t
  val singleton : item -> t
  val null: t -> bool
  val size: t -> int
  val insert: item -> t -> t
  val delete: item -> t -> t
  val member: item -> t -> bool
  val getAny: t -> option item
  val foldr: b ::: Type -> (item -> b -> b) -> b -> t -> b
end

signature SSET = sig
  include FSET
  val findMin: t -> option item
  val findMax: t -> option item
end

functor MkSortedSet(Q: sig con item :: Type
                         val ord_item: ord item
                     end): SSET = struct

  structure T = AATreeMap.MkAATreeMap(struct type key = Q.item
                                           type item = unit
                                           val ord_key = Q.ord_item
                                    end)

  type t = T.tree Q.item unit
  val empty: t = T.empty
  fun singleton (x: Q.item) = T.singleton x ()
  val null: t -> bool = T.null
  val size: t -> int = T.size
  val member: Q.item -> t -> bool = T.member
  fun insert (x: Q.item) : t -> t = T.insert x ()
  val delete: Q.item -> t -> t = T.delete
  val getAny: t -> option Q.item = compose (Monad.liftM fst) T.getAnyPair
  val findMin: t -> option Q.item = compose (Monad.liftM fst) T.findMin
  val findMax: t -> option Q.item = compose (Monad.liftM fst) T.findMax
  val foldr [b] (myop: Q.item -> b -> b) (z: b) (t1: t) =
    let
        fun myop' (p: Q.item * unit): b -> b = myop p.1
    in T.foldr myop' z t1
    end
  (* for SetOps referral *)
  type item = Q.item
end

structure HStruc = Hashable.Hashable

functor MkUnordHashSet(Q: sig con item :: Type
                     val eq_item: eq item
                     val hashable_item: HStruc.hashable item
                 end): FSET = struct

  structure T = HashEqTreeMap.MkHashEqTreeMap(struct
                     type key = Q.item
                     type item = unit
                     val eq_key = Q.eq_item
                     val hashable_key = Q.hashable_item
                 end)

  type t = T.htree Q.item unit
  val empty: t = T.empty
  fun singleton (x: Q.item) = T.singleton x ()
  val null: t -> bool = T.null
  val size: t -> int = T.size
  val member: Q.item -> t -> bool = T.member
  fun insert (x: Q.item) : t -> t = T.insert x ()
  val delete: Q.item -> t -> t = T.delete
  val getAny: t -> option Q.item = compose (Monad.liftM fst) T.getAnyPair
  val foldr [b] (myop: Q.item -> b -> b) (z: b) (t1: t) =
    let
        fun myop' (p: Q.item * unit): b -> b = myop p.1
    in T.foldr myop' z t1
    end
  (* for SetOps referral *)
  type item = Q.item
end
