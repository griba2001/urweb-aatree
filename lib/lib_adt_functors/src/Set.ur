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

functor SortedSet(Q: sig con item :: Type
                         val ord_item: ord item
                     end): SSET = struct

  structure T = AATreeMap.AATreeMap(struct type key = Q.item
                                           type item = unit
                                           val ord_key = Q.ord_item
                                    end)
  open Q

  type item = item
  type t = T.tree item unit
  val empty: t = T.empty
  fun singleton (x: item) = T.singleton x ()
  val null: t -> bool = T.null
  val size: t -> int = T.size
  val member: item -> t -> bool = T.member
  fun insert (x: item) : t -> t = T.insert x ()
  val delete: item -> t -> t = T.delete
  val getAny: t -> option item = compose (Monad.liftM fst) T.getAnyPair
  val findMin: t -> option item = compose (Monad.liftM fst) T.findMin
  val findMax: t -> option item = compose (Monad.liftM fst) T.findMax
  val foldr [b] (myop: item -> b -> b) (z: b) (t1: t) =
    let
        fun myop' (p: item * unit): b -> b = myop p.1
    in T.foldr myop' z t1
    end
end

structure HStruc = Hashable.Hashable

functor HashedEqSet(Q: sig con item :: Type
                     val eq_item: eq item
                     val hashable_item: HStruc.hashable item
                 end): FSET = struct

  structure T = HashEqTreeMap.HashEqTreeMap(struct
                     type key = Q.item
                     type item = unit
                     val eq_key = Q.eq_item
                     val hashable_key = Q.hashable_item
                 end)
  open Q

  type item = item
  type t = T.htree item unit
  val empty: t = T.empty
  fun singleton (x: item) = T.singleton x ()
  val null: t -> bool = T.null
  val size: t -> int = T.size
  val member: item -> t -> bool = T.member
  fun insert (x: item) : t -> t = T.insert x ()
  val delete: item -> t -> t = T.delete
  val getAny: t -> option item = compose (Monad.liftM fst) T.getAnyPair
  val foldr [b] (myop: item -> b -> b) (z: b) (t1: t) =
    let
        fun myop' (p: item * unit): b -> b = myop p.1
    in T.foldr myop' z t1
    end
end


(* functor instances example: 

structure IntItem = struct
                      type item = int
                      val ord_item = ord_int
                    end

structure StringItem = struct
                      type item = string
                      val eq_item = eq_string
                      val hashable_item = Hashable.hashable_string
                    end

structure IntSortedSet = SortedSet( IntItem)

structure IntSortedSetOps = MkSetOps (IntSortedSet)

structure StringHashedSet = HashedEqSet( StringItem)

structure StringHashedSetOps = MkSetOps (StringHashedSet)
*)