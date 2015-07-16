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

  val all: (item -> bool) -> t -> bool
  val exists: (item -> bool) -> t -> bool
  val find: (item -> bool) -> t -> option item

  val foldr: b ::: Type -> (item -> b -> b) -> b -> t -> b

  val fromList: list item -> t
  val toList: t -> list item

  val show_set: show item -> show t
end

signature SSET = sig
  include FSET
  val findMin: t -> option item
  val findMax: t -> option item
end

functor MkSortedSet(Q: sig con item :: Type
                         val ord_item: ord item
                     end): (SSET where con item = Q.item) = struct

  structure T = AATreeMap.MkAATreeMap(struct type key = Q.item
                                             val ord_key = Q.ord_item
                                    end)

  type t = T.t unit

  val empty: t = T.empty
  fun singleton (x: Q.item) = T.singleton x ()

  val null: t -> bool = T.null
  val size: t -> int = T.size

  val member: Q.item -> t -> bool = T.member
  val getAny: t -> option Q.item = T.getAnyPair >>> Monad.liftM fst

  fun insert (x: Q.item) : t -> t = T.insert x ()
  val delete: Q.item -> t -> t = T.delete

  fun all (prop: Q.item -> bool): t -> bool = T.all (fst >>> prop)
  fun exists (prop: Q.item -> bool): t -> bool = T.exists (fst >>> prop)
  fun find (prop: Q.item -> bool): t -> option Q.item = T.find (fst >>> prop) >>> Monad.liftM fst

  val findMin: t -> option Q.item = T.findMin >>> Monad.liftM fst
  val findMax: t -> option Q.item = T.findMax >>> Monad.liftM fst

  val foldr [b] (myop: Q.item -> b -> b): b -> t -> b = T.foldr (fst >>> myop)

  fun fromList (li: list Q.item): t = List.foldl insert empty li
  fun toList (t1: t): list Q.item = foldr (curry Cons) [] t1

  type item = Q.item

  val show_set (_:show item) =
      let fun show' (t1: t) = "fromList: " ^ show (toList t1)
      in mkShow show'
      end
end

structure HStruc = Hashable.Hashable

functor MkUnordHashSet(Q: sig con item :: Type
                     val eq_item: eq item
                     val hashable_item: HStruc.hashable item
                 end): (FSET where con item = Q.item) = struct

  structure T = HashTreeMap.MkHashEqTreeMap(struct
                     type key = Q.item
                     val eq_key = Q.eq_item
                     val hashable_key = Q.hashable_item
                 end)

  type t = T.t unit

  val empty: t = T.empty
  fun singleton (x: Q.item) = T.singleton x ()

  val null: t -> bool = T.null
  val size: t -> int = T.size

  val member: Q.item -> t -> bool = T.member
  val getAny: t -> option Q.item = T.getAnyPair >>> Monad.liftM fst

  fun insert (x: Q.item) : t -> t = T.insert x ()
  val delete: Q.item -> t -> t = T.delete

  fun all (prop: Q.item -> bool): t -> bool = T.all (fst >>> prop)
  fun exists (prop: Q.item -> bool): t -> bool = T.exists (fst >>> prop)
  fun find (prop: Q.item -> bool): t -> option Q.item = T.find (fst >>> prop) >>> Monad.liftM fst

  val foldr [b] (myop: Q.item -> b -> b): b -> t -> b = T.foldr (fst >>> myop)

  fun fromList (li: list Q.item): t = List.foldl insert empty li
  fun toList (t1: t): list Q.item = foldr (curry Cons) [] t1

  type item = Q.item

  val show_set (_:show item) =
      let fun show' (t1: t) = "fromList: " ^ show (toList t1)
      in mkShow show'
      end
end
