(* Map *)

open HFunction
open HTuple

signature FMAP = sig
  con t :: Type -> Type
  con key :: Type
  val empty: item ::: Type -> t item
  val singleton : item ::: Type -> key -> item -> t item
  val null: item ::: Type -> t item -> bool
  val size: item ::: Type -> t item -> int
  val insert: item ::: Type -> key -> item -> t item -> t item
  val insertWith: item ::: Type -> (item -> item -> item) -> key -> item -> t item -> t item
  val delete: item ::: Type -> key -> t item -> t item
  val lookup: item ::: Type -> key -> t item -> option item
  val member: item ::: Type -> key -> t item -> bool
  val adjust: item ::: Type -> (item -> item) -> key -> t item -> t item
  val update: item ::: Type -> (item -> option item) -> key -> t item -> t item
  val exists: item ::: Type -> (key * item -> bool) -> t item -> bool
  val all: item ::: Type -> (key * item -> bool) -> t item -> bool
  val find: item ::: Type -> (key * item -> bool) -> t item -> option (key * item)
  val getAnyPair: item ::: Type -> t item -> option (key * item)
  val mp: item ::: Type -> b ::: Type -> (item -> b) -> t item -> t b
  val foldrWithPair: item ::: Type -> b ::: Type -> (key * item -> b -> b) -> b -> t item -> b
  val foldr: item ::: Type -> b ::: Type -> (item -> b -> b) -> b -> t item -> b

  val fromList: item ::: Type -> list (key * item) -> t item
  val toList: item ::: Type -> t item -> list (key * item)
end

signature SMAP = sig
  include FMAP
  val findMinByKey: item ::: Type -> t item -> option (key * item)
  val findMaxByKey: item ::: Type -> t item -> option (key * item)
end

functor MkSortedMap(Q: sig
                         con key :: Type
                         val ord_key: ord key
                     end):(SMAP where con key = Q.key)  = struct

  structure T = AATreeMap.MkAATreeMap( Q)

  type t v = T.t v
  val empty [item]: t item = T.empty
  val singleton [item]: Q.key -> item -> t item = T.singleton
  val null [item]: t item -> bool = T.null
  val size [item]: t item -> int = T.size
  val member [item]: Q.key -> t item -> bool = T.member
  val lookup [item]: Q.key -> t item -> option item = T.lookup
  val insert [item]: Q.key -> item -> t item -> t item = T.insert
  val insertWith [item]: (item -> item -> item) -> Q.key -> item -> t item -> t item = T.insertWith
  val delete [item]: Q.key -> t item -> t item = T.delete
  val adjust [item]:  (item -> item) -> Q.key -> t item -> t item = T.adjust
  val update [item]:  (item -> option item) -> Q.key -> t item -> t item = T.update
  val exists [item]: (Q.key * item -> bool) -> t item -> bool = T.exists
  val all [item]: (Q.key * item -> bool) -> t item -> bool = T.all
  val find [item]: (Q.key * item -> bool) -> t item -> option (Q.key * item) = T.find
  val getAnyPair [item]: t item -> option (Q.key * item) = T.getAnyPair
  val foldrWithPair [item] [b]: (Q.key * item -> b -> b) -> b -> t item -> b = T.foldr
  fun foldr [item] [b] (myop: item -> b -> b): b -> t item -> b =
      let fun myop' (p: Q.key * item): b -> b = myop p.2
      in T.foldr myop'
      end 
  val mp [item] [b]: (item -> b) -> t item -> t b = T.mapValues
  val findMinByKey [item]: t item -> option (Q.key * item) = T.findMin
  val findMaxByKey [item]: t item -> option (Q.key * item) = T.findMax

  fun fromList [item] (li: list (Q.key * item)): t item = List.foldl (uncurry insert) empty li
  fun toList [item] (t1: t item): list (Q.key * item) = foldrWithPair (curry Cons) [] t1

  type key = Q.key
end


structure HStruc = Hashable.Hashable

functor MkUnordHashMap(Q: sig
                     con key :: Type
                     val eq_key: eq key
                     val hashable_key: HStruc.hashable key
                 end): (FMAP where con key = Q.key) = struct

  structure T = HashTreeMap.MkHashEqTreeMap( Q)

  type t v = T.t v
  val empty [item]: t item = T.empty
  val singleton [item]: Q.key -> item -> t item = T.singleton
  val null [item]: t item -> bool = T.null
  val size [item]: t item -> int = T.size
  val member [item]: Q.key -> t item -> bool = T.member
  val lookup [item]: Q.key -> t item -> option item = T.lookup
  val insert [item]: Q.key -> item -> t item -> t item = T.insert
  val insertWith [item]: (item -> item -> item) -> Q.key -> item -> t item -> t item = T.insertWith
  val delete [item]: Q.key -> t item -> t item = T.delete
  val adjust [item]: (item -> item) -> Q.key -> t item -> t item = T.adjust
  val update [item]:  (item -> option item) -> Q.key -> t item -> t item = T.update
  val exists [item]: (Q.key * item -> bool) -> t item -> bool = T.exists
  val all [item]: (Q.key * item -> bool) -> t item -> bool = T.all
  val find [item]: (Q.key * item -> bool) -> t item -> option (Q.key * item) = T.find
  val getAnyPair [item]: t item -> option (Q.key * item) = T.getAnyPair
  val mp [item] [b]: (item -> b) -> t item -> t b = T.mapValues
  val foldrWithPair [item] [b] : (Q.key * item -> b -> b) -> b -> t item -> b = T.foldr
  fun foldr [item] [b] (myop: item -> b -> b): b -> t item -> b =
      let fun myop' (p: Q.key * item): b -> b = myop p.2
      in T.foldr myop'
      end

  fun fromList [item] (li: list (Q.key * item)): t item = List.foldl (uncurry insert) empty li
  fun toList [item] (t1: t item): list (Q.key * item) = foldrWithPair (curry Cons) [] t1

  type key = Q.key
end

