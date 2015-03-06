(* Map *)

open HFunction
open HTuple

signature FMAP = sig
  con t :: Type -> Type
  con key :: Type
  con item :: Type
  val empty: t item
  val singleton : key -> item -> t item
  val null: t item -> bool
  val size: t item -> int
  val insert: key -> item -> t item -> t item
  val insertWith: (item -> item -> item) -> key -> item -> t item -> t item
  val delete: key -> t item -> t item
  val lookup: key -> t item -> option item
  val member: key -> t item -> bool
  val adjust:  (item -> item) -> key -> t item -> t item
  val update:  (item -> option item) -> key -> t item -> t item
  val exists: (key * item -> bool) -> t item -> bool
  val all: (key * item -> bool) -> t item -> bool
  val find: (key * item -> bool) -> t item -> option (key * item)
  val getAnyPair: t item -> option (key * item)
  val mp: b ::: Type -> (item -> b) -> t item -> t b
  val foldrWithPair: b ::: Type -> (key * item -> b -> b) -> b -> t item -> b
  val foldr: b ::: Type -> (item -> b -> b) -> b -> t item -> b
end

signature SMAP = sig
  include FMAP
  val findMinByKey: t item -> option (key * item)
  val findMaxByKey: t item -> option (key * item)
end

functor MkSortedMap(Q: sig
                         con key :: Type
                         con item :: Type
                         val ord_key: ord key
                     end):(SMAP where con item = Q.item
                                where con key = Q.key)  = struct

  structure T = AATreeMap.MkAATreeMap( Q)

  type t v = T.t Q.key v
  val empty: t Q.item = T.empty
  val singleton: Q.key -> Q.item -> t Q.item = T.singleton
  val null: t Q.item -> bool = T.null
  val size: t Q.item -> int = T.size
  val member: Q.key -> t Q.item -> bool = T.member
  val lookup: Q.key -> t Q.item -> option Q.item = T.lookup
  val insert: Q.key -> Q.item -> t Q.item -> t Q.item = T.insert
  val insertWith: (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.item -> t Q.item = T.insertWith
  val delete: Q.key -> t Q.item -> t Q.item = T.delete
  val adjust:  (Q.item -> Q.item) -> Q.key -> t Q.item -> t Q.item = T.adjust
  val update:  (Q.item -> option Q.item) -> Q.key -> t Q.item -> t Q.item = T.update
  val exists: (Q.key * Q.item -> bool) -> t Q.item -> bool = T.exists
  val all: (Q.key * Q.item -> bool) -> t Q.item -> bool = T.all
  val find: (Q.key * Q.item -> bool) -> t Q.item -> option (Q.key * Q.item) = T.find
  val getAnyPair: t Q.item -> option (Q.key * Q.item) = T.getAnyPair
  val foldrWithPair [b] : (Q.key * Q.item -> b -> b) -> b -> t Q.item -> b = T.foldr
  fun foldr [b] (myop: Q.item -> b -> b): b -> t Q.item -> b =
      let fun myop' (p: Q.key * Q.item): b -> b = myop p.2
      in T.foldr myop'
      end 
  val mp [b]: (Q.item -> b) -> t Q.item -> t b = T.mapValues
  val findMinByKey: t Q.item -> option (Q.key * Q.item) = T.findMin
  val findMaxByKey: t Q.item -> option (Q.key * Q.item) = T.findMax
  type key = Q.key
  type item = Q.item
end


structure HStruc = Hashable.Hashable

functor MkUnordHashMap(Q: sig
                     con key :: Type
                     con item :: Type
                     val eq_key: eq key
                     val hashable_key: HStruc.hashable key
                 end): (FMAP where con item = Q.item
                             where con key = Q.key) = struct

  structure T = HashTreeMap.MkHashEqTreeMap( Q)

  type t v = T.t Q.key v
  val empty: t Q.item = T.empty
  val singleton: Q.key -> Q.item -> t Q.item = T.singleton
  val null: t Q.item -> bool = T.null
  val size: t Q.item -> int = T.size
  val member: Q.key -> t Q.item -> bool = T.member
  val lookup: Q.key -> t Q.item -> option Q.item = T.lookup
  val insert: Q.key -> Q.item -> t Q.item -> t Q.item = T.insert
  val insertWith: (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.item -> t Q.item = T.insertWith
  val delete: Q.key -> t Q.item -> t Q.item = T.delete
  val adjust:  (Q.item -> Q.item) -> Q.key -> t Q.item -> t Q.item = T.adjust
  val update:  (Q.item -> option Q.item) -> Q.key -> t Q.item -> t Q.item = T.update
  val exists: (Q.key * Q.item -> bool) -> t Q.item -> bool = T.exists
  val all: (Q.key * Q.item -> bool) -> t Q.item -> bool = T.all
  val find: (Q.key * Q.item -> bool) -> t Q.item -> option (Q.key * Q.item) = T.find
  val getAnyPair: t Q.item -> option (Q.key * Q.item) = T.getAnyPair
  val mp [b]: (Q.item -> b) -> t Q.item -> t b = T.mapValues
  val foldrWithPair [b] : (Q.key * Q.item -> b -> b) -> b -> t Q.item -> b = T.foldr
  fun foldr [b] (myop: Q.item -> b -> b): b -> t Q.item -> b =
      let fun myop' (p: Q.key * Q.item): b -> b = myop p.2
      in T.foldr myop'
      end
  type key = Q.key
  type item = Q.item
end

