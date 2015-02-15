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
  val delete: key -> t item -> t item
  val lookup: key -> t item -> option item
  val member: key -> t item -> bool
  val exists: (key * item -> bool) -> t item -> bool
  val all: (key * item -> bool) -> t item -> bool
  val find: (key * item -> bool) -> t item -> option (key * item)
  val getAnyPair: t item -> option (key * item)
  val mp: b ::: Type -> (item -> b) -> t item -> t b
  val foldrWithIndex: b ::: Type -> (key * item -> b -> b) -> b -> t item -> b
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
                     end):SMAP  = struct

  structure T = AATreeMap.MkAATreeMap( Q)
  open Q
  
  type t v = T.tree key v
  type key = key
  type item = item
  val empty: t item = T.empty
  val singleton: key -> item -> t item = T.singleton
  val null: t item -> bool = T.null
  val size: t item -> int = T.size
  val member: key -> t item -> bool = T.member
  val lookup: key -> t item -> option item = T.lookup
  val insert: key -> item -> t item -> t item = T.insert
  val delete: key -> t item -> t item = T.delete
  val exists: (key * item -> bool) -> t item -> bool = T.exists
  val all: (key * item -> bool) -> t item -> bool = T.all
  val find: (key * item -> bool) -> t item -> option (key * item) = T.find
  val getAnyPair: t item -> option (key * item) = T.getAnyPair
  val foldrWithIndex [b] : (key * item -> b -> b) -> b -> t item -> b = T.foldr
  fun foldr [b] (myop: item -> b -> b): b -> t item -> b =
      let fun myop' (p: key * item): b -> b = myop p.2
      in T.foldr myop'
      end 
  val mp [b]: (item -> b) -> t item -> t b = T.mapValues
  val findMinByKey: t item -> option (key * item) = T.findMin
  val findMaxByKey: t item -> option (key * item) = T.findMax
end


structure HStruc = Hashable.Hashable

functor MkUnordHashMap(Q: sig
                     con key :: Type
                     con item :: Type
                     val eq_key: eq key
                     val hashable_key: HStruc.hashable key
                 end): FMAP = struct

  structure T = HashEqTreeMap.MkHashEqTreeMap( Q)

  open Q

  type t v = T.htree key v
  type key = key
  type item = item
  val empty: t item = T.empty
  val singleton: key -> item -> t item = T.singleton
  val null: t item -> bool = T.null
  val size: t item -> int = T.size
  val member: key -> t item -> bool = T.member
  val lookup: key -> t item -> option item = T.lookup
  val insert: key -> item -> t item -> t item = T.insert
  val delete: key -> t item -> t item = T.delete
  val exists: (key * item -> bool) -> t item -> bool = T.exists
  val all: (key * item -> bool) -> t item -> bool = T.all
  val find: (key * item -> bool) -> t item -> option (key * item) = T.find
  val getAnyPair: t item -> option (key * item) = T.getAnyPair
  val mp [b]: (item -> b) -> t item -> t b = T.mapValues
  val foldrWithIndex [b] : (key * item -> b -> b) -> b -> t item -> b = T.foldr
  fun foldr [b] (myop: item -> b -> b): b -> t item -> b =
      let fun myop' (p: key * item): b -> b = myop p.2
      in T.foldr myop'
      end
end

