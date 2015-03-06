functor MkHashEqTreeMap(Q: sig
                         con key :: Type
                         con item :: Type
                         val hashable_key: Hashable.Hashable.hashable key
                         val eq_key: eq key
                      end) : sig

        con t :: Type -> Type -> Type

        val empty :  t Q.key Q.item

        val singleton :  Q.key -> Q.item -> t Q.key Q.item

        val null :  t Q.key Q.item -> bool

        val size :  t Q.key Q.item -> int

        val lookup:  Q.key -> t Q.key Q.item -> option Q.item

        val member:  Q.key -> t Q.key Q.item -> bool

        val getAnyPair :  t Q.key Q.item -> option (Q.key * Q.item)

        val insert:  Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

        val insertWith:  (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

        val fromList :  list (Q.key * Q.item) -> t Q.key Q.item

        val delete:  Q.key -> t Q.key Q.item -> t Q.key Q.item

        val adjust:  (Q.item -> Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

        val update:  (Q.item -> option Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

        val mapValues : b ::: Type -> (Q.item -> b) -> t Q.key Q.item -> t Q.key b

        val foldr:  b ::: Type -> (Q.key * Q.item -> b -> b) -> b -> t Q.key Q.item -> b

        val toList :  t Q.key Q.item -> list (Q.key * Q.item)

        val exists : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

        val all : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

        val find : (Q.key * Q.item -> bool) -> t Q.key Q.item -> option (Q.key * Q.item)

        val valid :  t Q.key Q.item -> bool

        val maxBucketSize: t Q.key Q.item -> int
end

functor MkHashOrdTreeMap(Q: sig
                         con key :: Type
                         con item :: Type
                         val hashable_key: Hashable.Hashable.hashable key
                         val ord_key: ord key
                      end): sig

        con t :: Type -> Type -> Type

        val empty :  t Q.key Q.item

        val singleton :  Q.key -> Q.item -> t Q.key Q.item

        val null :  t Q.key Q.item -> bool

        val size :  t Q.key Q.item -> int

        val lookup:  Q.key -> t Q.key Q.item -> option Q.item

        val member:  Q.key -> t Q.key Q.item -> bool

        val getAnyPair :  t Q.key Q.item -> option (Q.key * Q.item)

        val insert:  Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

        val insertWith:  (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

        val fromList :  list (Q.key * Q.item) -> t Q.key Q.item

        val delete:  Q.key -> t Q.key Q.item -> t Q.key Q.item

        val adjust:  (Q.item -> Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

        val update:  (Q.item -> option Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

        val mapValues : b ::: Type -> (Q.item -> b) -> t Q.key Q.item -> t Q.key b

        val foldr:  b ::: Type -> (Q.key * Q.item -> b -> b) -> b -> t Q.key Q.item -> b

        val toList :  t Q.key Q.item -> list (Q.key * Q.item)

        val exists : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

        val all : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

        val find : (Q.key * Q.item -> bool) -> t Q.key Q.item -> option (Q.key * Q.item)

        val valid :  t Q.key Q.item -> bool

        val maxBucketSize: t Q.key Q.item -> int
end