functor MkHashEqTreeMap(Q: sig
                         con key :: Type
                         val hashable_key: Hashable.Hashable.hashable key
                         val eq_key: eq key
                      end): sig

        con t :: Type -> Type

        val empty : item ::: Type -> t item

        val singleton : item ::: Type -> Q.key -> item -> t item

        val null : item ::: Type -> t item -> bool

        val size : item ::: Type -> t item -> int

        val lookup: item ::: Type -> Q.key -> t item -> option item

        val member: item ::: Type -> Q.key -> t item -> bool

        val getAnyPair: item ::: Type -> t item -> option (Q.key * item)

        val insert: item ::: Type -> Q.key -> item -> t item -> t item

        val insertWith: item ::: Type -> (item -> item -> item) -> Q.key -> item -> t item -> t item

        val fromList: item ::: Type -> list (Q.key * item) -> t item

        val delete: item ::: Type -> Q.key -> t item -> t item

        val adjust: item ::: Type -> (item -> item) -> Q.key -> t item -> t item

        val update: item ::: Type -> (item -> option item) -> Q.key -> t item -> t item

        val mapValues : item ::: Type -> b ::: Type -> (item -> b) -> t item -> t b

        val foldr: item ::: Type -> b ::: Type -> (Q.key * item -> b -> b) -> b -> t item -> b

        val toList : item ::: Type -> t item -> list (Q.key * item)

        val exists : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

        val all : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

        val find : item ::: Type -> (Q.key * item -> bool) -> t item -> option (Q.key * item)

        val valid : item ::: Type ->  t item -> bool

        val maxBucketSize: item ::: Type -> t item -> int

end


functor MkHashOrdTreeMap(Q: sig
                         con key :: Type
                         val hashable_key: Hashable.Hashable.hashable key
                         val ord_key: ord key
                      end): sig

        con t :: Type -> Type

        val empty : item ::: Type -> t item

        val singleton : item ::: Type -> Q.key -> item -> t item

        val null : item ::: Type -> t item -> bool

        val size : item ::: Type -> t item -> int

        val lookup: item ::: Type -> Q.key -> t item -> option item

        val member: item ::: Type -> Q.key -> t item -> bool

        val getAnyPair: item ::: Type -> t item -> option (Q.key * item)

        val insert: item ::: Type -> Q.key -> item -> t item -> t item

        val insertWith: item ::: Type -> (item -> item -> item) -> Q.key -> item -> t item -> t item

        val fromList: item ::: Type -> list (Q.key * item) -> t item

        val delete: item ::: Type -> Q.key -> t item -> t item

        val adjust: item ::: Type -> (item -> item) -> Q.key -> t item -> t item

        val update: item ::: Type -> (item -> option item) -> Q.key -> t item -> t item

        val mapValues : item ::: Type -> b ::: Type -> (item -> b) -> t item -> t b

        val foldr: item ::: Type -> b ::: Type -> (Q.key * item -> b -> b) -> b -> t item -> b

        val toList : item ::: Type -> t item -> list (Q.key * item)

        val exists : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

        val all : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

        val find : item ::: Type -> (Q.key * item -> bool) -> t item -> option (Q.key * item)

        val valid : item ::: Type ->  t item -> bool

        val maxBucketSize: item ::: Type -> t item -> int

end
