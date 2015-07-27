
signature HASHTREE_MAP = sig
  include Common.DSMAP
  val maxBucketSize: item ::: Type -> t item -> int
end


functor MkHashEqTreeMap(Q: sig
                         con key :: Type
                         val hashable_key: Hashable.Hashable.hashable key
                         val eq_key: eq key
                      end): HASHTREE_MAP where con key = Q.key


functor MkHashOrdTreeMap(Q: sig
                         con key :: Type
                         val hashable_key: Hashable.Hashable.hashable key
                         val ord_key: ord key
                      end): HASHTREE_MAP where con key = Q.key