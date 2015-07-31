signature LIST_MAP = sig
  include Common.DSMAP
  val propBucketKeysAreUnique: item ::: Type -> t item -> bool
end

functor MkListMap(Q: sig
                         con key :: Type
                         val eq_key: eq key
end): (LIST_MAP where con key = Q.key)