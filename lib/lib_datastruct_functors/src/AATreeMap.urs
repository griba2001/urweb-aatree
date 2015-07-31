signature AA_TREE_MAP = sig
  include Common.DSMAP

  val findMin : item ::: Type -> t item -> option (key * item)
  val findMax : item ::: Type -> t item -> option (key * item)

  val propBST : item ::: Type -> t item -> bool
  val aaTreeProps : item ::: Type -> t item -> bool
end

functor MkAATreeMap(Q: sig
                         con key :: Type
                         val ord_key: ord key
end): (AA_TREE_MAP where con key = Q.key)