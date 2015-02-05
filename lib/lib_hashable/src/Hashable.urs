(* Hashable *)

structure Hashable : sig
        class hashable :: Type -> Type
        val mkHashable : a ::: Type -> (int -> a -> int) -> hashable a
        val hashWithSalt : a:::Type -> hashable a -> (int -> a -> int)
        val hash : a:::Type -> hashable a -> a -> int
end

val hashable_int: Hashable.hashable int

val hashable_float: Hashable.hashable float

val hashable_char: Hashable.hashable char

val hashable_list: a ::: Type -> Hashable.hashable a -> Hashable.hashable (list a)

val hashable_string: Hashable.hashable string

val hashable_option: a ::: Type -> Hashable.hashable a -> Hashable.hashable (option a)

val hashable_pair: a ::: Type -> b ::: Type -> Hashable.hashable a -> Hashable.hashable b -> Hashable.hashable (a * b)