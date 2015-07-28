(* HMonad *)

val xunless : m ::: (Type -> Type) -> monad m -> bool -> m xbody -> m xbody

val xwhen : m ::: (Type -> Type) -> monad m -> bool -> m xbody -> m xbody

val unfoldrM : m ::: (Type -> Type) -> a ::: Type -> b ::: Type -> monad m -> (b -> m (option (a * b))) -> b -> m (list a)

(*
val unfoldrOptionM : m ::: (Type -> Type) -> a ::: Type -> b ::: Type -> monad m -> (b -> m (option (option a * b))) -> b -> m (list a)
*)
