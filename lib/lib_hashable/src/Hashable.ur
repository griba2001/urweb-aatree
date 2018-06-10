(* Hashable (code borrowed from Haskell Data.Hashable) *)

structure B = Bits
structure F = HFunction
structure HS = HString

structure Util = struct

(* Haskell's defaultSalt: if B.wordSize = 64 then 0xdc36d1615b7400a4 else 0x087fc72c *)
val defaultSalt : int = -2578643520546668380 (* Basis.int is a long long *)

(*
fun defaultHashWithSalt [a] (_:hashable a) (salt: int) (x: a): a = combine salt (hash x)
*)

fun combine (h1: int) (h2: int):int = B.xorb (h1 * 16777619) h2

val distinguisher: int = 6148914691236517205 (* 0x5555555555555555 *)
end

structure HashableClass : sig
        class hashable :: Type -> Type
        val mkHashable : a ::: Type -> (int -> a -> int) -> hashable a
        val hashWithSalt : a:::Type -> hashable a -> (int -> a -> int)
        val hash : a:::Type -> hashable a -> a -> int   
end = struct
        open Util

        type hashable a = (int -> a -> int)
        fun mkHashable [a] (f: (int -> a -> int)): hashable a = f
        fun hashWithSalt [a] (h: hashable a): (int -> a -> int) = h
        fun hash [a] (h: hashable a): (a -> int) = h defaultSalt
end


(* instances *)

structure HashableInstances = struct

open HashableClass
open Util

val hashable_int: hashable int = mkHashable combine

val hashable_float: hashable float = 
    let fun hashWithSalt' (salt: int) (x: float):int = hashWithSalt salt (B.floatAsWord x)
    in mkHashable hashWithSalt'
    end

val hashable_char: hashable char =
    let fun hashWithSalt' (salt: int) (x: char):int = hashWithSalt salt (ord x)
    in mkHashable hashWithSalt'
    end

val hashable_list [a] (_:hashable a): hashable (list a) =
     let fun hashWithSalt' (salt: int) (xs: list a): int =
          List.foldl (F.flip hashWithSalt) salt xs
     in mkHashable hashWithSalt'
     end

val hashable_string: hashable string =
     let fun hashWithSalt' (salt: int) (xs: string): int =
          HS.foldr (F.flip hashWithSalt) salt xs
     in mkHashable hashWithSalt'
     end

val hashable_option [a] (_:hashable a): hashable (option a) =

     let fun hashWithSalt' (salt: int) (optA: option a): int =
          case optA of
             None => combine salt 0
             | Some x => combine salt (hashWithSalt distinguisher x)     (* s `combine` distinguisher `hashWithSalt` a *)

     in mkHashable hashWithSalt'
     end

val hashable_pair [a][b] (_:hashable a) (_:hashable b): hashable (a * b) =

     let fun hashWithSalt' (salt: int) (p: a * b): int = hashWithSalt (hashWithSalt salt p.1) p.2
     in mkHashable hashWithSalt'
     end
end