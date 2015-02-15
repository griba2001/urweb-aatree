(* SetOps *)

open HFunction
open HOrd

functor MkSetOps (S:Set.FSET): sig

  val filter: (S.item -> bool) -> S.t -> S.t
  val partition: (S.item -> bool) -> S.t -> S.t * S.t

  val union: S.t -> S.t -> S.t
  val diff: S.t -> S.t -> S.t
  val intersect: S.t -> S.t -> S.t

  val findByOrd: b ::: Type -> ord b -> (S.item -> b) -> (b -> b -> b) ->  S.t -> option S.item
end = struct
      open S

        fun filterFoldr [b] (prop: item -> bool) (myop: item -> b -> b) (z: b) (st: t): b =
                let fun myop' (x: item) (acc: b): b =
                        if prop x
                                then myop x acc
                                else acc
                in foldr myop' z st
                end


        fun filter (prop: item -> bool) : (t -> t) =

                filterFoldr prop insert empty

        fun partition (prop: item -> bool) : (t -> t * t) =
                let
                        fun myop (x: item) (pair: t * t): t * t =
                                if prop x then (insert x pair.1, pair.2)
                                else (pair.1, insert x pair.2)
                in
                        foldr myop (empty, empty)
                end

        fun union (s1: t) (s2: t): t = foldr insert s2 s1

        fun diff (s1: t) (s2: t): t = foldr delete s1 s2

        fun intersect (s1: t) (s2: t): t =
                let
                        val memberOf = flip member
                in
                        filterFoldr (memberOf s1) insert empty s2
                end

        (* findByOrd example: findMin for HashedEqSets with ord item
                 (findMin = findByOrd id min)
         *)

        fun findByOrd [b] (_: ord b) (proj: item -> b) (f: b -> b -> b) (d1: t): option item =

                let val optZ : option item = getAny d1

                fun myop (x: item) (acc: item): item =
                        let val acc_proj = proj acc
                                val x_proj = proj x
                        in if compare (f acc_proj x_proj) acc_proj = EQ then acc else x
                        end
                in
                case optZ of
                        None => None
                        | Some z => Some (foldr myop z d1)
                end

end
