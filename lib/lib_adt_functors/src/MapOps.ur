(* MapOps *)

open HFunction
open HTuple
open HOrd

functor MkMapOps (M:Map.FMAP): sig

  val filter: (M.item -> bool) -> M.t M.item -> M.t M.item
  val partition: (M.item -> bool) -> M.t M.item -> M.t M.item * M.t M.item


  val union: M.t M.item -> M.t M.item -> M.t M.item
  val diff: M.t M.item -> M.t M.item -> M.t M.item
  val findByOrd: b ::: Type -> ord b -> (M.key * M.item -> b) -> (b -> b -> b) ->  M.t M.item -> option (M.key * M.item)

end = struct
      open M

        fun filterFoldr [b] (prop: item -> bool) (myop: key * item -> b -> b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        if prop p.2
                                then myop p acc
                                else acc
                in foldrWithIndex myop' z st
                end


        fun filter (prop: item -> bool) : (t item -> t item) =

                filterFoldr prop (uncurry insert) empty

        fun partition (prop: item -> bool) : (t item -> t item * t item) =
                let
                        fun myop (p: key * item) (acc: t item * t item): t item * t item =
                                if prop p.2
                                then (uncurry insert p acc.1, acc.2)
                                else (acc.1, uncurry insert p acc.2)
                in
                        foldrWithIndex myop (empty, empty)
                end

        fun union (m1: t item) (m2: t item): t item = foldrWithIndex (uncurry insert) m2 m1

        fun diff (m1: t item) (m2: t item): t item = foldrWithIndex (compose delete fst) m1 m2

        (* findByOrd example: (findMinByValue = findByOrd snd min)
         *)

        fun findByOrd [b] (_: ord b) (proj: key * item -> b) (f: b -> b -> b) (d1: t item): option (key * item) =

                let val optZ : option (key * item) = getAnyPair d1

                fun myop (pair: key * item) (acc: key * item): key * item =
                        let val acc_p = proj acc
                                val pair_p = proj pair
                        in if compare (f acc_p pair_p) acc_p = EQ then acc else pair
                        end
                in
                case optZ of
                        None => None
                        | Some z => Some (foldrWithIndex myop z d1)
                end

end
