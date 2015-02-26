(* MapOps *)

open HFunction
open HTuple
open HOrd

functor MkMapOps (M:Map.FMAP): sig

  val filter: (M.item -> bool) -> M.t M.item -> M.t M.item
  val filterWithKey: (M.key -> M.item -> bool) -> M.t M.item -> M.t M.item

  val partition: (M.item -> bool) -> M.t M.item -> M.t M.item * M.t M.item
  val partitionWithKey: (M.key -> M.item -> bool) -> M.t M.item -> M.t M.item * M.t M.item

  val filterFoldr: b ::: Type -> (M.item -> bool) -> (M.key * M.item -> b -> b) -> b -> M.t M.item -> b
  val filterFoldrWithKey: b ::: Type -> (M.key -> M.item -> bool) -> (M.key * M.item -> b -> b) -> b -> M.t M.item -> b

  val foldrWithPairPartial: b ::: Type -> (M.key * M.item -> b -> option b) -> b -> M.t M.item -> b

  val union: M.t M.item -> M.t M.item -> M.t M.item
  val unionWith: (M.item -> M.item -> M.item) -> M.t M.item -> M.t M.item -> M.t M.item

  val diff: M.t M.item -> M.t M.item -> M.t M.item

  val findMapBy: b ::: Type -> eq b -> (M.key * M.item -> b) -> (b -> b -> b) ->  M.t M.item -> option (M.key * M.item)
  val findByOrd: b ::: Type -> ord b -> (M.key * M.item -> b) -> (b -> b -> b) ->  M.t M.item -> option (M.key * M.item)

end = struct
      open M

        fun filterFoldr [b] (prop: item -> bool) (myop: key * item -> b -> b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        if prop p.2
                                then myop p acc
                                else acc
                in foldrWithPair myop' z st
                end

        fun filterFoldrWithKey [b] (prop: key -> item -> bool) (myop: key * item -> b -> b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        if uncurry prop p
                                then myop p acc
                                else acc
                in foldrWithPair myop' z st
                end

        fun foldrWithPairPartial [b] (myop: key * item -> b -> option b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        case myop p acc of
                            Some res => res
                            | None => acc
                in foldrWithPair myop' z st
                end

        fun filter (prop: item -> bool) : (t item -> t item) =

                filterFoldr prop (uncurry insert) empty

        fun filterWithKey (prop: key -> item -> bool) : (t item -> t item) =

                filterFoldrWithKey prop (uncurry insert) empty


        fun partition (prop: item -> bool) : (t item -> t item * t item) =
                let
                        fun myop (p: key * item) (acc: t item * t item): t item * t item =
                                if prop p.2
                                then (uncurry insert p acc.1, acc.2)
                                else (acc.1, uncurry insert p acc.2)
                in
                        foldrWithPair myop (empty, empty)
                end

        fun partitionWithKey (prop: key -> item -> bool) : (t item -> t item * t item) =
                let
                        fun myop (p: key * item) (acc: t item * t item): t item * t item =
                                if uncurry prop p
                                then (uncurry insert p acc.1, acc.2)
                                else (acc.1, uncurry insert p acc.2)
                in
                        foldrWithPair myop (empty, empty)
                end

        fun union (m1: t item) (m2: t item): t item = foldrWithPair (uncurry insert) m2 m1

        fun unionWith (f: item -> item -> item) (m1: t item) (m2: t item): t item = foldrWithPair (uncurry (insertWith f)) m2 m1

        fun diff (m1: t item) (m2: t item): t item = foldrWithPair (compose delete fst) m1 m2

        fun findMapBy [b] (_: eq b) (proj: key * item -> b) (f: b -> b -> b) (d1: t item): option (key * item) =

                let val optZ : option (key * item) = getAnyPair d1

                fun myop (x: key * item) (acc: key * item): key * item =
                        let val acc_p = proj acc
                        in if f acc_p (proj x) = acc_p then acc else x
                        end
                in
                case optZ of
                        None => None
                        | Some z => Some (foldrWithPair myop z d1)
                end

        (* findByOrd example: (findMinByValue = findByOrd snd min)
         *)

        fun findByOrd [b] (_: ord b) (proj: key * item -> b) (f: b -> b -> b) (d1: t item): option (key * item) =

                let val optZ : option (key * item) = getAnyPair d1

                fun myop (x: key * item) (acc: key * item): key * item =
                        let val acc_p = proj acc
                        in if compare (f acc_p (proj x)) acc_p = EQ then acc else x
                        end
                in
                case optZ of
                        None => None
                        | Some z => Some (foldrWithPair myop z d1)
                end

end
