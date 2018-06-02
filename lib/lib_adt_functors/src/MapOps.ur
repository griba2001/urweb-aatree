(* MapOps *)

open HFunction
open HTuple
open HOrd

functor MkMapOps (M:Map.FMAP): sig

  val filter: item ::: Type -> (item -> bool) -> M.t item -> M.t item
  val filterWithKey: item ::: Type -> (M.key -> item -> bool) -> M.t item -> M.t item

  val partition: item ::: Type -> (item -> bool) -> M.t item -> M.t item * M.t item
  val partitionWithKey: item ::: Type -> (M.key -> item -> bool) -> M.t item -> M.t item * M.t item

  val filterFoldr: item ::: Type -> b ::: Type -> (item -> bool) -> (M.key * item -> b -> b) -> b -> M.t item -> b
  val filterFoldrWithKey: item ::: Type -> b ::: Type -> (M.key -> item -> bool) -> (M.key * item -> b -> b) -> b -> M.t item -> b

  val foldrWithPairPartial: item ::: Type -> b ::: Type -> (M.key * item -> b -> option b) -> b -> M.t item -> b

  val union: item ::: Type -> M.t item -> M.t item -> M.t item
  val unionWith: item ::: Type -> (item -> item -> item) -> M.t item -> M.t item -> M.t item

  val diff: item ::: Type -> M.t item -> M.t item -> M.t item

  val findMapBy: item ::: Type -> b ::: Type -> eq b -> (M.key * item -> b) -> (b -> b -> b) ->  M.t item -> option (M.key * item)
  val findByOrd: item ::: Type -> b ::: Type -> ord b -> (M.key * item -> b) -> (b -> b -> b) ->  M.t item -> option (M.key * item)

  val count: item ::: Type -> (M.key * item -> bool) -> M.t item -> int  

end = struct
      open M

        fun filterFoldr [item] [b] (prop: item -> bool) (myop: key * item -> b -> b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        if prop p.2
                                then myop p acc
                                else acc
                in foldrWithPair myop' z st
                end

        fun filterFoldrWithKey [item] [b] (prop: key -> item -> bool) (myop: key * item -> b -> b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        if uncurry prop p
                                then myop p acc
                                else acc
                in foldrWithPair myop' z st
                end

        fun foldrWithPairPartial [item] [b] (myop: key * item -> b -> option b) (z: b) (st: t item): b =
                let fun myop' (p: key * item) (acc: b): b =
                        case myop p acc of
                            Some res => res
                            | None => acc
                in foldrWithPair myop' z st
                end

        fun filter [item] (prop: item -> bool) : (t item -> t item) =

                filterFoldr prop (uncurry insert) empty

        fun filterWithKey [item] (prop: key -> item -> bool) : (t item -> t item) =

                filterFoldrWithKey prop (uncurry insert) empty


        fun partition [item] (prop: item -> bool) : (t item -> t item * t item) =
                let
                        fun myop (p: key * item) (acc: t item * t item): t item * t item =
                                if prop p.2
                                then (uncurry insert p acc.1, acc.2)
                                else (acc.1, uncurry insert p acc.2)
                in
                        foldrWithPair myop (empty, empty)
                end

        fun partitionWithKey [item] (prop: key -> item -> bool) : (t item -> t item * t item) =
                let
                        fun myop (p: key * item) (acc: t item * t item): t item * t item =
                                if uncurry prop p
                                then (uncurry insert p acc.1, acc.2)
                                else (acc.1, uncurry insert p acc.2)
                in
                        foldrWithPair myop (empty, empty)
                end

        fun union [item] (m1: t item) (m2: t item): t item = foldrWithPair (uncurry insert) m2 m1

        fun unionWith [item] (f: item -> item -> item) (m1: t item) (m2: t item): t item = foldrWithPair (uncurry (insertWith f)) m2 m1

        fun diff [item] (m1: t item) (m2: t item): t item = foldrWithPair (fst >>> delete) m1 m2

        fun findMapBy [item] [b] (_: eq b) (proj: key * item -> b) (f: b -> b -> b) (d1: t item): option (key * item) =

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

        fun findByOrd [item] [b] (_: ord b) (proj: key * item -> b) (f: b -> b -> b) (d1: t item): option (key * item) =

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

        fun count [item] (prop: key * item -> bool) (t1: t item): int =
                let
                      foldrWithPair incrOnProp 0 t1
                where
                   fun incrOnProp (pair: key * item) (acc: int): int = 
                          (if prop pair then acc + 1 
                                       else acc)
                end

end
