(* Arne Anderson Tree *)


(*
*)

functor MkAATreeMap(Q: sig
                         con key :: Type
                         val ord_key: ord key
end):sig

con t :: Type -> Type

val empty : item ::: Type -> t item

val singleton : item ::: Type -> Q.key -> item -> t item

val null : item ::: Type -> t item -> bool

val size : item ::: Type -> t item -> int

val lookup: item ::: Type -> Q.key -> t item -> option item

val member: item ::: Type -> Q.key -> t item -> bool

val findMin : item ::: Type -> t item -> option (Q.key * item)

val findMax : item ::: Type -> t item -> option (Q.key * item)

val getAnyPair : item ::: Type -> t item -> option (Q.key * item)

val insert: item ::: Type -> Q.key -> item -> t item -> t item

val insertWith: item ::: Type -> (item -> item -> item) -> Q.key -> item -> t item -> t item

val fromList : item ::: Type -> list (Q.key * item) -> t item

val delete: item ::: Type -> Q.key -> t item -> t item

val adjust: item ::: Type -> (item -> item) -> Q.key -> t item -> t item

val update: item ::: Type -> (item -> option item) -> Q.key -> t item -> t item

val mapValues: item ::: Type -> b ::: Type -> (item -> b) -> t item -> t b

val foldr: item ::: Type -> b ::: Type -> (Q.key * item -> b -> b) -> b -> t item -> b

val toList : item ::: Type -> t item -> list (Q.key * item)

val exists : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

val all : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

val find: item ::: Type -> (Q.key * item -> bool) -> t item -> option (Q.key * item)

(* * Invariants *)

val propBST : item ::: Type -> t item -> bool

val aaTreeProps : item ::: Type -> t item -> bool

val valid : item ::: Type -> t item -> bool

end = struct

open Q

open HFunction
open HTuple
open HOrd
open Option
structure HS = HString
structure HL = HList
structure HO = HOption
structure HR = HRecord


datatype t v = Empty | Node of {Key: Q.key,
                                Value: v,
                                Level: int,
                                Left: t v,
                                Right: t v}

(* * Instances *)


val eq_tree [item] = fn (_ : eq Q.key) (_ : eq item) =>
        let
                fun eq' (t1: t item) (t2: t item) =
                   case (t1, t2) of
                        | (Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...},
                           Node {Key = k2, Value = v2, Left = lt2, Right = rt2, ...}) =>
                                  k1 = k2 && v1 = v2 && eq' lt1 lt2 && eq' rt1 rt2
                        | (Empty, Empty) => True
                        | _ => False
        in mkEq eq'
        end


val show_tree [item] = fn (_ : show Q.key) (_ : show item) =>
        let
           fun show' (t1: t item): string =
              case t1 of
                | Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...} =>
                    HS.concat ("[" :: show' lt1 :: "," :: show (k1, v1) :: "," :: show' rt1 :: "]" :: [])
                | Empty => "Empty"
        in mkShow show'
        end
                
(* * Setters / Getters *)

fun setValue [item] (v1: item) (t1: t item): t item =
    case t1 of
        Node r => Node (HR.overwrite r {Value = v1})
        | _ => error <xml>setValue: not a Node</xml>

fun setKeyAndValue [item] (k1: Q.key) (v1: item) (t1: t item) : t item =
    case t1 of
        Node r => Node (HR.overwrite r {Key = k1, Value = v1})
        | _ => error <xml>setKeyAndValue: not a Node</xml>

fun setLevel [item] (v1: int) (t1: t item) : t item =
    case t1 of
        Node r => Node (HR.overwrite r {Level = v1})
        | _ => error <xml>setLevel: not a Node</xml>

fun setLeft [item] (v1: t item) (t1: t item) : t item =
    case t1 of
        Node r => Node (HR.overwrite r {Left = v1})
        | _ => error <xml>setLeft: not a Node</xml>

fun setRight [item] (v1: t item) (t1: t item) : t item =
    case t1 of
        Node r => Node (HR.overwrite r {Right = v1})
        | _ => error <xml>setRight: not a Node</xml>

fun getLevel [item] (t1: t item) : int =
   case t1 of
     | Node {Level = lvl, ...} => lvl
     | Empty => 0

(* * Construction *)

val empty [item] : t item = Empty

fun singleton [item] (k1: Q.key) (v1: item): t item = Node {Key = k1, Value = v1, Level = 1, Left = Empty, Right = Empty}

(* * Query *)

fun null [item] (t1: t item): bool =
    case t1 of
        Empty => True
        | _ => False

fun size [item] (t1: t item) : int =
    case t1 of
     | Node {Left = l, Right = r, ...} => 1 + size l + size r
     | Empty => 0


fun lookup [item] (k1: Q.key) (t1: t item): option item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
            (case compare k1 k0 of
                EQ => Some v0
                | LT => lookup k1 l
                | GT => lookup k1 r
                )
        | Empty => None

val member [item] (k1: Q.key): (t item -> bool) = lookup k1 >>> isSome

(* get root pair to start minimum / maximum value folds *)
fun getAnyPair [item] (t1: t item): option (Q.key * item) =
    case t1 of
      Empty => None
      | Node {Key = key, Value = item, ...} => Some (key, item)

(* minimum, maximum to be used in deletes
*)

fun findMin [item] (t1: t item): option (key * item) =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, ...} =>
            (case l: t item of
               Empty => Some (k0, v0)
               | _ => findMin l
               )
        | Empty => None

fun minimum [item] (t1: t item): Q.key * item =
    case findMin t1 of
        Some x => x
        | None => error <xml>aatree minimum: empty tree</xml>

fun findMax [item] (t1: t item): option (key * item) =
    case t1 of
        Node {Key = k0, Value = v0, Right = r, ...} =>
            (case r: t item of
               Empty => Some (k0, v0)
               | _ => findMax r
               )
        | Empty => None

fun maximum [item] (t1: t item): Q.key * item =
    case findMax t1 of
        Some x => x
        | None => error <xml>aatree maximum: empty tree</xml>


(* * Node balancing *)

(* skew: remove left horizontal links with right rotation
*)
fun skew [item] (t1: t item) : t item =
    case t1 of
        Node {Level = lvT, Left = l, ...} =>
            (case l of
               Node {Level = lvL, Right = lRight, ...} =>
                        if lvT = lvL
                        then setRight (setLeft lRight t1) l
                        else t1
               | _ => t1
               )
        | _ => t1

(* split: remove consecutive horizontal links
*)

fun split [item] (t1: t item) : t item =
    case t1 of
      Node {Level = lvT, Right = r, ...} =>
        (case r of
          Node {Level = lvR, Left = rLeft, Right = s, ...} =>
            (case s of
              Node {Level = lvS, ...} =>
                if (lvT = lvS)
                  then setLevel (lvR +1) (setLeft (setRight rLeft t1) r)
                  else t1
              | _ => t1)
          | _ => t1)
     | _ => t1

(*
*)

fun splitRight [item] (t1: t item): t item =
    case t1 of
        | Node {Right = r, ...} =>
           (case r of
             | Node {...} => setRight (split r) t1
             | Empty => t1
             )
        | Empty => t1

(*
*)

fun skewRight [item] (t1: t item): t item =
    case t1 of
        Node {Right = r, ...} =>
           (case r of
              Node {...} => setRight (skew r) t1
              | Empty => t1
              )
        | Empty => t1

(*
*)

fun skewRightRight [item] (t1: t item): t item =
    case t1 of
        Node {Right = r, ...} =>
          (case r of
             Node {Right = s, ...} =>
               (case s of
                  Node {...} => let val r' : t item = setRight (skew s) r
                                  in setRight r' t1
                                  end
                  | Empty => t1
                  )
             | _ => t1
             )
       | _ => t1

(*
*)

fun decreaseLevel [item] (t1: t item): t item =
    case t1 of
      Node {Level = lvP, Left = l, Right = r, ...} =>
          (case r of
            Node {Level = lvR, ...} =>
                     let val should_be = 1 + min (getLevel l) (getLevel r)
                     in if lvP > should_be
                        then let val r' : t item = if lvR > should_be
                                            then setLevel should_be r
                                            else r
                                in setRight r' (setLevel should_be t1)
                                end
                        else t1
                     end
            | Empty => let val should_be = 1 + getLevel l
                     in if lvP > should_be
                           then setLevel should_be t1
                           else t1
                     end
            )  
      | Empty => Empty

(*
*)
val rebalance [item] : (t item -> t item) = (* with left to right function composition *)
    decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight

val skewThenSplit [item] : (t item -> t item) = skew >>> split

(* * Insert / delete  *)

(*
*)


fun insertWith [item] (f: item -> item -> item) (k1: Q.key) (v1: item) (t1: t item): t item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => skewThenSplit (setLeft (insertWith f k1 v1 l) t1)
              | GT => skewThenSplit (setRight (insertWith f k1 v1 r) t1)
              | EQ => setKeyAndValue k1 (f v1 v0) t1
              )
        | Empty => singleton k1 v1

val insert [item] (k1: Q.key) (v1: item):  (t item -> t item) = insertWith const k1 v1


(*
*)

fun delete [item] (k1: Q.key) (t1: t item): t item =
    case t1 of
        Node {Key = k0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => rebalance (setLeft (delete k1 l) t1)
              | GT => rebalance (setRight (delete k1 r) t1)
              | EQ => (case (l, r) of
                         (Empty, Empty) => Empty  (* deleted *)
                         | (Empty, _) => let val (succK, succV) = minimum r
                                       in rebalance (setKeyAndValue succK succV (setRight (delete succK r) t1))
                                       end
                         | (_, _) => let val (predK, predV) = maximum l
                                   in rebalance (setKeyAndValue predK predV (setLeft (delete predK l) t1))
                                   end
                         )
              )
        | Empty => Empty

(* * Folding *)

fun foldr' [item] [b] (op: Q.key * item -> b -> b) (t1: t item) (acc: b): b =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
          (case (l: t item, r: t item) of
                (Empty, Empty) => op (k0, v0)  acc
                | _ =>  foldr' op l (op (k0, v0) (foldr' op r acc))
                )
      | Empty => acc

fun foldr [item] [b] (op: Q.key * item -> b -> b) (acc: b) (t1: t item): b = foldr' op t1 acc

fun toList [item] (t1: t item): list (key * item) = foldr' (curry Cons) t1 []

fun fromList [item] (li: list (key * item)): t item = List.foldl (uncurry insert) empty li


(* * Adjust and mapping *)

fun adjust' [item] (f: item -> item) (k1: Q.key) (t1: t item): t item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => setLeft (adjust' f k1 l) t1
              | GT => setRight (adjust' f k1 r) t1
              | EQ => setValue (f v0) t1
              )
        | Empty => t1 (* case unreached if Q.key non-membership is filtered out *)

fun adjust [item] (f: item -> item) (k1: Q.key) (t1: t item): t item =
    if member k1 t1
       then adjust' f k1 t1
       else t1

fun update' [item] (f: item -> option item) (k1: Q.key) (t1: t item): t item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => rebalance (setLeft (update' f k1 l) t1)
              | GT => rebalance (setRight (update' f k1 r) t1)
              | EQ => (case f v0 of
                        | Some v1 => setValue v1 t1
                        | None => delete k1 t1
                        )  
              )
        | Empty => t1 (* case unreached if Q.key non-membership is filtered out *)

fun update [item] (f: item -> option item) (k1: Q.key) (t1: t item): t item =
    if member k1 t1
       then update' f k1 t1
       else t1

fun mapValues [item] [w] (f: item -> w) (t1: t item): t w =
       case t1 of
         Node rc => Node (HR.overwrite rc {Value = f rc.Value,
                                           Left = mapValues f rc.Left,
                                           Right = mapValues f rc.Right})
         | Empty => Empty

(* short-circuiting exists *)
fun exists [item] (prop: Q.key * item -> bool) (t1: t item): bool =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
                  prop (k0, v0) || exists prop l || exists prop r 
      | Empty => False

(* short-circuiting all *)
fun all [item] (prop: Q.key * item -> bool) (t1: t item): bool =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
                  prop (k0, v0) && all prop l && all prop r
      | Empty => True

fun find [item] (prop: Q.key * item -> bool) (t1: t item): option (key * item) =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
          if prop (k0, v0) then Some (k0, v0)
          else HO.optAlternative (find prop l) (fn () => find prop r)
      | Empty => None

(* * Invariants *)

(* BST property:
       all nodes on the left branch have lesser Q.key values,
       all nodes on the right branch have greater Q.key values,

 propMinMaxBST':
   @returns (propHolds, keyMin, keyMax)
*)

fun propMinMaxBST' [item] (t1: t item): (bool * Q.key * Q.key) =
    case t1 of
      | Empty => error <xml>propBST': empty tree</xml>
      | Node {Key = k0, Left = l, Right = r, ...} =>
          (case (l: t item, r: t item) of
             | (Empty, Empty) => (True, k0, k0)
             | (Empty, Node {...}) => let val (holdsR, minR, maxR) = propMinMaxBST' r
                                          val itHolds = holdsR && k0 < minR
                                          val keyMin = min k0 minR
                                          val keyMax = max k0 maxR
                                      in (itHolds, keyMin, keyMax)
                                      end 
             | (Node {...}, Empty) =>
                                      let val (holdsL, minL, maxL) = propMinMaxBST' l
                                          val itHolds = holdsL && k0 > maxL
                                          val keyMin = min k0 minL
                                          val keyMax = max k0 maxL
                                      in (itHolds, keyMin, keyMax)
                                      end 
             | (Node {...}, Node {...}) =>
                                      let val (holdsR, minR, maxR) = propMinMaxBST' r
                                          val (holdsL, minL, maxL) = propMinMaxBST' l
                                          val itHolds = holdsL && holdsR && k0 > maxL && k0 < minR
                                          val keyMin = min k0 (min minL minR)
                                          val keyMax = max k0 (max maxL maxR)
                                      in (itHolds, keyMin, keyMax)
                                      end
             )

fun propBST [item] (t1: t item): bool =
    case t1 of
      | Empty => True
      | Node {...} => let val (propHolds, _, _) = propMinMaxBST' t1
                      in propHolds
                      end  


(* AATree prop1: Leaf nodes have level 1
  AATree prop2: if there is a left child, the level of the parent is one greater than the left child's one
  AATree prop3: if there is a right child, the level of the parent is 0 or 1 more than the level of the right child
  AATree prop4: if there is a right right grandchild, its level is strictly less than that of the actual node
  AATree prop5: all nodes with level > 1 have two children
*)

fun aaTreeProps [item] (t1: t item): bool =
    case t1 of
      | Empty => True
      | Node {Left = Empty, Right = Empty, Level = lvl, ...} => (* prop1 *) lvl = 1
      | Node {Left = l, Right = r, Level = lvParent, ...} =>
           let
                val prop2 =
                        case l: t item of
                        | Node {Level = lvLChild, ...} => lvParent = 1 + lvLChild
                        | _ => True

                val prop3 =
                        case r: t item of
                        | Node {Level = lvRChild, ...} => let val prcDiff = lvParent - lvRChild
                                                          in 0 <= prcDiff && prcDiff <= 1
                                                          end   
                        | _ => True

                val prop4 =
                        case r: t item of
                        | Node {Right = Node {Level = lvRGChild, ...}, ...} => lvRGChild < lvParent
                        | _ => True

                val prop5 =
                        if lvParent > 1
                        then not (null l) && not (null r)
                        else True
           in
              prop2 && prop3 && prop4 && prop5 &&
              aaTreeProps l && aaTreeProps r
           end  

fun valid [item] (t1: t item): bool = aaTreeProps t1 && propBST t1

end
