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

datatype node v = Node of {Key: Q.key,
                                Value: v,
                                Level: int,
                                Left: option (node v),
                                Right: option (node v)}

type t v = option (node v)

(* * Instances *)


val eq_tree [item] = fn (_ : eq Q.key) (_ : eq item) =>
        let
                fun eq' (t1: t item) (t2: t item) =
                   case (t1, t2) of
                        | (Some (Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...}),
                           Some (Node {Key = k2, Value = v2, Left = lt2, Right = rt2, ...})) =>
                                  k1 = k2 && v1 = v2 && eq' lt1 lt2 && eq' rt1 rt2
                        | (None, None) => True
                        | _ => False
        in mkEq eq'
        end


val show_tree [item] = fn (_ : show Q.key) (_ : show item) =>
        let
           fun show' (t1: t item): string =
              case t1 of
                | Some (Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...}) =>
                    HS.concat ("[" :: show' lt1 :: "," :: show (k1, v1) :: "," :: show' rt1 :: "]" :: [])
                | None => "Empty"
        in mkShow show'
        end
                
(* * Setters / Getters *)

fun setValue [item] (v1: item) (t1: node item): node item =
    case t1 of
        Node r => Node (HR.overwrite r {Value = v1})

fun setKeyAndValue [item] (k1: Q.key) (v1: item) (t1: node item) : node item =
    case t1 of
        Node r => Node (HR.overwrite r {Key = k1, Value = v1})

fun setLevel [item] (v1: int) (t1: node item) : node item =
    case t1 of
        Node r => Node (HR.overwrite r {Level = v1})

fun setLeft [item] (v1: t item) (t1: node item) : node item =
    case t1 of
        Node r => Node (HR.overwrite r {Left = v1})

fun setRight [item] (v1: t item) (t1: node item) : node item =
    case t1 of
        Node r => Node (HR.overwrite r {Right = v1})

fun getLevel [item] (t1: t item) : int =
   case t1 of
     | Some (Node {Level = lvl, ...}) => lvl
     | None => 0

(* * Construction *)

val empty [item] : t item = None

fun singleton [item] (k1: Q.key) (v1: item): t item = Some (Node {Key = k1, Value = v1, Level = 1, Left = None, Right = None})

(* * Query *)

fun null [item] (t1: t item): bool =
    case t1 of
        None => True
        | _ => False

fun size [item] (t1: t item) : int =
    case t1 of
     | Some (Node {Left = l, Right = r, ...}) => 1 + size l + size r
     | None => 0


fun lookup [item] (k1: Q.key) (t1: t item): option item =
    case t1 of
        Some( Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
            (case compare k1 k0 of
                EQ => Some v0
                | LT => lookup k1 l
                | GT => lookup k1 r
                )
        | None => None

val member [item] (k1: Q.key): (t item -> bool) = lookup k1 >>> isSome

(* get root pair to start minimum / maximum value folds *)
fun getAnyPair [item] (t1: t item): option (Q.key * item) =
    case t1 of
      None => None
      | Some( Node {Key = key, Value = item, ...}) => Some (key, item)

(* minimum, maximum to be used in deletes
*)

fun minimum [item] (node1: node item): Q.key * item =
    case node1 of
        Node {Key = k0, Value = v0, Left = l, ...} =>
            case l: t item of
               None => (k0, v0)
               | Some nodeL => minimum nodeL


fun findMin [item] (t1: t item): option (key * item) =
    case t1 of
        Some node1 => Some <| minimum node1
        | None => None
               

fun maximum [item] (node1: node item): Q.key * item =
    case node1 of
        Node {Key = k0, Value = v0, Right = r, ...} =>
            case r: t item of
               None => (k0, v0)
               | Some nodeR => maximum nodeR
               
fun findMax [item] (t1: t item): option (key * item) =
    case t1 of
        Some node1 => Some <| maximum node1
        | None => None

(* * Node balancing *)

(* skew: remove left horizontal links with right rotation
*)
fun skew [item] (t1: t item) : t item =
    case t1 of
        Some node1 =>
          (case node1 of
            Node {Level = lvT, Left = l, ...} =>
              (case l of
                 Some nodeL =>
                   (case nodeL of Node {Level = lvL, Right = lRight, ...} =>
                        if lvT = lvL
                        then Some <| setRight (Some <| setLeft lRight node1) nodeL
                        else t1
                   ) 
                 | None => t1
                 )
           ) 
        | None => t1

(* split: remove consecutive horizontal links
*)

fun split [item] (t1: t item) : t item =
    case t1 of
      Some node1 =>
      (case node1 of
        Node {Level = lvT, Right = r, ...} =>
        (case r of
          Some nodeR =>
            (case nodeR of
            Node {Level = lvR, Left = rLeft, Right = s, ...} =>
              (case s of
                Some nodeS =>
                  (case nodeS of
                    Node {Level = lvS, ...} =>
                      if (lvT = lvS)
                      then Some <| setLevel (lvR +1) (setLeft (Some <| setRight rLeft node1) nodeR)
                      else t1
                  ) 
                | None => t1
              ))
          | None => t1
          ))
     | None => t1

(*
*)

fun splitRight [item] (t1: t item): t item =
    case t1 of
        | Some node1 =>
          (case node1 of
            Node {Right = r, ...} =>
             (case r of
               | Some _ => Some <| setRight (split r) node1
               | None => t1
               ))
        | None => t1

(*
*)

fun skewRight [item] (t1: t item): t item =
    case t1 of
        Some node1 =>
           (case node1 of Node {Right = r, ...} =>
             (case r of
                Some _ => Some <| setRight (skew r) node1
                | None => t1
                ))
        | None => t1

(*
*)

fun skewRightRight [item] (t1: t item): t item =
    case t1 of
        Some node1 =>
          (case node1 of Node {Right = r, ...} =>
          (case r of
             Some nodeR =>
               (case nodeR of Node {Right = s, ...} =>
               (case s of
                  Some _ => let val r' : t item = Some <| setRight (skew s) nodeR
                                  in Some <| setRight r' node1
                                  end
                  | None => t1
                  ))
             | _ => t1
             ))
       | _ => t1

(*
*)

fun decreaseLevel [item] (t1: t item): t item =
    case t1 of
      Some node1 =>
          (case node1 of Node {Level = lvP, Left = l, Right = r, ...} =>
          (case r of
            Some nodeR =>
                 (case nodeR of Node {Level = lvR, ...} =>
                     let val should_be = 1 + min (getLevel l) (getLevel r)
                     in if lvP > should_be
                        then let val r' : t item = if lvR > should_be
                                            then Some <| setLevel should_be nodeR
                                            else r
                                in Some <| setRight r' (setLevel should_be node1)
                                end
                        else t1
                     end)
            | None => let val should_be = 1 + getLevel l
                     in if lvP > should_be
                           then Some <| setLevel should_be node1
                           else t1
                     end
            ))  
      | None => None

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
        Some node1 =>
           (case node1 of
           Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => skewThenSplit (Some <| setLeft (insertWith f k1 v1 l) node1)
              | GT => skewThenSplit (Some <| setRight (insertWith f k1 v1 r) node1)
              | EQ => Some <| setKeyAndValue k1 (f v1 v0) node1
              ))
        | None => singleton k1 v1

val insert [item] (k1: Q.key) (v1: item):  (t item -> t item) = insertWith const k1 v1

(*
*)

fun delete [item] (k1: Q.key) (t1: t item): t item =
    case t1 of
        Some node1 =>
           (case node1 of Node {Key = k0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => rebalance (Some <| setLeft (delete k1 l) node1)
              | GT => rebalance (Some <| setRight (delete k1 r) node1)
              | EQ => (case (l, r) of
                         (None, None) => None  (* deleted *)
                         | (None, Some nodeR) => let val (succK, succV) = minimum nodeR
                                       in rebalance (Some <| setKeyAndValue succK succV (setRight (delete succK r) node1))
                                       end
                         | (Some nodeL, _) => let val (predK, predV) = maximum nodeL
                                   in rebalance (Some <| setKeyAndValue predK predV (setLeft (delete predK l) node1))
                                   end
                         )
              ))
        | None => None

(* * Folding *)

fun foldr' [item] [b] (op: Q.key * item -> b -> b) (t1: t item) (acc: b): b =
    case t1 of
      Some( Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
          (case (l: t item, r: t item) of
                (None, None) => op (k0, v0)  acc
                | _ =>  foldr' op l (op (k0, v0) (foldr' op r acc))
                )
      | None => acc

fun foldr [item] [b] (op: Q.key * item -> b -> b) (acc: b) (t1: t item): b = foldr' op t1 acc

fun toList [item] (t1: t item): list (key * item) = foldr' (curry Cons) t1 []

fun fromList [item] (li: list (key * item)): t item = List.foldl (uncurry insert) empty li


(* * Adjust and mapping *)

fun adjust' [item] (f: item -> item) (k1: Q.key) (t1: t item): t item =
    case t1 of
        Some node1 => (case node1 of
            (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
                (case compare k1 k0 of
                | LT => Some <| setLeft (adjust' f k1 l) node1
                | GT => Some <| setRight (adjust' f k1 r) node1
                | EQ => Some <| setValue (f v0) node1
              ))
        | None => t1 (* case unreached if Q.key non-membership is filtered out *)

fun adjust [item] (f: item -> item) (k1: Q.key) (t1: t item): t item =
    if member k1 t1
       then adjust' f k1 t1
       else t1

fun update' [item] (f: item -> option item) (k1: Q.key) (t1: t item): t item =
    case t1 of
        Some node1 => (case node1 of
           Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
             (case compare k1 k0 of
                LT => rebalance (Some <| setLeft (update' f k1 l) node1)
                | GT => rebalance (Some <| setRight (update' f k1 r) node1)
                | EQ => (case f v0 of
                        | Some v1 => Some <| setValue v1 node1
                        | None => delete k1 t1
                        )  
             ))
        | None => t1 (* case unreached if Q.key non-membership is filtered out *)

fun update [item] (f: item -> option item) (k1: Q.key) (t1: t item): t item =
    if member k1 t1
       then update' f k1 t1
       else t1

fun mapValues [item] [w] (f: item -> w) (t1: t item): t w =
       case t1 of
         Some(Node rc) => Some (Node (HR.overwrite rc {Value = f rc.Value,
                                           Left = mapValues f rc.Left,
                                           Right = mapValues f rc.Right}))
         | None => None

(* short-circuiting exists *)
fun exists [item] (prop: Q.key * item -> bool) (t1: t item): bool =
    case t1 of
      Some (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
                  prop (k0, v0) || exists prop l || exists prop r 
      | None => False

(* short-circuiting all *)
fun all [item] (prop: Q.key * item -> bool) (t1: t item): bool =
    case t1 of
      Some (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
                  prop (k0, v0) && all prop l && all prop r
      | None => True

fun find [item] (prop: Q.key * item -> bool) (t1: t item): option (key * item) =
    case t1 of
      Some (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
          if prop (k0, v0) then Some (k0, v0)
          else HO.optAlternative (find prop l) (fn () => find prop r)
      | None => None

(* * Invariants *)

(* BST property:
       all nodes on the left branch have lesser Q.key values,
       all nodes on the right branch have greater Q.key values,

 propMinMaxBST':
   @returns (propHolds, keyMin, keyMax)
*)

fun propMinMaxBST' [item] (node1: node item): (bool * Q.key * Q.key) =
    case node1 of
        Node {Key = k0, Left = l, Right = r, ...} =>
          case (l: t item, r: t item) of
             | (None, None) => (True, k0, k0)
             | (None, Some nodeR) => let val (holdsR, minR, maxR) = propMinMaxBST' nodeR
                                          val itHolds = holdsR && k0 < minR
                                          val keyMin = min k0 minR
                                          val keyMax = max k0 maxR
                                      in (itHolds, keyMin, keyMax)
                                      end 
             | (Some nodeL, None) =>
                                      let val (holdsL, minL, maxL) = propMinMaxBST' nodeL
                                          val itHolds = holdsL && k0 > maxL
                                          val keyMin = min k0 minL
                                          val keyMax = max k0 maxL
                                      in (itHolds, keyMin, keyMax)
                                      end 
             | (Some nodeL, Some nodeR) =>
                                      let val (holdsR, minR, maxR) = propMinMaxBST' nodeR
                                          val (holdsL, minL, maxL) = propMinMaxBST' nodeL
                                          val itHolds = holdsL && holdsR && k0 > maxL && k0 < minR
                                          val keyMin = min k0 (min minL minR)
                                          val keyMax = max k0 (max maxL maxR)
                                      in (itHolds, keyMin, keyMax)
                                      end
             

fun propBST [item] (t1: t item): bool =
    case t1 of
      | None => True
      | Some node1 => let val (propHolds, _, _) = propMinMaxBST' node1
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
      | None => True
      | Some (Node {Left = None, Right = None, Level = lvl, ...}) => (* prop1 *) lvl = 1
      | Some (Node {Left = l, Right = r, Level = lvParent, ...}) =>
           let
                val prop2 =
                        case l: t item of
                        | Some( Node {Level = lvLChild, ...}) => lvParent = 1 + lvLChild
                        | _ => True

                val prop3 =
                        case r: t item of
                        | Some (Node {Level = lvRChild, ...}) => let val prcDiff = lvParent - lvRChild
                                                          in 0 <= prcDiff && prcDiff <= 1
                                                          end   
                        | _ => True

                val prop4 =
                        case r: t item of
                        | Some (Node {Right = Some (Node {Level = lvRGChild, ...}), ...}) => lvRGChild < lvParent
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
