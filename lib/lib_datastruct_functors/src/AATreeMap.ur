(* Arne Anderson Tree *)


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
end): (AA_TREE_MAP where con key = Q.key) = struct

open Q

open HFunction
open HTuple
open HOrd
open Option
open Monad
structure HS = HString
structure HL = HList
structure HO = HOption
structure HR = HRecord

datatype node v = Node of {Key: key,
                                Value: v,
                                Level: int,
                                Left: option (node v),
                                Right: option (node v)}

type t v = option (node v)

(* * Instances *)


val eq_tree [item] = fn (_ : eq key) (_ : eq item) =>
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


val show_tree [item] = fn (_ : show key) (_ : show item) =>
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

fun setKeyAndValue [item] (k1: key) (v1: item) (t1: node item) : node item =
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

fun getNodeKeyValuePair [item] (t1: node item): (key * item) =
   case t1 of
     Node r => (r.Key, r.Value)

(* * Construction *)

val empty [item] : t item = None

fun singleton [item] (k1: key) (v1: item): t item = Some (Node {Key = k1, Value = v1, Level = 1, Left = None, Right = None})

(* * Query *)

val null [item]: t item -> bool = isNone

fun size [item] (t1: t item) : int =
    case t1 of
     | Some (Node {Left = l, Right = r, ...}) => 1 + size l + size r
     | None => 0


fun lookup [item] (k1: key) (t1: t item): option item =
    case t1 of
        Some( Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
            (case compare k1 k0 of
                EQ => Some v0
                | LT => lookup k1 l
                | GT => lookup k1 r
                )
        | None => None

val member [item] (k1: key): (t item -> bool) = lookup k1 >>> isSome

(* get root pair to start minimum / maximum value folds *)
val getAnyPair [item]: t item -> option (key * item) = liftM getNodeKeyValuePair

(* minimum: leftmost (k,v) *)
val rec minimum [item]: node item -> key * item =
       fn (Node {Key = k0, Value = v0, Left = l, ...}) => HO.fromOptionDefMap (k0, v0) minimum l

(* maximum: rightmost (k,v) *)               
val rec maximum [item]: node item -> key * item =
       fn (Node {Key = k0, Value = v0, Right = r, ...}) => HO.fromOptionDefMap (k0, v0) maximum r


val findMin [item]: t item -> option (key * item) = liftM minimum

val findMax [item]: t item -> option (key * item) = liftM maximum

(* * Node balancing *)

(* skew: remove left horizontal links with right rotation
*)
fun skew [item] (root: node item) : node item =
          case root of
            | Node {Level = lvParent, Left = Some nodeL, ...} =>
                   (case nodeL of
                    | Node {Level = lvL, Right = lRight, ...} =>
                        if lvParent = lvL
                        then setRight (Some <| setLeft lRight root) nodeL
                        else root
                   ) 
            | _ => root

(* split: remove consecutive horizontal links
*)

fun split [item] (root: node item) : node item =
      case root of
        | Node {Level = lvParent, Right = Some nodeR, ...} =>
          (case nodeR of
            | Node {Level = lvR, Left = rLeft, Right = Some nodeS, ...} =>
              (case nodeS of
                Node {Level = lvS, ...} =>
                      if (lvParent = lvS)
                      then setLevel (lvR +1) <| setLeft (Some <| setRight rLeft root) nodeR
                      else root
              )
            | _ => root
          )
        | _ => root

(*
*)

fun splitRight [item] (root: node item): node item =
          case root of
           | Node {Right = Some nodeR, ...} =>
               setRight (Some <| split nodeR) root

           | _ => root

(*
*)

fun skewRight [item] (root: node item): node item =
           case root of
            | Node {Right = Some nodeR, ...} =>
                    setRight (Some <| skew nodeR) root
            | _ => root

(*
*)

fun skewRightRight [item] (root: node item): node item =
           case root of
           | Node {Right = Some nodeR, ...} =>
                  (case nodeR of
                  | Node {Right = Some nodeS, ...} =>
                        let val nodeR' : node item = setRight (Some <| skew nodeS) nodeR
                        in
                            setRight (Some nodeR') root
                        end
                  | _ => root
                  )
           | _ => root

(*
*)

fun decreaseLevel [item] (root: node item): node item =
          case root of Node {Level = lvP, Left = l, Right = r, ...} =>
          (case r of
            Some nodeR =>
                 (case nodeR of Node {Level = lvR, ...} =>
                     let val lvl_should_be = 1 + min (getLevel l) (getLevel r)
                     in if lvP > lvl_should_be
                        then let val r' : t item = if lvR > lvl_should_be
                                            then Some <| setLevel lvl_should_be nodeR
                                            else r
                                in setRight r' (setLevel lvl_should_be root)
                                end
                        else root
                     end)

            | None => let val lvl_should_be = 1 + getLevel l
                     in if lvP > lvl_should_be
                           then setLevel lvl_should_be root
                           else root
                     end
            )

(*
*)
val rebalance [item] : (node item -> node item) = (* with left to right function composition *)
    decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight


(* * Insert *)

fun insertWith [item] (f: item -> item -> item) (k1: key) (v1: item) (t1: t item): t item =
    let HO.fromOptionDefMap (singleton k1 v1) (insertWith' >>> Some) t1
    where
        fun insertWith' (root: node item): node item =
           case root of
             Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
             (case compare k1 k0 of
                | LT => (skew >>> split) <| setLeft (insertWith f k1 v1 l) root
                | GT => (skew >>> split) <| setRight (insertWith f k1 v1 r) root
                | EQ => setKeyAndValue k1 (f v1 v0) root
              )
    end

val insert [item]: key -> item -> t item -> t item = insertWith const

fun fromList [item] (li: list (key * item)): t item = List.foldl (uncurry insert) empty li


(* * Delete *)

fun delete [item] (k1: key) (t1: t item): t item =
    let
        HO.fromOptionDefMap t1 delete' t1
    where
      fun delete' (root: node item): t item =
         case root of Node {Key = k0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => (rebalance >>> Some) <| setLeft (delete k1 l) root
              | GT => (rebalance >>> Some) <| setRight (delete k1 r) root
              | EQ => (case (l, r) of
                         (None, None) => (* it is a leaf => delete it *) None
                         | (None, Some nodeR) => (* replace with successor and rebalance *)
                                       let val (succK, succV) = minimum nodeR
                                       in root |> setRight (delete succK r)
                                               |> setKeyAndValue succK succV
                                               |> (rebalance >>> Some)    
                                       end
                         | (Some nodeL, _) => (* replace with predecessor and rebalance *)
                                   let val (predK, predV) = maximum nodeL
                                   in root |> setLeft (delete predK l)
                                           |> setKeyAndValue predK predV
                                           |> (rebalance >>> Some) 
                                   end
                         )
              )
    end

(* * Folding *)

fun foldr [item] [b] (op: key * item -> b -> b) (acc: b) (t1: t item): b =
  let
        g t1 acc
  where

    val rec foldr': node item -> (b -> b) =
            fn (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
                 g l <<< op (k0, v0) <<< g r

    and g: t item -> (b -> b) = fn tree => HO.fromOptionDefMap id foldr' tree
  end

fun toList [item] (t1: t item): list (key * item) = foldr (curry Cons) [] t1


(* * Adjust and mapping *)

datatype updated = Upd_Adjusted | Upd_Deleted | Upd_UnModified

val update [item] (f: item -> option item) (k1: key) (t1: t item): t item =
    let snd (update' t1)
    where
        fun update' (t2: t item): (updated * t item) =
        case t2 of
                Some root => (case root of
                   Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>

                        (case compare k1 k0 of

                        | EQ => (case f v0 of
                                | Some v1 => (Upd_Adjusted, Some <| setValue v1 root)
                                | None => (Upd_Deleted, delete k1 t2)
                                )

                        | LT => let val (updated, newLeft) = update' l
                                    val newVal = case updated of
                                        | Upd_Adjusted => Some <| setLeft newLeft root
                                        | Upd_Deleted => (rebalance >>> Some) <| setLeft newLeft root
                                        | Upd_UnModified => t2
                                in
                                  (updated, newVal)
                                end

                        | GT => let val (updated, newRight) = update' r
                                    val newVal = case updated of
                                        | Upd_Adjusted => Some <| setRight newRight root
                                        | Upd_Deleted => (rebalance >>> Some) <| setRight newRight root 
                                        | Upd_UnModified => t2
                                in
                                  (updated, newVal)
                                end
                ))
                | None => (Upd_UnModified, t2)
    end

val adjust [item] (f: item -> item): key -> t item -> t item = update (Some <<< f)

fun mapValues [item] [b] (f: item -> b) (t1: t item): t b =
    let
       liftM mapValues' t1
    where
       fun mapValues' (root: node item): node b =
           case root of
               Node rc => Node (HR.overwrite rc {
                                    Value = f rc.Value,
                                    Left = liftM mapValues' rc.Left,
                                    Right = liftM mapValues' rc.Right
                                    })
    end

(* short-circuiting exists *)
fun exists [item] (prop: key * item -> bool) (t1: t item): bool =
    case t1 of
      Some (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
                  prop (k0, v0) || exists prop l || exists prop r 
      | None => False

(* short-circuiting all *)
fun all [item] (prop: key * item -> bool) (t1: t item): bool =
    case t1 of
      Some (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
                  prop (k0, v0) && all prop l && all prop r
      | None => True

fun find [item] (prop: key * item -> bool) (t1: t item): option (key * item) =
    case t1 of
      Some (Node {Key = k0, Value = v0, Left = l, Right = r, ...}) =>
          if prop (k0, v0) then Some (k0, v0)
          else  (find prop l) `HO.orElse` (fn () => find prop r)
      | None => None

(* * Invariants *)

(* BST property:
       all nodes on the left branch have lesser key values,
       all nodes on the right branch have greater key values,
*)

fun propBST [item] (t1: t item): bool =
  let
    case t1 of
      | None => True
      | Some root => let val (prop, _, _) = propMinMax root
                      in prop
                      end  
  where
    (* propMinMax:
        @param root
        @return (propHolds, keyMin, keyMax)
    *)
    fun propMinMax [item] (root: node item): (bool * key * key) =
      case root of
        Node {Key = k0, Left = l, Right = r, ...} =>
          case (l: t item, r: t item) of
             | (None, None) => (True, k0, k0)
             | (None, Some nodeR) => let val (holdsR, minR, maxR) = propMinMax nodeR
                                      in (holdsR && k0 < minR, 
                                          min k0 minR, 
                                          max k0 maxR)
                                      end
             | (Some nodeL, None) =>
                                      let val (holdsL, minL, maxL) = propMinMax nodeL
                                      in (holdsL && k0 > maxL, 
                                          min k0 minL, 
                                          max k0 maxL)
                                      end
             | (Some nodeL, Some nodeR) =>
                                      let val (holdsR, minR, maxR) = propMinMax nodeR
                                          val (holdsL, minL, maxL) = propMinMax nodeL

                                      in (holdsL && holdsR && k0 > maxL && k0 < minR, 
                                          min k0 (min minL minR), 
                                          max k0 (max maxL maxR))
                                      end
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
