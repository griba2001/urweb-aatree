(* Arne Anderson Tree *)


(*
*)

functor MkAATreeMap(Q: sig
                         con key :: Type
                         con item :: Type
                         val ord_key: ord key
end):sig

con t :: Type -> Type -> Type

val empty :  t Q.key Q.item

val singleton :  Q.key -> Q.item -> t Q.key Q.item

val null :  t Q.key Q.item -> bool

val size :  t Q.key Q.item -> int

val lookup:  Q.key -> t Q.key Q.item -> option Q.item

val member:  Q.key -> t Q.key Q.item -> bool

val findMin :  t Q.key Q.item -> option (Q.key * Q.item)

val findMax :  t Q.key Q.item -> option (Q.key * Q.item)

val getAnyPair :  t Q.key Q.item -> option (Q.key * Q.item)

val insert:  Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

val insertWith:  (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

val fromList :  list (Q.key * Q.item) -> t Q.key Q.item

val delete:  Q.key -> t Q.key Q.item -> t Q.key Q.item

val adjust:  (Q.item -> Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

val update:  (Q.item -> option Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

val mapValues:  b ::: Type -> (Q.item -> b) -> t Q.key Q.item -> t Q.key b

val mapKeysMonotonic :  key' ::: Type -> (Q.key -> key') -> t Q.key Q.item -> t key' Q.item

val foldr:  b ::: Type -> (Q.key * Q.item -> b -> b) -> b -> t Q.key Q.item -> b

val toList :  t Q.key Q.item -> list (Q.key * Q.item)

val exists : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

val all : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

val find: (Q.key * Q.item -> bool) -> t Q.key Q.item -> option (Q.key * Q.item)

(* * Invariants *)

val propBST :  t Q.key Q.item -> bool

val aaTreeProps :  t Q.key Q.item -> bool

val valid :  t Q.key Q.item -> bool

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


datatype t k v = Empty | Node of {Key: k,
                                Value: v,
                                Level: int,
                                Left: t k v,
                                Right: t k v}

(* * Instances *)

val eq_tree = fn (_ : eq key) (_ : eq item) =>
        let
                fun eq' (t1: t key item) (t2: t key item) =
                   case (t1, t2) of
                        | (Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...},
                           Node {Key = k2, Value = v2, Left = lt2, Right = rt2, ...}) =>
                                  k1 = k2 && v1 = v2 && eq' lt1 lt2 && eq' rt1 rt2
                        | (Empty, Empty) => True
                        | _ => False
        in mkEq eq'
        end

val show_tree = fn (_ : show key) (_ : show item) =>
        let
           fun show' (t1: t key item): string =
              case t1 of
                | Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...} =>
                    HS.concat ("[" :: show' lt1 :: "," :: show (k1, v1) :: "," :: show' rt1 :: "]" :: [])
                | Empty => "Empty"
        in mkShow show'
        end

                
(* * Setters / Getters *)

fun setValue (v1: item) (t1: t key item): t key item =
    case t1 of
        Node r => Node (HR.overwrite r {Value = v1})
        | _ => error <xml>setValue: not a Node</xml>

fun setKeyAndValue (k1: key) (v1: item) (t1: t key item) : t key item =
    case t1 of
        Node r => Node (HR.overwrite r {Key = k1, Value = v1})
        | _ => error <xml>setKeyAndValue: not a Node</xml>

fun setLevel (v1: int) (t1: t key item) : t key item =
    case t1 of
        Node r => Node (HR.overwrite r {Level = v1})
        | _ => error <xml>setLevel: not a Node</xml>

fun setLeft (v1: t key item) (t1: t key item) : t key item =
    case t1 of
        Node r => Node (HR.overwrite r {Left = v1})
        | _ => error <xml>setLeft: not a Node</xml>

fun setRight (v1: t key item) (t1: t key item) : t key item =
    case t1 of
        Node r => Node (HR.overwrite r {Right = v1})
        | _ => error <xml>setRight: not a Node</xml>

fun getLevel (t1: t key item) : int =
   case t1 of
     | Node {Level = lvl, ...} => lvl
     | Empty => 0

(* * Construction *)

val empty : t key item = Empty

fun singleton (k1: key) (v1: item): t key item = Node {Key = k1, Value = v1, Level = 1, Left = Empty, Right = Empty}

(* * Query *)

fun null (t1: t key item): bool =
    case t1 of
        Empty => True
        | _ => False

fun size (t1: t key item) : int =
    case t1 of
     | Node {Left = l, Right = r, ...} => 1 + size l + size r
     | Empty => 0


fun lookup (k1: key) (t1: t key item): option item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
            (case compare k1 k0 of
                EQ => Some v0
                | LT => lookup k1 l
                | GT => lookup k1 r
                )
        | Empty => None

val member (k1: key): (t key item -> bool) = lookup k1 >>> isSome

(* get root pair to start minimum / maximum value folds *)
fun getAnyPair (t1: t key item): option (key * item) =
    case t1 of
      Empty => None
      | Node {Key = key, Value = item, ...} => Some (key, item)

(* minimum, maximum to be used in deletes
*)

fun findMin (t1: t key item): option (key * item) =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, ...} =>
            (case l: t key item of
               Empty => Some (k0, v0)
               | _ => findMin l
               )
        | Empty => None

fun minimum (t1: t key item): key * item =
    case findMin t1 of
        Some x => x
        | None => error <xml>aatree minimum: empty tree</xml>

fun findMax (t1: t key item): option (key * item) =
    case t1 of
        Node {Key = k0, Value = v0, Right = r, ...} =>
            (case r: t key item of
               Empty => Some (k0, v0)
               | _ => findMax r
               )
        | Empty => None

fun maximum (t1: t key item): key * item =
    case findMax t1 of
        Some x => x
        | None => error <xml>aatree maximum: empty tree</xml>


(* * Node balancing *)

(* skew: remove left horizontal links with right rotation
*)
fun skew (t1: t key item) : t key item =
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

fun split (t1: t key item) : t key item =
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

fun splitRight (t1: t key item): t key item =
    case t1 of
        | Node {Right = r, ...} =>
           (case r of
             | Node {...} => setRight (split r) t1
             | Empty => t1
             )
        | Empty => t1

(*
*)

fun skewRight (t1: t key item): t key item =
    case t1 of
        Node {Right = r, ...} =>
           (case r of
              Node {...} => setRight (skew r) t1
              | Empty => t1
              )
        | Empty => t1

(*
*)

fun skewRightRight (t1: t key item): t key item =
    case t1 of
        Node {Right = r, ...} =>
          (case r of
             Node {Right = s, ...} =>
               (case s of
                  Node {...} => let val r' : t key item = setRight (skew s) r
                                  in setRight r' t1
                                  end
                  | Empty => t1
                  )
             | _ => t1
             )
       | _ => t1

(*
*)

fun decreaseLevel (t1: t key item): t key item =
    case t1 of
      Node {Level = lvP, Left = l, Right = r, ...} =>
          (case r of
            Node {Level = lvR, ...} =>
                     let val should_be = 1 + min (getLevel l) (getLevel r)
                     in if lvP > should_be
                        then let val r' : t key item = if lvR > should_be
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
val rebalance : (t key item -> t key item) = (* with left to right function composition *)
    decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight

val skewThenSplit : (t key item -> t key item) = skew >>> split

(* * Insert / delete  *)

(*
*)


fun insertWith (f: item -> item -> item) (k1: key) (v1: item) (t1: t key item): t key item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => skewThenSplit (setLeft (insertWith f k1 v1 l) t1)
              | GT => skewThenSplit (setRight (insertWith f k1 v1 r) t1)
              | EQ => setKeyAndValue k1 (f v1 v0) t1
              )
        | Empty => singleton k1 v1

val insert (k1: key) (v1: item):  (t key item -> t key item) = insertWith const k1 v1


(*
*)

fun delete (k1: key) (t1: t key item): t key item =
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

fun foldr' [b] (op: key * item -> b -> b) (t1: t key item) (acc: b): b =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
          (case (l: t key item, r: t key item) of
                (Empty, Empty) => op (k0, v0)  acc
                | _ =>  foldr' op l (op (k0, v0) (foldr' op r acc))
                )
      | Empty => acc

fun foldr [b] (op: key * item -> b -> b) (acc: b) (t1: t key item): b = foldr' op t1 acc

fun toList (t1: t key item): list (key * item) = foldr' (curry Cons) t1 []

fun fromList (li: list (key * item)): t key item = List.foldl (uncurry insert) empty li


(* * Adjust and mapping *)

fun adjust' (f: item -> item) (k1: key) (t1: t key item): t key item =
    case t1 of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => setLeft (adjust' f k1 l) t1
              | GT => setRight (adjust' f k1 r) t1
              | EQ => setValue (f v0) t1
              )
        | Empty => t1 (* case unreached if key non-membership is filtered out *)

fun adjust (f: item -> item) (k1: key) (t1: t key item): t key item =
    if member k1 t1
       then adjust' f k1 t1
       else t1

fun update' (f: item -> option item) (k1: key) (t1: t key item): t key item =
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
        | Empty => t1 (* case unreached if key non-membership is filtered out *)

fun update (f: item -> option item) (k1: key) (t1: t key item): t key item =
    if member k1 t1
       then update' f k1 t1
       else t1

fun mapValues [w] (f: item -> w) (t1: t key item): t key w =
       case t1 of
         Node rc => Node (HR.overwrite rc {Value = f rc.Value,
                                           Left = mapValues f rc.Left,
                                           Right = mapValues f rc.Right})
         | Empty => Empty

fun mapKeysMonotonic [key'] (f: key -> key') (t1: t key item): t key' item =
       case t1 of
         Node rc => Node (HR.overwrite rc {Key = f rc.Key,
                                           Left = mapKeysMonotonic f rc.Left,
                                           Right = mapKeysMonotonic f rc.Right})
         | Empty => Empty


(* short-circuiting exists *)
fun exists (prop: key * item -> bool) (t1: t key item): bool =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
                  prop (k0, v0) || exists prop l || exists prop r 
      | Empty => False

(* short-circuiting all *)
fun all (prop: key * item -> bool) (t1: t key item): bool =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
                  prop (k0, v0) && all prop l && all prop r
      | Empty => True

fun find (prop: key * item -> bool) (t1: t key item): option (key * item) =
    case t1 of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
          if prop (k0, v0) then Some (k0, v0)
          else HO.optAlternative (find prop l) (fn () => find prop r)
      | Empty => None

(* * Invariants *)

(* BST property worth checking after MapKeysMonotonic:
       all nodes on the left branch have lesser key values,
       all nodes on the right branch have greater key values,

 propMinMaxBST':
   @returns (propHolds, keyMin, keyMax)
*)

fun propMinMaxBST' (t1: t key item): (bool * key * key) =
    case t1 of
      | Empty => error <xml>propBST': empty tree</xml>
      | Node {Key = k0, Left = l, Right = r, ...} =>
          (case (l: t key item, r: t key item) of
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

fun propBST (t1: t key item): bool =
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

fun aaTreeProps (t1: t key item): bool =
    case t1 of
      | Empty => True
      | Node {Left = Empty, Right = Empty, Level = lvl, ...} => (* prop1 *) lvl = 1
      | Node {Left = l, Right = r, Level = lvParent, ...} =>
           let
                val prop2 =
                        case l: t key item of
                        | Node {Level = lvLChild, ...} => lvParent = 1 + lvLChild
                        | _ => True

                val prop3 =
                        case r: t key item of
                        | Node {Level = lvRChild, ...} => let val prcDiff = lvParent - lvRChild
                                                          in 0 <= prcDiff && prcDiff <= 1
                                                          end   
                        | _ => True

                val prop4 =
                        case r: t key item of
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

fun valid (t1: t key item): bool = aaTreeProps t1 && propBST t1

end
