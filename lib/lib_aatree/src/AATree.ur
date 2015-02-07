(* Arne Anderson Tree *)

open HFunction
open HTuple
open HOrd
open Option 
structure HS = HString
structure HL = HList

datatype tree k v = Empty | Node of {Key: k,
                                Value: v,
                                Level: int,
                                Left: tree k v,
                                Right: tree k v}

(* * Instances *)

val eq_tree = fn [k][v] (_ : eq k) (_ : eq v) =>
        let
                fun eq' (t1: tree k v) (t2: tree k v) =
                   case (t1, t2) of
                        | (Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...},
                           Node {Key = k2, Value = v2, Left = lt2, Right = rt2, ...}) =>
                                  k1 = k2 && v1 = v2 && eq' lt1 lt2 && eq' rt1 rt2
                        | (Empty, Empty) => True
                        | _ => False
        in mkEq eq'
        end

val show_tree = fn [k][v] (_ : show k) (_ : show v) =>
        let
           fun show' (t: tree k v): string =
              case t of
                | Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...} =>
                    HS.concat ("[" :: show' lt1 :: "," :: show (k1, v1) :: "," :: show' rt1 :: "]" :: [])
                | Empty => "Empty"
        in mkShow show'
        end        
                
(* * Setters / Getters *)

fun setValue [k][v] (v1: v) (t: tree k v): tree k v =
    case t of
        Node r => Node (r -- #Value ++ {Value = v1})
        | _ => error <xml>setValue: not a Node</xml>

fun setKeyAndValue [k][v] (k1: k) (v1: v) (t: tree k v) : tree k v =
    case t of
        Node r => Node (r -- #Key -- #Value ++ {Key = k1, Value = v1})
        | _ => error <xml>setKeyAndValue: not a Node</xml>

fun setLevel [k][v] (v1: int) (t: tree k v) : tree k v =
    case t of
        Node r => Node (r -- #Level ++ {Level = v1})
        | _ => error <xml>setLevel: not a Node</xml>

fun setLeft [k][v] (v1: tree k v) (t: tree k v) : tree k v =
    case t of
        Node r => Node (r -- #Left ++ {Left = v1})
        | _ => error <xml>setLeft: not a Node</xml>

fun setRight [k][v] (v1: tree k v) (t: tree k v) : tree k v =
    case t of
        Node r => Node (r -- #Right ++ {Right = v1})
        | _ => error <xml>setRight: not a Node</xml>

fun getLevel [k][v] (t: tree k v) : int =
   case t of
     | Node {Level = lvl, ...} => lvl
     | Empty => 0

(* * Construction *)

val empty [k][v] : tree k v = Empty

fun singleton [k][v] (k1: k) (v1: v): tree k v = Node {Key = k1, Value = v1, Level = 1, Left = Empty, Right = Empty}

(* * Query *)

fun null [k][v] (t: tree k v): bool =
    case t of
        Empty => True
        | _ => False

fun size [k][v] (t: tree k v) : int =
    case t of
     | Node {Left = l, Right = r, ...} => 1 + size l + size r
     | Empty => 0


fun lookup [k][v] (_: ord k) (k1: k) (t: tree k v): option v =
    case t of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
            (case compare k1 k0 of
                EQ => Some v0
                | LT => lookup k1 l
                | GT => lookup k1 r
                )
        | Empty => None

val member [k][v] (_ : ord k) (k1: k): (tree k v -> bool) = compose isSome (lookup k1)

(* get root pair to start minimum / maximum value folds *)
fun getAnyPair [k][v] (t: tree k v): option (k * v) =
    case t of
      Empty => None
      | Node {Key = k, Value = v, ...} => Some (k, v)

(* minimum, maximum to be used in deletes

* Haskell code
minimum (Node x _ Empty _) = x
minimum (Node x _ l _) = minimum l
minimum Empty = error "minimum: empty tree"

maximum (Node x _ _ Empty) = x
maximum (Node x _ _ r) = maximum r
maximum Empty = error "maximum: empty tree"
*)

fun findMin [k][v] (t: tree k v): option (k * v) =
    case t of
        Node {Key = k0, Value = v0, Left = l, ...} =>
            (case l: tree k v of
               Empty => Some (k0, v0)
               | _ => findMin l
               )
        | Empty => None

fun minimum [k][v] (t: tree k v): k * v =
    case findMin t of
        Some x => x
        | None => error <xml>aatree minimum: empty tree</xml>

fun findMax [k][v] (t: tree k v): option (k * v) =
    case t of
        Node {Key = k0, Value = v0, Right = r, ...} =>
            (case r: tree k v of
               Empty => Some (k0, v0)
               | _ => findMax r
               )
        | Empty => None

fun maximum [k][v] (t: tree k v): k * v =
    case findMax t of
        Some x => x
        | None => error <xml>aatree maximum: empty tree</xml>

(* * Node balancing *)

(* skew: right rotation *)
fun skew [k][v] (t: tree k v) : tree k v =
    case t of
        Node {Level = lvT, Left = l, ...} =>
            (case l of
               Node {Level = lvL, Right = r, ...} =>
                        if lvT = lvL
                        then (let val t' = setLeft r t
                                  val l' = setRight t' l
                                in l'
                                end)
                        else t
               | _ => t
               )
        | _ => t

(* Haskell :

split t @ (Node _ lvT a r @ (Node _ lvR b x @ (Node _ lvX _ _)))
   | lvT == lvX = let t' = t {right = left r}
                      r' = r {left = t', level = lvR + 1}
                  in r'
   | otherwise = t

split t = t

*)

fun split [k][v] (t: tree k v) : tree k v =
    case t of
      Node {Level = lvT, Right = r, ...} =>
        (case r of
          Node {Level = lvR, Left = rLeft, Right = s, ...} =>
            (case s of
              Node {Level = lvS, ...} =>
                if (lvT = lvS)
                  then let val t' = setRight rLeft t
                           val r' = setLevel (lvR +1) (setLeft t' r)
                       in r' end
                  else t
              | _ => t)
          | _ => t)
     | _ => t

(* Haskell
splitRight Empty = Empty
splitRight (Node x lv l r) = Node x lv l (split r)
*)

fun splitRight [k][v] (t: tree k v): tree k v =
    case t of
        | Node {Right = r, ...} =>
           (case r of
             | Node {...} => setRight (split r) t
             | Empty => t
             )
        | Empty => t

(* Haskell
skewRight Empty = Empty
skewRight (Node x lv l r) = Node x lv l (skew r)
*)

fun skewRight [k][v] (t: tree k v): tree k v =
    case t of
        Node {Right = r, ...} =>
           (case r of
              Node {...} => setRight (skew r) t
              | Empty => t
              )
        | Empty => t

(* Haskell
skewRightRight (Node x lv l (Node x' lv' l' r')) = Node x lv l (Node x' lv' l' (skew r'))
skewRightRight t = t
*)

fun skewRightRight [k][v] (t: tree k v): tree k v =
    case t of
        Node {Right = r, ...} =>
          (case r of
             Node {Right = s, ...} =>
               (case s of
                  Node {...} => let val r' : tree k v = setRight (skew s) r
                                  in setRight r' t
                                  end
                  | Empty => t
                  )
             | _ => t
             )
       | _ => t

(* Haskell

(.$) = flip ($)

decreaseLevel :: Tree a -> Tree a
decreaseLevel Empty = Empty
decreaseLevel t @ (Node _ lvP l Empty)
        | lvP > should_be = t {level = should_be}
        | otherwise = t
        where should_be = 1 + getLevel l

decreaseLevel t @ (Node _ lvP l r @ (Node _ lvR _ _))
        | lvP > should_be =
             let r' = if (r .$ level) > should_be
                then r {level = should_be}
                else r
             in t {level = should_be, right = r'}
        | otherwise = t
    where should_be = 1 + min (getLevel l) (getLevel r)
*)

fun decreaseLevel [k][v] (t: tree k v): tree k v =
    case t of
      Node {Level = lvP, Left = l, Right = r, ...} =>
          (case r of
            Node {Level = lvR, ...} =>
                     let val should_be = 1 + min (getLevel l) (getLevel r)
                     in if lvP > should_be
                        then let val r' : tree k v = if lvR > should_be
                                            then setLevel should_be r
                                            else r
                                in setRight r' (setLevel should_be t)
                                end
                        else t
                     end
            | Empty => let val should_be = 1 + getLevel l
                     in if lvP > should_be
                           then setLevel should_be t
                           else t
                     end
            )  
      | Empty => Empty

(* Haskell
rebalance = decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight
*)
val rebalance [k][v] : (tree k v -> tree k v) = (* with left to right function composition *)
    andThen decreaseLevel (andThen skew (andThen skewRight (andThen skewRightRight (andThen split splitRight))))


(* * Insert / delete  *)

(* Haskell:

(.$) = flip ($)

insert x Empty = singleton x
insert x (Node y lv l r) = case compare x y of
        LT -> Node y lv (insert x l) r .$ skew .$ split
        GT -> Node y lv l (insert x r) .$ skew .$ split
        EQ -> Node x lv l r
*)


fun insertWith [k][v] (_: ord k) (f: v -> v -> v) (k1: k) (v1: v) (t: tree k v): tree k v =
    case t of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => split (skew (setLeft (insertWith f k1 v1 l) t))
              | GT => split (skew (setRight (insertWith f k1 v1 r) t))
              | EQ => setKeyAndValue k1 (f v1 v0) t
              )
        | Empty => singleton k1 v1

val insert [k][v] (_: ord k) (k1: k) (v1: v):  (tree k v -> tree k v) = insertWith const k1 v1   


(* Haskell
(.$) = flip ($)

delete x Empty = Empty
delete x t @ (Node y lv l r) = case compare x y of
        LT -> Node y lv (delete x l) r .$ rebalance
        GT -> Node y lv l (delete x r) .$ rebalance
        EQ -> case (l, r) of
                (Empty, Empty) -> Empty -- deleted
                (Empty, _) -> Node successor lv l (delete successor r) .$ rebalance -- copy successor value and delete successor
                where successor = minimum r
                (_, _) -> Node predecessor lv (delete predecessor l) r .$ rebalance -- copy predecessor value and delete predecessor
                where predecessor = maximum l
*)

fun delete [k][v] (_: ord k) (k1: k) (t: tree k v): tree k v =
    case t of
        Node {Key = k0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => rebalance (setLeft (delete k1 l) t)
              | GT => rebalance (setRight (delete k1 r) t)
              | EQ => (case (l, r) of
                         (Empty, Empty) => Empty  (* deleted *)
                         | (Empty, _) => let val (succK, succV) = minimum r
                                       in rebalance (setKeyAndValue succK succV (setRight (delete succK r) t))
                                       end
                         | (_, _) => let val (predK, predV) = maximum l
                                   in rebalance (setKeyAndValue predK predV (setLeft (delete predK l) t))
                                   end
                         )
              )
        | Empty => Empty

(* * Folding *)

fun foldr' [k][v][b] (op: k * v -> b -> b) (t: tree k v) (acc: b): b =
    case t of
      Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
          (case (l: tree k v, r: tree k v) of
                (Empty, Empty) => op (k0, v0)  acc
                | _ =>  foldr' op l (op (k0, v0) (foldr' op r acc))
                )
      | Empty => acc

fun foldr [k][v][b] (op: k * v -> b -> b) (acc: b) (t: tree k v): b = foldr' op t acc

fun filterFoldr [k][v][b] (prop: k * v -> bool) (op: k * v -> b -> b) (acc: b) (t: tree k v): b =
    let fun op' (pair: k * v) (acc: b): b =
            if prop pair then op pair acc
            else acc
    in
        foldr' op' t acc
    end

fun toList [k][v] (t: tree k v): list (k * v) = foldr' (curry Cons) t []

fun fromList [k][v] (_ : ord k) (li: list (k * v)): tree k v = List.foldl (uncurry insert) empty li


(* * Adjust and mapping *)

fun adjust' [k][v] (_: ord k) (f: v -> v) (k1: k) (t: tree k v): tree k v =
    case t of
        Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => setLeft (adjust' f k1 l) t
              | GT => setRight (adjust' f k1 r) t
              | EQ => setValue (f v0) t
              )
        | Empty => t (* case unreached if key non-membership is filtered *)

fun adjust [k][v] (_: ord k) (f: v -> v) (k1: k) (t: tree k v): tree k v =
    if member k1 t
       then adjust' f k1 t
       else t


fun mapValues [k][v][w] (f: v -> w) (t: tree k v): tree k w =
       case t of
         Node rc => Node (rc -- #Value -- #Left -- #Right ++
                               {Value = f rc.Value,
                               Left = mapValues f rc.Left,
                               Right = mapValues f rc.Right})
         | Empty => Empty

fun mapKeysMonotonic [k][v][k'] (f: k -> k') (t: tree k v): tree k' v =
       case t of
         Node rc => Node (rc -- #Key -- #Left -- #Right ++
                               {Key = f rc.Key,
                               Left = mapKeysMonotonic f rc.Left,
                               Right = mapKeysMonotonic f rc.Right})
         | Empty => Empty


(* * Invariants *)

(* BST property worth checking after MapKeysMonotonic:
       all nodes on the left branch have lesser key values,
       all nodes on the right branch have greater key values,
*)
(* propBST':
   @returns (propHolds, keyMin, keyMax)
*)

fun propBST' [k][v] (_: ord k) (t: tree k v): (bool * k * k) = 
    case t of
      | Empty => error <xml>propBST': empty tree</xml>
      | Node {Key = k0, Left = l, Right = r, ...} =>
          (case (l: tree k v, r: tree k v) of
             | (Empty, Empty) => (True, k0, k0)
             | (Empty, Node {...}) => let val (holdsR, minR, maxR) = propBST' r
                                          val holdsThis = holdsR && k0 < minR
                                          val minThis = min k0 minR
                                          val maxThis = max k0 maxR
                                      in (holdsThis, minThis, maxThis)
                                      end 
             | (Node {...}, Empty) =>
                                      let val (holdsL, minL, maxL) = propBST' l
                                          val holdsThis = holdsL && k0 > maxL
                                          val minThis = min k0 minL
                                          val maxThis = max k0 maxL
                                      in (holdsThis, minThis, maxThis)
                                      end 
             | (Node {...}, Node {...}) =>
                                      let val (holdsR, minR, maxR) = propBST' r
                                          val (holdsL, minL, maxL) = propBST' l
                                          val holdsThis = holdsL && holdsR && k0 > maxL && k0 < minR
                                          val minThis = min k0 (min minL minR)
                                          val maxThis = max k0 (max maxL maxR)
                                      in (holdsThis, minThis, maxThis)
                                      end
             )

fun propBST [k][v] (_: ord k) (t: tree k v): bool =
    case t of
      | Empty => True
      | Node {...} => let val (propHolds, _, _) = propBST' t
                      in propHolds
                      end  


(* AATree prop1: Leaf nodes have level 1
  AATree prop2: if there is a left child, the level of the parent is one greater than the left child's one
  AATree prop3: if there is a right child, the level of the parent is 0 or 1 more than the level of the right child
  AATree prop4: if there is a right right grandchild, its level is strictly less than that of the actual node
  AATree prop5: all nodes with level > 1 have two children
*)

fun aaTreeProps [k][v] (t: tree k v): bool =
    case t of
      | Empty => True
      | Node {Left = Empty, Right = Empty, Level = lvl, ...} => (* prop1 *) lvl = 1
      | Node {Left = l, Right = r, Level = lvParent, ...} =>
           let
                val prop2 =
                        case l: tree k v of
                        | Node {Level = lvLChild, ...} => lvParent = 1 + lvLChild
                        | _ => True

                val prop3 =
                        case r: tree k v of
                        | Node {Level = lvRChild, ...} => lvParent - lvRChild <= 1
                        | _ => True

                val prop4 =
                        case r: tree k v of
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

fun valid [k][v] (_: ord k) (t: tree k v): bool = aaTreeProps t && propBST t
