
open HFunction
open HTuple
open HOrd
structure HS = HString
structure HL = HList

datatype tree k v = Empty | Node of {Key: k,
                                     Value: v,
                                     Level: int,
                                     Left: tree k v,
                                     Right: tree k v}

(* eq instance *)
val eq_tree = fn [k][v] (_ : eq k) (_ : eq v) =>
        let
                fun eq' (t1: tree k v) (t2: tree k v) =
                   case (t1, t2) of
                        (Empty, Empty) => True
                        | (Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...},
                           Node {Key = k2, Value = v2, Left = lt2, Right = rt2, ...}) =>
                                  k1 = k2 && v1 = v2 && eq' lt1 lt2 && eq' rt1 rt2
                        | _ => False
        in mkEq eq'
        end

val show_tree = fn [k][v] (_ : show k) (_ : show v) =>
        let
           fun show' (t: tree k v): string =
              case t of
                Empty => "Empty"
                | Node {Key = k1, Value = v1, Left = lt1, Right = rt1, ...} =>
                    HS.concat ("[" :: show' lt1 :: "," :: show (k1, v1) :: "," :: show' rt1 :: "]" :: [])
        in mkShow show'
        end        
                
(* Unused
 
fun setKey [k][v] (v1: k) (t: tree k v) : tree k v =
    case t of
        Node r => Node (r -- #Key ++ {Key = v1})
        | _ => error <xml>setKey: not a Node</xml>
*)

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
     Empty => 0
     | Node {Level = lvl, ...} => lvl

val empty [k][v] : tree k v = Empty

fun null [k][v] (t: tree k v): bool =
    case t of
        Empty => True
        | _ => False

fun singleton [k][v] (k1: k) (v1: v): tree k v = Node {Key = k1, Value = v1, Level = 1, Left = Empty, Right = Empty}

fun size [k][v] (t: tree k v) : int =
    case t of
     Empty => 0
     | Node {Left = l, Right = r, ...} => 1 + size l + size r

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
        Empty => Empty
        | Node {Right = r, ...} => setRight (split r) t

(* Haskell
skewRight Empty = Empty
skewRight (Node x lv l r) = Node x lv l (skew r)
*)

fun skewRight [k][v] (t: tree k v): tree k v =
    case t of
        Empty => Empty
        | Node {Right = r, ...} => setRight (skew r) t

(* Haskell
skewRightRight (Node x lv l (Node x' lv' l' r')) = Node x lv l (Node x' lv' l' (skew r'))
skewRightRight t = t
*)

fun skewRightRight [k][v] (t: tree k v): tree k v =
    case t of
        Empty => Empty
        | Node {Right = r, ...} =>
          (case r of
             Node {Right = s, ...} => let val r' : tree k v = setRight (skew s) r
                                          val t' : tree k v = setRight r' t
                                      in t' end
             | _ => t)
    | _ => t

(* Haskell

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
      Empty => Empty
      | Node {Level = lvP, Left = l, Right = r, ...} =>
          case r of
            Empty => let val should_be = 1 + getLevel l
                     in if lvP > should_be
                           then setLevel should_be t
                           else t
                     end
            | Node {Level = lvR, ...} =>
                     let val should_be = 1 + min (getLevel l) (getLevel r)
                     in if lvP > should_be
                        then let val r' : tree k v = if lvR > should_be
                                            then setLevel should_be r
                                            else r
                                in setRight r' (setLevel should_be t)
                                end
                        else t
                     end 

(* Haskell
rebalance = decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight
*)
val rebalance [k][v] : (tree k v -> tree k v) =
    andThen decreaseLevel (andThen skew (andThen skewRight (andThen skewRightRight (andThen split splitRight))))


(* Haskell:

insert x Empty = singleton x
insert x (Node y lv l r) = case compare x y of
        LT -> Node y lv (insert x l) r .$ skew .$ split
        GT -> Node y lv l (insert x r) .$ skew .$ split
        EQ -> Node x lv l r
*)


fun insertWith [k][v] (_: ord k) (f: v -> v -> v) (k1: k) (v1: v) (t: tree k v): tree k v =
    case t of
        Empty => singleton k1 v1
        | Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => split (skew (setLeft (insertWith f k1 v1 l) t))
              | GT => split (skew (setRight (insertWith f k1 v1 r) t))
              | EQ => setKeyAndValue k1 (f v1 v0) t
              )

val insert [k][v] (_: ord k) (k1: k) (v1: v):  (tree k v -> tree k v) = insertWith const k1 v1

fun adjust [k][v] (_: ord k) (f: v -> v) (k1: k) (t: tree k v): tree k v =
    case t of
        Empty => t
        | Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
           (case compare k1 k0 of
              LT => split (skew (setLeft (adjust f k1 l) t))
              | GT => split (skew (setRight (adjust f k1 r) t))
              | EQ => setValue (f v0) t
              )



(* Haskell
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

(* Haskell
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
        Empty => Empty
        | Node {Key = k0, Left = l, Right = r, ...} =>
           case compare k1 k0 of
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

fun lookup [k][v] (_: ord k) (k1: k) (t: tree k v): option v =
    case t of
        Empty => None
        | Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
            (case compare k1 k0 of
                EQ => Some v0
                | LT => lookup k1 l
                | GT => lookup k1 r
                ) 

(* toList' with prepend style *)

fun toList' [k][v] (t: tree k v) (li: list (k*v)): list (k * v) =
    case t of
      Empty => li
      | Node {Key = k0, Value = v0, Left = l, Right = r, ...} =>
          case (l: tree k v, r: tree k v) of
                (Empty, Empty) => (k0, v0) :: li
                | _ =>  toList' l ((k0, v0) :: toList' r li)

fun toList [k][v] (t: tree k v): list (k * v) = toList' t []            

fun fromList [k][v] (_ : ord k) (li: list (k * v)): tree k v = List.foldl (uncurry insert) empty li

fun mapValues [k][v][w] (f: v -> w) (t: tree k v): tree k w =
       case t of
         Empty => Empty
         | Node rc => Node (rc -- #Value -- #Left -- #Right ++ {Value = f rc.Value,
                                                                Left = mapValues f rc.Left,
                                                                Right = mapValues f rc.Right})

fun mapKeysMonotonic [k][v][k'] (f: k -> k') (t: tree k v): tree k' v =
       case t of
         Empty => Empty
         | Node rc => Node (rc -- #Key -- #Left -- #Right ++ {Key = f rc.Key,
                                                                Left = mapKeysMonotonic f rc.Left,
                                                                Right = mapKeysMonotonic f rc.Right})

