
open HFunction
open HOrd
structure D = HDList

datatype tree a = Empty | Node of {Value: a, Level: int, Left: tree a, Right: tree a}

(* eq instance *)
val eq_tree = fn [a] (_ : eq a) =>
        let
                fun eq' (t1: tree a) (t2: tree a) =
                   case (t1, t2) of
                        (Empty, Empty) => True
                        | (Node {Value = x1, Level = l1, Left = lt1, Right = rt1},
                           Node {Value = x2, Level = l2, Left = lt2, Right = rt2}) =>
                                  x1 = x2 && l1 = l2 && eq' lt1 lt2 && eq' rt1 rt2
                        | _ => False
        in mkEq eq'
        end


fun setValue [a] (v: a) (t: tree a) : tree a =
   case t of
     Node r => Node (r -- #Value ++ {Value = v})
     | _ => error <xml>setNodeField: not a Node</xml>

fun setLevel [a] (v: int) (t: tree a) : tree a =
   case t of
     Node r => Node (r -- #Level ++ {Level = v})
     | _ => error <xml>setNodeField: not a Node</xml>

fun setLeft [a] (v: tree a) (t: tree a) : tree a =
   case t of
     Node r => Node (r -- #Left ++ {Left = v})
     | _ => error <xml>setNodeField: not a Node</xml>

fun setRight [a] (v: tree a) (t: tree a) : tree a =
   case t of
     Node r => Node (r -- #Right ++ {Right = v})
     | _ => error <xml>setNodeField: not a Node</xml>

fun getLevel [a] (t: tree a) : int =
   case t of
     Empty => 0
     | Node {Level = lvl, ...} => lvl

val empty [a] : tree a = Empty

fun null [a] (t: tree a) = case t of
                               Empty => True
                               | _ => False

fun singleton [a] (x: a) = Node { Value = x, Level = 1, Left = Empty, Right = Empty}

(* skew: right rotation *)
fun skew [a] (t: tree a) : tree a =
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

fun split [a] (t: tree a) : tree a =
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

fun splitRight [a] (t: tree a): tree a =
    case t of
        Empty => Empty
        | Node {Right = r, ...} => setRight (split r) t

(* Haskell
skewRight Empty = Empty
skewRight (Node x lv l r) = Node x lv l (skew r)
*)

fun skewRight [a] (t: tree a): tree a =
    case t of
        Empty => Empty
        | Node {Right = r, ...} => setRight (skew r) t

(* Haskell
skewRightRight (Node x lv l (Node x' lv' l' r')) = Node x lv l (Node x' lv' l' (skew r'))
skewRightRight t = t
*)

fun skewRightRight [a] (t: tree a): tree a =
    case t of
        Empty => Empty
        | Node {Right = r, ...} =>
          (case r of
             Node {Right = s, ...} => let val r' = setRight (skew s) r
                                          val t' = setRight r' t
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

fun decreaseLevel [a] (t: tree a): tree a =
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
                        then let val r' = if lvR > should_be
                                            then setLevel should_be r
                                            else r
                                in setRight r' (setLevel should_be t)
                                end
                        else t
                     end 

(* Haskell
rebalance = decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight
*)
fun rebalance [a] (t: tree a): tree a =
    andThen decreaseLevel (andThen skew (andThen skewRight (andThen skewRightRight (andThen split splitRight)))) t


(* Haskell:

insert x Empty = singleton x
insert x (Node y lv l r) = case compare x y of
        LT -> Node y lv (insert x l) r .$ skew .$ split
        GT -> Node y lv l (insert x r) .$ skew .$ split
        EQ -> Node x lv l r
*)

fun insert [a] (_: ord a) (x: a) (t: tree a): tree a =
    case t of
        Empty => singleton x
        | Node {Value = y, Left = l, Right = r, ...} =>
           (case compare x y of
              LT => split (skew (setLeft (insert x l) t))
              | GT => split (skew (setRight (insert x r) t))
              | EQ => setValue x t
              )

(* Haskell
minimum (Node x _ Empty _) = x
minimum (Node x _ l _) = minimum l
minimum Empty = error "minimum: empty tree"

maximum (Node x _ _ Empty) = x
maximum (Node x _ _ r) = maximum r
maximum Empty = error "maximum: empty tree"
*)

fun minimum [a] (t: tree a): a =
    case t of
        Node {Value = x, Left = l, ...} =>
            (case l of
               Empty => x
               | _ => minimum l
               )
        | Empty => error <xml>aatree minimum: empty tree</xml>

fun maximum [a] (t: tree a): a =
    case t of
        Node {Value = x, Right = r, ...} =>
            (case r of
               Empty => x
               | _ => maximum r
               )
        | Empty => error <xml>aatree maximum: empty tree</xml>

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

fun delete [a] (_: ord a) (x: a) (t: tree a): tree a =
    case t of
        Empty => Empty
        | Node {Value = y, Left = l, Right = r, ...} =>
           case compare x y of
              LT => rebalance (setLeft (delete x l) t)
              | GT => rebalance (setRight (delete x r) t)
              | EQ => (case (l, r) of
                         (Empty, Empty) => Empty  (* deleted *)
                         | (Empty, _) => let val successor = minimum r
                                       in rebalance (setValue successor (setRight (delete successor r) t))
                                       end
                         | (_, _) => let val predecessor = maximum l
                                   in rebalance (setValue predecessor (setLeft (delete predecessor l) t))
                                   end
                         )

fun lookup [a] (_: ord a) (x: a) (t: tree a): option a =
    case t of
        Empty => None
        | Node {Value = y, Left = l, Right = r, ...} =>
            (case compare x y of
                EQ => Some y
                | LT => lookup x l
                | GT => lookup x r
                ) 

(* Haskell
toDList :: Tree a -> D.DList a
toDList Empty = D.empty
toDList (Node x _ Empty Empty) = D.singleton x
toDList (Node x _ l r) = toDList l `D.append` D.singleton x `D.append` toDList r
*)

fun toDList [a] (t: tree a): D.dlist a =
    case t of
        Empty => D.empty
        | Node {Value = x, Left = l, Right = r, ...} =>
             (case (l, r) of
                (Empty, Empty) => D.singleton x
                | _ => D.append (toDList l) (D.append (D.singleton x) (toDList r))
                )

val toList [a] :(tree a -> list a) = compose D.toList toDList

fun fromList [a] (_ : ord a) (li: list a): tree a = List.foldl insert empty li

