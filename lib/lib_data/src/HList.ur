(* HList *)

fun null [a] (li: list a) = (* li = Nil *)
   case li of
     [] => True
     | _ => False

fun singleton [a] (x: a) = x :: []

fun concat [a] (xss: list (list a)) = List.foldr List.append [] xss

fun partition [a] (p: a -> bool) (ls: list a) : list a * list a =
    let
        fun part (acc1: list a) (acc2: list a) (ls: list a) =
                case ls of
                [] => (List.rev acc1, List.rev acc2)
                | x :: xs => if p x
                        then part (x :: acc1) acc2 xs
                        else part acc1 (x :: acc2) xs     
    in
        part [] [] ls
    end

fun elem [a] (_: eq a) (x: a) (li: list a): bool =  List.exists (eq x) li
    (*
    case li of
      [] => False
      | y :: ys => x = y || elem x ys
     *) 

fun nub [a] (_: eq a) (li: list a): list a =
   let fun nub' li' ls =
       case li' of
         x :: xs => if elem x ls
                      then nub' xs ls
                      else x :: nub' xs (x :: ls)
         | [] => []
   in nub' li []
   end

fun delete [a] (_: eq a) (x: a) (li: list a): list a =
   let fun del' (li': list a) (acc: list a) =
       case li' of
         | [] => li (* not found, returns original list *)
         | y :: ys => if y = x
                        then List.append (List.rev acc) ys
                        else del' ys (y :: acc)
   in del' li []
   end

fun zip [a][b] (xs: list a) (ys: list b): list (a * b) =
      let fun zip' (xs': list a) (ys': list b) (acc: list (a * b)): list (a * b) =
          case (xs', ys') of
             (x :: xs'', y :: ys'') => zip' xs'' ys'' ((x, y) :: acc)
             | _ => List.rev acc
      in
         zip' xs ys []
      end   