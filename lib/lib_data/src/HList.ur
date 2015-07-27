(* HList *)

fun null [a] (li: list a) = (* li = Nil *)
   case li of
     [] => True
     | _ => False

fun singleton [a] (x: a) = x :: []

fun concat [a] (xss: list (list a)) = List.foldr List.append [] xss

fun partition [a] (p: a -> bool) (ls: list a) : list a * list a =
    let
       part [] [] ls
    where
       fun part (acc1: list a) (acc2: list a) (ls: list a) =
                case ls of
                [] => (List.rev acc1, List.rev acc2)
                | x :: xs => if p x
                        then part (x :: acc1) acc2 xs
                        else part acc1 (x :: acc2) xs     
    end

fun elem [a] (_: eq a) (x: a) (li: list a): bool =  List.exists (eq x) li

fun nub [a] (_: eq a) (li: list a): list a =
   let
      nub' [] li 
   where
      fun nub' acc li' =
       case li' of
         | [] => List.rev acc
         | x :: xs => if elem x acc
                      then nub' acc xs
                      else nub' (x :: acc) xs
   end

fun delete [a] (_: eq a) (x: a) (li: list a): list a =
   let
      del' li []
   where
      fun del' (li': list a) (acc: list a) =
       case li' of
         | [] => li (* not found, returns original list *)
         | y :: ys => if y = x
                        then List.append (List.rev acc) ys
                        else del' ys (y :: acc)
   end

fun zip [a][b] (xs: list a) (ys: list b): list (a * b) =
      let
         zip' xs ys []
      where
         fun zip' (xs': list a) (ys': list b) (acc: list (a * b)): list (a * b) =
          case (xs', ys') of
             (x :: xs'', y :: ys'') => zip' xs'' ys'' ((x, y) :: acc)
             | _ => List.rev acc
      end

fun unzip [a][b] (li: list (a * b)): (list a) * (list b) =
      let
          unzip' [] [] li
      where
         fun unzip' (acc_xs: list a) (acc_ys: list b) (li: list (a * b)) =
            case li of
              | [] => (List.rev acc_xs, List.rev acc_ys)
              | (x, y) :: rest => unzip' (x :: acc_xs) (y :: acc_ys) rest
      end       
