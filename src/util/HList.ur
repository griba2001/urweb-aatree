(* HList *)

fun null [a] (_:eq a) (li: list a) = li = Nil

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

fun elem [a] (_: eq a) (x: a) (li: list a): bool =
    case li of
      [] => False
      | y :: ys => x = y || elem x ys

fun nub [a] (_: eq a) (li: list a): list a =
   let fun nub' li' ls =
       case li' of
         x :: xs => if elem x ls
                      then nub' xs ls
                      else x :: nub' xs (x :: ls)
         | [] => []
   in nub' li []
   end
