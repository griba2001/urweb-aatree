(* ListBucket *)

structure L = List
structure HL = HList
open HFunction

datatype entry k v = Entry of k * v

type t k v = list (entry k v)

val eq_entry [k][v] (_: eq k): eq (entry k v) =
   let fun eq' (e1: entry k v) (e2: entry k v) =
           let val Entry (k1, _) = e1
               val Entry (k2, _) = e2
           in k1 = k2
           end
   in mkEq eq'
   end

val show_entry [k][v] (_: show k) (_: show v): show (entry k v) =
   let fun show' (e1: entry k v) =
           case e1 of
             Entry (k, v) => "(" ^ show k ^ "," ^ show v ^ ")"
   in mkShow show'
   end

val empty [k][v]: t k v = [] 

fun null [k][v] (t1: t k v): bool = HL.null t1 

fun singleton [k][v] (k1: k) (v1: v): t k v = Entry (k1, v1) :: []

fun size [k][v] (t1: t k v): int = L.length t1

fun adjust [k][v] (_:eq k) (f: v -> v) (k1: k) (li: t k v) : t k v =
    let fun adjust' (li': t k v) (acc: t k v): t k v =
            case li' of
              | [] => li (* not found, return original *)
              | (y: entry k v) :: ys => (let val Entry (k0, v0) = y
                            in if k0 = k1
                                  then L.append (L.rev acc) (Entry (k0, (f v0)) :: ys)
                                  else adjust' ys (y :: acc)
                            end)
    in adjust' li empty
    end
 

fun insertWith [k][v] (_:eq k) (f: v -> v -> v) (k1: k) (v1: v) (li: t k v) : t k v =
    let fun insert' (li': t k v) (acc: t k v): t k v =
            case li' of
              | [] => Entry (k1, v1) :: li (* not found, push new entry *)
              | (y: entry k v) :: ys => (let val Entry (k0, v0) = y
                            in if k0 = k1
                                  then L.append (L.rev acc) (Entry (k0, (f v1 v0)) :: ys)
                                  else insert' ys (y :: acc)
                            end)
    in insert' li empty
    end


fun insert [k][v]  (_:eq k): k -> v -> t k v -> t k v = insertWith const

fun delete [k][v] (_: eq k) (k1: k) (li: t k v): t k v =

   let fun del' (li': t k v) (acc: t k v): t k v =
       case li' of
         | [] => li (* not found, returns original list *)
         | (y: entry k v) :: ys =>
             (let val Entry (k0, v0) = y
              in if k0 = k1
                    then List.append (List.rev acc) ys
                    else del' ys (y :: acc)
              end) 
   in del' li empty
   end

fun lookup [k][v] (_:eq k) (k1: k) (li: t k v) : option v =
    case li of
      [] => None
      | (y: entry k v) :: ys => (let val Entry (k0, v0) = y
                   in if k0 = k1
                      then Some v0
                      else lookup k1 ys
                   end)

fun fromList [k][v] (_:eq k) (li: list (k * v)): t k v =
    L.mp Entry li

fun toList [k][v] (_:eq k) (li: t k v): list (k * v) =
    let fun fromEntry (e: entry k v) =
             (let val Entry (k0, v0) = e
             in (k0, v0)
             end)  
    in L.mp fromEntry li
    end


fun withEntry [k][v][b] (f: k * v -> b -> b) (e: entry k v) (z: b): b =
      let val Entry (k0, v0) = e
      in f (k0, v0) z
      end   

fun foldr [k][v][b] (myop: k * v -> b -> b): (b -> t k v -> b) =
      List.foldr (withEntry myop) 

fun getAnyPair [k][v] (li: t k v): option (k * v) =
      case li of
        [] => None
        | (Entry (k0, v0)) :: _ => Some (k0, v0)

fun mapValues [k][v][w] (f: v -> w) (li: t k v): t k w =
       let fun f' (e: entry k v): entry k w =
               let val Entry (k, v) = e
               in Entry (k, f v)
               end
       in
          List.mp f' li
       end   

(* invariants *)

fun propBucketKeysAreUnique [k][v] (_:eq k) (li: t k v): bool = li = HL.nub li

fun valid [k][v] (_:eq k): (t k v -> bool) = propBucketKeysAreUnique