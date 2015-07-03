(* ListMap *)

structure L = List
structure HL = HList
open HFunction

functor MkListMap(Q: sig
                         con key :: Type
                         val eq_key: eq key
end): sig
con t :: Type -> Type

val empty : item ::: Type -> t item

val null : item ::: Type -> t item -> bool

val singleton : item ::: Type -> Q.key -> item -> t item

val size : item ::: Type -> t item -> int

val insert : item ::: Type -> Q.key -> item -> t item -> t item

val insertWith : item ::: Type -> (item -> item -> item) -> Q.key -> item -> t item -> t item

val adjust: item ::: Type -> (item -> item) -> Q.key -> t item -> t item

val update: item ::: Type -> (item -> option item) -> Q.key -> t item -> t item

val delete : item ::: Type -> Q.key -> t item -> t item

val lookup : item ::: Type -> Q.key -> t item -> option item

val member : item ::: Type -> Q.key -> t item -> bool

val fromList : item ::: Type -> list (Q.key * item) -> t item

val toList : item ::: Type -> t item -> list (Q.key * item)

val foldr : item ::: Type -> b ::: Type -> (Q.key * item -> b -> b) -> b -> t item -> b

val getAnyPair : item ::: Type -> t item -> option (Q.key * item)

val mapValues : item ::: Type -> item' ::: Type -> (item -> item') -> t item -> t item'

val exists : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

val all : item ::: Type -> (Q.key * item -> bool) -> t item -> bool

val find : item ::: Type -> (Q.key * item -> bool) -> t item -> option (Q.key * item)

val valid : item ::: Type -> t item -> bool

end = struct

open Q

datatype entry v = Entry of Q.key * v

type t v = list (entry v)

val eq_entry [item]: eq (entry item) =
   let fun eq' (e1: entry item) (e2: entry item) =
           let val Entry (k1, _) = e1
               val Entry (k2, _) = e2
           in k1 = k2
           end
   in mkEq eq'
   end

val show_entry [item]  (_: show Q.key) (_: show item): show (entry item) =
   let fun show' (e1: entry item) =
           case e1 of
             Entry (key, item) => "(" ^ show key ^ "," ^ show item ^ ")"
   in mkShow show'
   end

val empty [item] : t item = []

fun null [item] (t1: t item): bool = HL.null t1

fun singleton [item] (k1: Q.key) (v1: item): t item = Entry (k1, v1) :: []

fun size [item] (t1: t item): int = L.length t1

fun insertWith [item] (f: item -> item -> item) (k1: Q.key) (v1: item) (li: t item) : t item =
    let fun insert' (li': t item) (acc: t item): t item =
            case li' of
              | [] => Entry (k1, v1) :: li (* not found, push new entry *)
              | (y: entry item) :: ys => (let val Entry (k0, v0) = y
                            in if k0 = k1
                                  then L.append (L.rev acc) (Entry (k0, (f v1 v0)) :: ys)
                                  else insert' ys (y :: acc)
                            end)
    in insert' li empty
    end


val insert [item] : Q.key -> item -> t item -> t item = insertWith const

fun delete [item] (k1: Q.key) (li: t item): t item =

   let fun del' (li': t item) (acc: t item): t item =
       case li' of
         | [] => li (* not found, returns original list *)
         | (y: entry item) :: ys =>
             (let val Entry (k0, v0) = y
              in if k0 = k1
                    then List.append (List.rev acc) ys
                    else del' ys (y :: acc)
              end) 
   in del' li empty
   end

fun lookup [item] (k1: Q.key) (li: t item) : option item =
    case li of
      [] => None
      | (y: entry item) :: ys => (let val Entry (k0, v0) = y
                   in if k0 = k1
                      then Some v0
                      else lookup k1 ys
                   end)

fun member [item] (k1: Q.key): t item -> bool = lookup k1 >>> Option.isSome

fun fromList [item]  (li: list (Q.key * item)): t item =
    L.mp Entry li

fun toList [item] (li: t item): list (Q.key * item) =
    let fun fromEntry (e: entry item) =
             (let val Entry (k0, v0) = e
             in (k0, v0)
             end)  
    in L.mp fromEntry li
    end


fun adjust [item] (f: item -> item) (k1: Q.key) (li: t item) : t item =
    let fun adjust' (li': t item) (acc: t item): t item =
            case li' of
              | [] => li (* not found, return original *)
              | (y: entry item) :: ys =>
                           (let val Entry (k0, v0) = y
                            in if k0 = k1
                                  then L.append (L.rev acc) (Entry (k0, (f v0)) :: ys)
                                  else adjust' ys (y :: acc)
                            end)
    in adjust' li empty
    end

fun update [item] (f: item -> option item) (k1: Q.key) (li: t item) : t item =
    let fun update' (li': t item) (acc: t item): t item =
            case li' of
              | [] => li (* not found, return original *)
              | (y: entry item) :: ys =>
                       (let val Entry (k0, v0) = y
                        in if k0 <> k1
                                then update' ys (y :: acc)
                                else case f v0 of
                                       | Some v => (* update it *)
                                                 L.append (L.rev acc) (Entry (k0, v) :: ys)
                                       | None => (* delete it *)
                                                 L.append (L.rev acc) ys
                        end)
    in update' li empty
    end


fun withEntry [item] [b] (f: Q.key * item -> b -> b) (e: entry item) (z: b): b =
      let val Entry (k0, v0) = e
      in f (k0, v0) z
      end   

fun foldr [item] [b] (myop: Q.key * item -> b -> b): (b -> t item -> b) =
      List.foldr (withEntry myop) 

fun getAnyPair [item] (li: t item): option (Q.key * item) =
      case li of
        [] => None
        | (Entry (k0, v0)) :: _ => Some (k0, v0)

fun mapValues [item] [w] (f: item -> w) (li: t item): t w =
       let fun f' (e: entry item): entry w =
               let val Entry (key, item) = e
               in Entry (key, f item)
               end
       in
          List.mp f' li
       end

fun exists [item] (prop: Q.key * item -> bool) (li: t item): bool =
    let fun prop' (e: entry item): bool =
          let val Entry (k, v) = e
          in prop (k, v)
          end
    in
       List.exists prop' li
    end  

fun all [item] (prop: Q.key * item -> bool) (li: t item): bool =
    let fun prop' (e: entry item): bool =
          let val Entry (k, v) = e
          in prop (k, v)
          end
    in
       List.all prop' li
    end

fun find [item] (prop: Q.key * item -> bool) (li: t item): option (Q.key * item) =
     case li of
       | [] => None
       | (Entry (k0, v0)) :: es => if prop (k0, v0)
                                      then Some (k0, v0)
                                      else find prop es



(* invariants *)

fun propBucketKeysAreUnique [item] (li: t item): bool = li = HL.nub li

val valid [item] : (t item -> bool) = propBucketKeysAreUnique

end