(* ListMap *)

structure L = List
structure HL = HList
open HFunction

functor MkListMap(Q: sig
                         con key :: Type
                         con item :: Type
                         val eq_key: eq key
end): sig
con t :: Type -> Type -> Type

val empty : t Q.key Q.item

val null : t Q.key Q.item -> bool

val singleton : Q.key -> Q.item -> t Q.key Q.item

val size : t Q.key Q.item -> int

val insert : Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

val insertWith : (Q.item -> Q.item -> Q.item) -> Q.key -> Q.item -> t Q.key Q.item -> t Q.key Q.item

val adjust: (Q.item -> Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

val update: (Q.item -> option Q.item) -> Q.key -> t Q.key Q.item -> t Q.key Q.item

val delete : Q.key -> t Q.key Q.item -> t Q.key Q.item

val lookup : Q.key -> t Q.key Q.item -> option Q.item

val member : Q.key -> t Q.key Q.item -> bool

val fromList : list (Q.key * Q.item) -> t Q.key Q.item

val toList : t Q.key Q.item -> list (Q.key * Q.item)

val foldr : b ::: Type -> (Q.key * Q.item -> b -> b) -> b -> t Q.key Q.item -> b

val getAnyPair : t Q.key Q.item -> option (Q.key * Q.item)

val mapValues : item' ::: Type -> (Q.item -> item') -> t Q.key Q.item -> t Q.key item'

val exists : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

val all : (Q.key * Q.item -> bool) -> t Q.key Q.item -> bool

val find : (Q.key * Q.item -> bool) -> t Q.key Q.item -> option (Q.key * Q.item)

val valid : t Q.key Q.item -> bool

end = struct

open Q

datatype entry k v = Entry of k * v

type t k v = list (entry k v)

val eq_entry: eq (entry key item) =
   let fun eq' (e1: entry key item) (e2: entry key item) =
           let val Entry (k1, _) = e1
               val Entry (k2, _) = e2
           in k1 = k2
           end
   in mkEq eq'
   end

val show_entry  (_: show key) (_: show item): show (entry key item) =
   let fun show' (e1: entry key item) =
           case e1 of
             Entry (key, item) => "(" ^ show key ^ "," ^ show item ^ ")"
   in mkShow show'
   end

val empty : t key item = []

fun null  (t1: t key item): bool = HL.null t1

fun singleton  (k1: key) (v1: item): t key item = Entry (k1, v1) :: []

fun size  (t1: t key item): int = L.length t1

fun insertWith  (f: item -> item -> item) (k1: key) (v1: item) (li: t key item) : t key item =
    let fun insert' (li': t key item) (acc: t key item): t key item =
            case li' of
              | [] => Entry (k1, v1) :: li (* not found, push new entry *)
              | (y: entry key item) :: ys => (let val Entry (k0, v0) = y
                            in if k0 = k1
                                  then L.append (L.rev acc) (Entry (k0, (f v1 v0)) :: ys)
                                  else insert' ys (y :: acc)
                            end)
    in insert' li empty
    end


val insert: key -> item -> t key item -> t key item = insertWith const

fun delete  (k1: key) (li: t key item): t key item =

   let fun del' (li': t key item) (acc: t key item): t key item =
       case li' of
         | [] => li (* not found, returns original list *)
         | (y: entry key item) :: ys =>
             (let val Entry (k0, v0) = y
              in if k0 = k1
                    then List.append (List.rev acc) ys
                    else del' ys (y :: acc)
              end) 
   in del' li empty
   end

fun lookup  (k1: key) (li: t key item) : option item =
    case li of
      [] => None
      | (y: entry key item) :: ys => (let val Entry (k0, v0) = y
                   in if k0 = k1
                      then Some v0
                      else lookup k1 ys
                   end)

fun member (k1: key): t key item -> bool = lookup k1 >>> Option.isSome

fun fromList  (li: list (key * item)): t key item =
    L.mp Entry li

fun toList  (li: t key item): list (key * item) =
    let fun fromEntry (e: entry key item) =
             (let val Entry (k0, v0) = e
             in (k0, v0)
             end)  
    in L.mp fromEntry li
    end


fun adjust  (f: item -> item) (k1: key) (li: t key item) : t key item =
    let fun adjust' (li': t key item) (acc: t key item): t key item =
            case li' of
              | [] => li (* not found, return original *)
              | (y: entry key item) :: ys =>
                           (let val Entry (k0, v0) = y
                            in if k0 = k1
                                  then L.append (L.rev acc) (Entry (k0, (f v0)) :: ys)
                                  else adjust' ys (y :: acc)
                            end)
    in adjust' li empty
    end

fun update  (f: item -> option item) (k1: key) (li: t key item) : t key item =
    let fun update' (li': t key item) (acc: t key item): t key item =
            case li' of
              | [] => li (* not found, return original *)
              | (y: entry key item) :: ys =>
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


fun withEntry [b] (f: key * item -> b -> b) (e: entry key item) (z: b): b =
      let val Entry (k0, v0) = e
      in f (k0, v0) z
      end   

fun foldr [b] (myop: key * item -> b -> b): (b -> t key item -> b) =
      List.foldr (withEntry myop) 

fun getAnyPair  (li: t key item): option (key * item) =
      case li of
        [] => None
        | (Entry (k0, v0)) :: _ => Some (k0, v0)

fun mapValues [w] (f: item -> w) (li: t key item): t key w =
       let fun f' (e: entry key item): entry key w =
               let val Entry (key, item) = e
               in Entry (key, f item)
               end
       in
          List.mp f' li
       end

fun exists (prop: key * item -> bool) (li: t key item): bool =
    let fun prop' (e: entry key item): bool =
          let val Entry (k, v) = e
          in prop (k, v)
          end
    in
       List.exists prop' li
    end  

fun all (prop: key * item -> bool) (li: t key item): bool =
    let fun prop' (e: entry key item): bool =
          let val Entry (k, v) = e
          in prop (k, v)
          end
    in
       List.all prop' li
    end

fun find (prop: key * item -> bool) (li: t key item): option (key * item) =
     case li of
       | [] => None
       | (Entry (k0, v0)) :: es => if prop (k0, v0)
                                      then Some (k0, v0)
                                      else find prop es



(* invariants *)

fun propBucketKeysAreUnique  (li: t key item): bool = li = HL.nub li

val valid : (t key item -> bool) = propBucketKeysAreUnique

end