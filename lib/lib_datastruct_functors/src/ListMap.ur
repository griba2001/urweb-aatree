(* ListMap *)

signature LIST_MAP = sig
  include Common.DSMAP
  val propBucketKeysAreUnique: item ::: Type -> t item -> bool
end


structure L = List
structure HL = HList
open HFunction

functor MkListMap(Q: sig
                         con key :: Type
                         val eq_key: eq key
end): (LIST_MAP where con key = Q.key) = struct

open Q
open HTuple

(* I use a specific entry datatype to define a specific equality
to prevent key multiplicity

Two entries are equal if its keys are equal

So I can use equality to detect an existing key in the ListMap
 *)

datatype entry v = Entry of key * v

type t v = list (entry v)

val fromEntry[v]: entry v -> key * v = fn (Entry (k0, v0)) => (k0, v0)

val eq_entry [item]: eq (entry item) =
   let fun eq' (e1: entry item) (e2: entry item) =
           let val Entry (k1, _) = e1
               val Entry (k2, _) = e2
           in k1 = k2
           end
   in mkEq eq'
   end

val show_entry [item]  (_: show key) (_: show item): show (entry item) =
   let fun show' (e1: entry item) =
           case e1 of
             Entry (key, item) => "(" ^ show key ^ "," ^ show item ^ ")"
   in mkShow show'
   end

val empty [item] : t item = []

val null [item]: t item -> bool = HL.null

fun singleton [item] (k1: key) (v1: item): t item = Entry (k1, v1) :: []

val size [item]: t item -> int = L.length

fun insertWith [item] (f: item -> item -> item) (k1: key) (v1: item) (li: t item) : t item =
    let
       insert' li empty
    where
       fun insert' (li': t item) (acc: t item): t item =
            case li' of
              | [] => Entry (k1, v1) :: li (* not found, push new entry *)
              | (y: entry item) :: ys => (let val Entry (k0, v0) = y
                            in if k0 <> k1
                                  then (* push into acc *) insert' ys (y :: acc)
                                  else (* found *) L.revAppend acc <| Entry (k0, (f v1 v0)) :: ys
                            end)
    
    end

val insert [item] : key -> item -> t item -> t item = insertWith const

fun fromList [item]  (li: list (key * item)): t item = List.foldl (uncurry insert) empty li

fun delete [item] (k1: key) (li: t item): t item =

   let
      del' li empty
   where
       fun del' (li': t item) (acc: t item): t item =
       case li' of
         | [] => li (* not found, returns original list *)
         | (y: entry item) :: ys =>
             (let val Entry (k0, v0) = y
              in if k0 <> k1
                    then (* push into acc *) del' ys (y :: acc)
                    else (* found *) L.revAppend acc ys
              end) 
   end

fun lookup [item] (k1: key) (li: t item) : option item =
    case li of
      [] => None
      | (y: entry item) :: ys => (let val Entry (k0, v0) = y
                   in if k0 <> k1
                      then lookup k1 ys
                      else (* found *) Some v0
                   end)

fun member [item] (k1: key): t item -> bool = lookup k1 >>> Option.isSome

fun toList [item] (li: t item): list (key * item) = L.mp fromEntry li

fun update [item] (f: item -> option item) (k1: key) (li: t item) : t item =
    let
       update' li empty
    where
        fun update' (li': t item) (acc: t item): t item =
            case li' of
              | [] => li (* not found, return original *)
              | (y: entry item) :: ys =>
                       (let val Entry (k0, v0) = y
                        in if k0 <> k1 
                                then (* push into acc *) update' ys (y :: acc)
                                else (* found *) case f v0 of
                                       | Some v => (* adjust it *)
                                                 L.revAppend acc (Entry (k0, v) :: ys) 
                                       | None => (* delete it *)
                                                 L.revAppend acc ys

                        end)
    end

val adjust [item] (f: item -> item): key -> t item -> t item = update (Some <<< f)


(* Convert function (key * item -> w) to (entry item -> w) *)
fun withEntry [item] [w] (f: key * item -> w) (e: entry item): w = f (fromEntry e)


fun foldr [item] [b] (myop: key * item -> b -> b): (b -> t item -> b) =

      List.foldr (withEntry myop)


fun getAnyPair [item] (li: t item): option (key * item) =
      case li of
        [] => None
        | e :: _ => (Some <<< fromEntry) e

fun mapValues [item] [w] (f: item -> w) (li: t item): t w =

     List.mp (fromEntry >>> HTuple.fmap f >>> Entry) li


fun exists [item] (prop: key * item -> bool) (li: t item): bool = List.exists (withEntry prop) li

fun all [item] (prop: key * item -> bool) (li: t item): bool = List.all (withEntry prop) li

fun find [item] (prop: key * item -> bool) (li: t item): option (key * item) =

           List.find (withEntry prop) li |> Option.mp fromEntry

(* invariants *)

fun propBucketKeysAreUnique [item] (li: t item): bool = li = HL.nub li

val valid [item] : (t item -> bool) = propBucketKeysAreUnique

end