(* HOption *)

structure HS = HString

val show_option [a] (_:show a): show (option a) =
        let fun show' (opt: option a) =
               case opt of
                  None => "None"
                  | Some v => HS.concat( "Some " :: show v :: [])
        in mkShow show'
        end

val optTest [a] (f: a -> bool) (x: a): option a =
        if f x then Some x
               else None

val optAlternative [a] (x: option a) (lazy_y: unit -> option a): option a =
        case x of
          | Some _ => x
          | None => lazy_y ()

fun option [a] [b] (def: b) (f: a -> b) (x: option a): b =
        case x of
          None => def
          | Some v => f v
    