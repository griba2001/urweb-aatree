structure HS = HString

val show_option [a] (_:show a): show (option a) =
        let fun show' (opt: option a) =
               case opt of
                  None => "None"
                  | Some v => HS.concat( "Some " :: show v :: [])
        in mkShow show'
        end
