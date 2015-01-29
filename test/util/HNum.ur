(* HNum *)

fun abs [a] (_:num a) (_:ord a) (x: a): a = if x < zero then (-x) else x

(*
fun signum [a] (_:num a) (_:ord a) (x: a): a =
    case compare x zero of
        LT => -one
        | EQ => zero
        | GT => one

*)