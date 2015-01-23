structure F = HFunction

datatype test = Test of transaction xbody | TestList of list test

(* class *)
structure Testable : sig
     class testable
     val mkTestable : t ::: Type -> (t -> test) -> testable t
     val testIt : t ::: Type -> testable t -> t -> test
end = struct
     type testable t = t -> test
     fun mkTestable [t] (f : t -> test) = f
     fun testIt [t] (f : testable t) = f
end

open Testable

(* instances *)
val testable_test: testable test = mkTestable F.id

val testable_testList [t] (_: testable t): testable (list t) =
      let
           fun testIt' (t1: list t): test =
                TestList (List.mp testIt t1)
      in mkTestable testIt'
      end
