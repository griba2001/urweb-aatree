(* type assertion *)

datatype test = Test of transaction xbody | TestList of list test

(* class *)
structure Testable : sig
     class testable
     val mkTestable : t ::: Type -> (t -> test) -> testable t
     val testIt : t ::: Type -> testable t -> t -> test
end

val testable_test: Testable.testable test
val testable_testList : t ::: Type -> Testable.testable t -> Testable.testable (list t)
