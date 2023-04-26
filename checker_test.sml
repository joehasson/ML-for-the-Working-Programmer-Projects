use "checker.sml";

 (* Axioms from Mendelson *)
val test1 = is_tautology (ifthen (Atom "B", ifthen(Atom "C", Atom "B")))
val test2 = is_tautology (ifthen (ifthen (Atom "B", ifthen (Atom "C", Atom "D"))),
                                 (ifthen (ifthen (Atom "B", Atom "C"), ifthen (Atom "B", Atom "D"))))
val test3 = is_tautology (ifthen (Neg (Atom "C"), Neg (Atom "B")), 
                                 ifthen (ifthen (Neg (Atom "C"), Atom "B"), Atom "C"))

(* Validity of modus ponens and EFQ *)
val test4 = is_valid ([Atom "A", ifthen (Atom "A", Atom "B")], Atom "B")
val test5 = is_valid ([Conj (Atom "A", Neg (Atom "A"))], Atom "B")

(* Affirming the consequent and denying the antecedent are invalid *)
val test6 = not (is_valid ([Atom "B", ifthen (Atom "A", Atom "B")], Atom "A"))
val test7 = not (is_valid ([Neg (Atom "A"), ifthen (Atom "A", Atom "B")], Neg (Atom "B")))
