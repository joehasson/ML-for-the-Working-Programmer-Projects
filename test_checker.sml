use "checker.sml";
open Prop;

fun is_nnf (Atom s) = true
  | is_nnf (Neg (Atom _)) = true
  | is_nnf (Disj (p, q)) = is_nnf p andalso is_nnf q
  | is_nnf (Conj (p, q)) = is_nnf p andalso is_nnf q
  | is_nnf (Neg p) = false

fun is_cnf (Conj (p,q)) =(is_cnf p) andalso (is_cnf q)
  | is_cnf p =
      let fun no_conj (Atom _) = true
            | no_conj (Neg p) = no_conj p
            | no_conj (Disj (p,q)) = (no_conj p) andalso (no_conj q)
            | no_conj (Conj _) = false
      in no_conj p end

(* Testing nnf*)

val test0_nnf = nnf (Neg (Disj (Atom "A", Atom "B")))
val test0_nnf_s = to_string test0_nnf
val test1_nnf = nnf (Disj (Conj (Neg (Atom "A"),Neg (Atom "B")), Neg (Atom "A")))
val test1_nnf_s = to_string test1_nnf

(*Testing nnf to cnf *)

val test0_nnf_to_cnf = nnf_to_cnf (Disj(test0_nnf, test0_nnf))
val test0_s = to_string test0_nnf_to_cnf 

val test_1_nnf_to_cnf = nnf_to_cnf (test1_nnf)
val test1_s = to_string test_1_nnf_to_cnf

(* Testing is_tautology: Axioms from Mendelson's intro to Mathematical Logic *)

val mendelson1 = ifthen (Atom "B", ifthen (Atom "C", Atom "B"))
val mendelson2 = ifthen(
                  ifthen (Atom "B", ifthen (Atom "C", Atom "D")),
                  ifthen (ifthen (Atom "B", Atom "C"), ifthen (Atom "B", Atom "D")))
val mendelson3 = ifthen (
                   ifthen (Neg (Atom "C"), Neg (Atom "B")),
                   ifthen (ifthen (Neg (Atom "C"), Atom "B"), Atom "C"))

val mendelson2_cnf = (nnf_to_cnf (nnf mendelson2))
val mendelson2_cnfs = to_string mendelson2_cnf 
val bar = is_nnf mendelson2_cnf
val foo = is_cnf mendelson2_cnf 
          
val is_tautology_1 = is_tautology mendelson1
val is_tautology_2 = is_tautology mendelson2
val is_tautology_3 = is_tautology mendelson3
val is_tautology_4 = not (is_tautology (Atom "A"))
val is_tautology_5 = not (is_tautology (Disj (Atom "A", Atom "B")))

(* Testing is_valid *)
(* Contradiction implies everything *)
val is_valid1 = is_valid ([Atom "A", Neg (Atom "A")], Atom "B")

(* Modus ponens *)
val is_valid2 = is_valid ([Atom "A", ifthen (Atom "A", Atom "B")], Atom "B")

(* Modus tollens *)
val is_valid3 = 
  is_valid ([Neg (Atom "B"), ifthen (Atom "A", Atom "B")], Neg (Atom "A"))

(* Affirming the consequent is invalid *)
val is_valid4 = not (is_valid ([Atom "B", ifthen (Atom "A", Atom "B")], Atom "A"))

(* Denying the antecedent is invalid *)
val is_valid5 = 
  not (is_valid ([Neg (Atom "A"), ifthen (Atom "A", Atom "B")], Neg (Atom "B")))

(* Testing is_contradiction *)
val is_contradiction_0 = is_contradiction (Conj (Atom "A", Neg (Atom "A")))
val is_contradiction_1 = is_contradiction (Neg mendelson1)
val is_contradiction_2 = is_contradiction (Neg mendelson2)
val is_contradiction_3 = is_contradiction (Neg mendelson3)
val is_contradiction_4 = not (is_contradiction (Atom "A"))
val is_contradiction_5 = not (is_contradiction (Conj (Atom "A", Neg (Atom "B"))))
val is_contradiction_6 = not (is_contradiction (Disj (Atom "A", Atom "B")))
