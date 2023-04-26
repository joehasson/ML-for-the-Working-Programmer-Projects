use "set.sml";

infix !>
fun (x !> f) = f x

signature PROP = sig
  type t
  val to_string: t -> string
  val is_tautology: t -> bool
  val is_valid: t list * t -> bool
end


structure Prop = struct
  exception FormError
  exception Contradiction

  datatype t = Atom of string 
                | Neg of t 
                | Disj of (t * t) 
                | Conj of (t * t)

  fun to_string (Atom s) = s
    | to_string (Neg p) = "~" ^ (to_string p)
    | to_string (Disj (Neg p, q)) = "(" ^ (to_string p) ^ " -> " ^ (to_string q) ^ ")"  
    | to_string (Disj (p, q)) = "(" ^ (to_string p) ^ " V " ^ (to_string q) ^ ")"
    | to_string (Conj (p, q)) = "(" ^ (to_string p) ^ " & " ^ (to_string q) ^ ")"

  structure PropOrder : MEM = struct
    type t = t
    fun compare (p, q) = String.compare (to_string p, to_string q)
  end

  structure PropSet = Set(PropOrder)

  fun ifthen (p, q) = Disj (Neg p, q)
  
  (* Put a formula into negation normal form *)
  fun nnf (Neg (Neg p)) = nnf p
    | nnf (Neg (Conj (p, q))) = Disj (nnf (Neg p), nnf (Neg q))
    | nnf (Neg (Disj (p, q))) = Conj (nnf (Neg p), nnf (Neg q))
    | nnf (Disj (p, q)) = Disj (nnf p, nnf q)
    | nnf (Conj (p, q)) = Conj (nnf p, nnf q)
    | nnf (Neg p) = Neg (nnf p)
    | nnf (Atom s) = Atom s

  (* Put a formula in negation normal form into cnf *)
  fun nnf_to_cnf (Disj (p, Conj (q, r))) = 
        Conj (nnf_to_cnf (Disj (p, q)), nnf_to_cnf (Disj (p, r)))
    | nnf_to_cnf (Disj (Conj (p, q), r)) = 
        Conj (nnf_to_cnf (Disj (p, r)), nnf_to_cnf (Disj (q, r)))
    | nnf_to_cnf (Conj (p, q)) = Conj (nnf_to_cnf p, nnf_to_cnf q)
    | nnf_to_cnf (Disj (p, q)) = Disj (nnf_to_cnf p, nnf_to_cnf q)
    | nnf_to_cnf (Neg p) = Neg (nnf_to_cnf p)
    | nnf_to_cnf (Atom s) = Atom s

  (* Checker whether a formula in cnf is a tautology *)
  fun check_cnf p = false
(* Check whether an arbitary formula is a tautology by converting to cnf *) fun is_tautology p = p !> nnf !> nnf_to_cnf !> check_cnf (* Check whether an argument with arguments args and conclusion p is valid *)
  fun is_valid (args, p) =
    (*By the deduction theorem for propositional logic, An argument 
     * A1,..,An THEREFORE B is valid iff the proposition A1 -> ... -> An -> B
     * is a tautology. This function converts an argument to a tautology of
     * this form which we can then check for validity. Not quite, because we
     * don't get the argument order right, but this is irrelevant *)
    let
      fun arg_to_prop ([], q) = q
        | arg_to_prop (p::args, q) =  arg_to_prop(args, ifthen(p,q))
    in 
      is_tautology (arg_to_prop(args, p)) 
    end
end

