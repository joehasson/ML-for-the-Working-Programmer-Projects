use "set";

datatype prop = Atom of string 
              | Neg of prop 
              | Disj of (prop * prop) 
              | Conj of (prop * prop)

signature PROP = sig
  type t
  val to_string: t -> string
  val is_tautology: t -> bool
  val is_valid: t list * t -> bool
end

structure Prop : PROP = struct
  exception FormError
  type t = prop

  fun ifthen (p, q) = Disj (Neg p, q)

  
  fun to_string (Atom s) = s
    | to_string (Neg p) = "~( " ^ (to_string p) ^ " )"
    | to_string (Disj (Neg p, q)) = "( " ^ (to_string p) ^ " -> " ^ (to_string q) ^ ")"  
    | to_string (Disj (p, q)) = "( " ^ (to_string p) ^ " V " ^ (to_string q) ^ " )"
    | to_string (Conj (p, q)) = "( " ^ (to_string p) ^ " & " ^ (to_string q) ^ " )"

  fun nnf (Neg (Neg p)) = p
    | nnf (Neg (Conj (p, q))) = Disj (Neg (nnf p), Neg (nnf q))
    | nnf (Neg (Disj (p, q))) = Conj (Neg (nnf p), Neg (nnf q))
    | nnf (Disj (p, q)) = Disj (nnf p, nnf q)
    | nnf (Conj (p, q)) = Conj (nnf p, nnf q)
    | nnf (Neg p) = Neg p
    | nnf (Atom s) = Atom s

  fun nnf_to_cnf (Disj (p, Conj (q, r))) = Conj (Disj (p, q), Disj (p, r))
    | nnf_to_cnf (Disj (Conj (p, q), r)) = Conj (Disj (p, r), Disj (q, r))
    | nnf_to_cnf (Conj (p, q)) = Conj (nnf_to_cnf p, nnf_to_cnf q)
    | nnf_to_cnf (Disj (p, q)) = Disj (nnf_to_cnf p, nnf_to_cnf q)
    | nnf_to_cnf (Neg p) = Neg (nnf_to_cnf p)
    | nnf_to_cnf (Atom s) = Atom s

  fun is_tautology (Conj (p, q)) =
        let 
          fun eval_literals (Disj p, q) = 
          fun aux (c1 as Conj (p1, p2), c2 as Conj (q1, q2)) = (is_tautology c1) andalso (is_tautology c2)
            | aux (literals, c2 as Conj (q1, q2)) = 
            | aux (c1 as Conj (p1, p2), literals) = 
                  
    | is_tautology _ = raise FormError

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

