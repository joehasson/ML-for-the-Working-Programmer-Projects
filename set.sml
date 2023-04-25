(* Define a polymorphic type of sets *
* In chapter 7 of ML for the working programmer, Paulson defines
* a functor creating dictionary types with arbitrarily ordered keys.*)

signature SET = sig
  type element
  type t
  val empty: t
  val insert: element * t -> t 
  val mem: element * t -> bool
  val size: t -> int
end

signature MEM = sig
  type t
  val compare: t * t -> order
end

(* A functor for sets *)
(* Figure out a way to seal the type
 * without sealing the element type *)
functor Set (Mem: MEM) : SET = struct
  type element = Mem.t
  datatype t = Lf | Br of element * t * t

  val empty = Lf

  fun insert (x, Lf) = Br (x, Lf, Lf)
    | insert (x, Br (y, t1, t2)) =
        case Mem.compare (x,y) of
              LESS => Br (y, insert (x, t1), t2)
            | EQUAL => Br (y, t1, t2)
            | GREATER => Br (y, t1, insert (x, t2))

  fun mem (x, Lf) = false
    | mem (x, Br (y, t1, t2)) = 
        case Mem.compare(x,y) of
              EQUAL => true
            | _ => mem(x, t1) orelse mem (x, t2)
            
  fun size Lf = 0
    | size (Br (_, t1, t2)) = 1 + (size t1) + (size t2)

end

