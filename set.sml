(* Define a polymorphic type of sets *
* In chapter 7 of ML for the working programmer, Paulson defines
* a functor creating dictionary types with arbitrarily ordered keys.
 * The following is a similar implementation of tree sets.*)

infix |>
fun (x |> f) = f x

signature SET = sig
  exception Duplicate
  type element
  type t
  val empty: t
  val insert: element * t -> t 
  val mem: element * t -> bool
  val size: t -> int
  val inter: t * t -> t
  val union: t * t -> t
  val to_list: t -> element list
  val from_list: element list -> t
end

signature MEM = sig
  type t
  val compare: t * t -> order
end

(* A functor for sets *)
(* Figure out a way to seal the type
 * without sealing the element type *)
functor Set (Mem: MEM) : SET = struct
  exception Duplicate
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

  (* Return a sorted list of set elements *)
  fun to_list s =
    (* Traverse the set tree "reverse inorder" -
     *  right subtree then root then left subree -
     *   in order to build the list. *)
    let fun aux (Lf, acc) = acc
          | aux (Br (x, t1, t2), acc) =
              aux (t1, x::aux (t2, acc))
    in aux (s, []) end

  val from_list = List.foldl insert empty

  fun union (s1, s2) = List.foldl insert s1 (to_list s2)

  fun inter (s1, s2) = to_list s1
    |> List.filter (fn x => mem (x, s2))
    |> from_list
end

