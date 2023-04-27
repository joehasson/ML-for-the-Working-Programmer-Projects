use "set.sml";

(*Setup: Constructing the test module from Set functor *)

structure IntOrder: MEM = struct
  type t = int
  val compare = Int.compare
end

structure IntSet = Set(IntOrder)
val test_val = IntSet.insert(1, IntSet.empty)

(* Test cases *)

infix !>
fun (s !> x) = IntSet.insert (x, s)

val test_val = IntSet.empty !> 1 !> 2 !> 3 !> 4 !> 5

(* Set contains all the elements added *)
val test0 = List.foldl (fn (x, b) => b andalso IntSet.mem(x, test_val)) true [1,2,3,4,5] 
val test1 = (IntSet.size test_val) = 5
  
(* Adding elements already in set leaves it unchanged *)
val test_val0 = IntSet.insert(3, test_val)
val test2 = (IntSet.size test_val) = (IntSet.size test_val0)
  
(* Negative membership testing works *)
val test3 = not (IntSet.mem(100, test_val))

(* to_list works *)
val test4 = IntSet.to_list IntSet.empty = []
val test5 = IntSet.to_list test_val = [1,2,3,4,5]

(* Union works *)
val test6 = 
  let
    val u = IntSet.union (IntSet.empty, test_val)
  in 
    IntSet.to_list u = IntSet.to_list test_val
  end

val test7 =
  let 
    val other = IntSet.empty !> 2 !> 4 !> 6 !> 8
    val u = IntSet.union (other, test_val)
  in 
    IntSet.to_list u = [1,2,3,4,5,6,8]
  end

(* Intersection works *)

val test8 = 
  let
    val u = IntSet.inter (IntSet.empty, test_val)
  in 
    IntSet.to_list u = []
  end

val test9 =
  let 
    val other = IntSet.empty !> 2 !> 4 !> 6 !> 8
    val u = IntSet.inter (other, test_val)
  in 
    IntSet.to_list u = [2,4]
  end

