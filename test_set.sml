use "set.sml";
(* Test cases *)
structure IntOrder: MEM = struct
  type t = int
  val compare = Int.compare
end

structure IntSet = Set(IntOrder)

val test_val = 
  let 
    infix !>;
    fun (set !> x) = IntSet.insert(x, set)
  in
    IntSet.empty !> 1 !> 2 !> 3 !> 4 !> 5
  end

(* Set contains all the elements added *)
val test0 = List.foldl (fn (x, b) => b andalso IntSet.mem(x, test_val)) true [1,2,3,4,5] 
val test1 = (IntSet.size test_val) = 5

(* Adding elements already in set leaves it unchanged *)
val test_val0 = IntSet.insert(3, test_val)
val test2 = (IntSet.size test_val) = (IntSet.size test_val0)

(* Negative membership testing works *)
val test3 = not (IntSet.mem(100, test_val))
