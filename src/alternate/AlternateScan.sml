structure AlternateScan :
sig
  type grain = int

  (* scan directly on arrays *)
  val scanArr : grain
          -> ('a * 'a -> 'a)
          -> 'a
          -> 'a array
          -> 'a array  (* length N+1, for both inclusive and exclusive scan *)

  (* general index scan, using up-down sweep *)
  val scanUpDown : grain
          -> ('a * 'a -> 'a)
          -> 'a
          -> (int * int)
          -> (int -> 'a)
          -> 'a array  (* length N+1, for both inclusive and exclusive scan *)

end =
struct

  type grain = int

  structure A = Array
  structure AS = ArraySlice

  (*
  fun upd a i x = Unsafe.Array.update (a, i, x)
  fun nth a i   = Unsafe.Array.sub (a, i)
  *)

  fun upd a i x = A.update (a, i, x)
  fun nth a i   = A.sub (a, i)

  val parfor = Primitives.parfor
  val par = Primitives.par

  fun tabulate grain (lo, hi) f =
    let
      val n = hi-lo
      val result = Primitives.alloc n
    in
      if lo = 0 then
        parfor grain (0, n) (fn i => upd result i (f i))
      else
        parfor grain (0, n) (fn i => upd result i (f (lo+i)));

      result
    end

  fun iterate g b (lo, hi) f =
    if lo >= hi then b else
    let
      val b' = g (b, f lo)
    in
      iterate g b' (lo+1, hi) f
    end

  fun scanArr grain g b A =
    if Array.length A <= grain then
      let
        val n = Array.length A
        val result = Primitives.alloc (n+1)
        fun bump ((j,b),x) = (upd result j b; (j+1, g (b, x)))
        val (_, total) = iterate bump (0, b) (0, n) (nth A)
      in
        upd result n total;
        result
      end
    else
      let
        val f = nth A
        val n = Array.length A
        val k = grain
        val m = 1 + (n-1) div k (* number of blocks *)
        val sums = tabulate 1 (0, m) (fn i =>
          let val start = i*k
          in iterate g b (start, Int.min (start+k, n)) f
          end)
        val partials = scanArr grain g b sums
        val result = Primitives.alloc (n+1)
      in
        parfor 1 (0, m) (fn i =>
          let
            fun bump ((j,b),x) = (upd result j b; (j+1, g (b, f j)))
            val start = i*k
          in
            iterate bump (i*k, nth partials i) (start, Int.min (start+k, n)) f;
            ()
          end);
        upd result n (nth partials m);
        result
      end

  datatype 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
  fun treeval (Leaf x) = x
    | treeval (Node (x, _, _)) = x

  fun scanUpDown grain g b (lo, hi) f =
    let
      val n = hi - lo
      val k = grain
      val m = 1 + (n-1) div k (* number of blocks *)

      fun block i = (lo + i*k, Int.min (lo + (i+1)*k, hi))

      fun upsweep i j =
        case j - i of
          0 => Leaf b
        | 1 => Leaf (iterate g b (block i) f)
        | n => let
                 val mid = i + (j-i) div 2
                 val (l, r) = par (fn _ => upsweep i mid, fn _ => upsweep mid j)
               in
                 Node (g (treeval l, treeval r), l, r)
               end

      val tree = upsweep 0 m
      val result = Primitives.alloc (n+1)

      fun downsweep b t i j =
        case t of
          Leaf _ =>
            let fun bump ((j,b),x) = (upd result j b; (j+1, g (b, f j)))
            in iterate bump (i*k, b) (block i) f;
               ()
            end
        | Node (_, l, r) =>
            let val mid = i + (j-i) div 2
            in par (fn _ => downsweep b l i mid,
                    fn _ => downsweep (g (b, treeval l)) r mid j);
               ()
            end
    in
      downsweep b tree 0 m;
      upd result n (treeval tree);
      result
    end

end
