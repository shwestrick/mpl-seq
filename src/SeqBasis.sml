structure SeqBasis :
sig
  type grain = int

  val tabulate : grain -> (int * int) -> (int -> 'a) -> 'a array

  val iterate : ('b * 'a -> 'b)
             -> 'b
             -> (int * int)
             -> (int -> 'a)
             -> 'b

  val reduce : grain
            -> ('a * 'a -> 'a)
            -> 'a
            -> (int * int)
            -> (int -> 'a)
            -> 'a

  val scan : grain
          -> ('a * 'a -> 'a)
          -> 'a
          -> (int * int)
          -> (int -> 'a)
          -> 'a array  (* length N+1, for both inclusive and exclusive scan *)

  val filter : grain
            -> (int * int)
            -> (int -> 'a)
            -> (int -> bool)
            -> 'a array
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

  val parfor = ParUtil.parfor
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

  fun reduce grain g b (lo, hi) f =
    let
      val n = hi - lo
      val k = grain
      val m = 1 + (n-1) div k (* number of blocks *)

      fun red i j =
        case j - i of
          0 => b
        | 1 => iterate g b (lo + i*k, Int.min (lo + (i+1)*k, hi)) f
        | n => let val mid = i + (j-i) div 2
               in g (par (fn _ => red i mid, fn _ => red mid j))
               end
    in
      red 0 m
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
            iterate bump (i*k, nth partials i) (start, Int.min (start+k, n)) f
          end);
        upd result n (nth partials m);
        result
      end

  fun scan grain g b (lo, hi) (f : int -> 'a) =
    if hi - lo <= grain then
      let
        val n = hi - lo
        val result = Primitives.alloc (n+1)
        fun bump ((j,b),x) = (upd result j b; (j+1, g (b, x)))
        val (_, total) = iterate bump (0, b) (lo, hi) f
      in
        upd result n total;
        result
      end
    else
      let
        val n = hi - lo
        val k = grain
        val m = 1 + (n-1) div k (* number of blocks *)
        val sums = tabulate 1 (0, m) (fn i =>
          let val start = lo + i*k
          in iterate g b (start, Int.min (start+k, hi)) f
          end)
        (* val partials = scanArr grain g b sums *)
        val partials = scan grain g b (0, m) (nth sums)
        val result = Primitives.alloc (n+1)
      in
        parfor 1 (0, m) (fn i =>
          let
            fun bump ((j,b),x) = (upd result j b; (j+1, g (b, x)))
            val start = lo + i*k
          in
            iterate bump (i*k, nth partials i) (start, Int.min (start+k, hi)) f
          end);
        upd result n (nth partials m);
        result
      end

(*
  datatype 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
  fun treeval (Leaf x) = x
    | treeval (Node (x, _, _)) = x

  fun scan grain g b (lo, hi) f =
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
*)
  fun filter grain (lo, hi) f g =
    let
      val n = hi - lo
      val k = grain
      val m = 1 + (n-1) div k (* number of blocks *)
      fun count (i, j) c =
        if i >= j then c
        else if g i then count (i+1, j) (c+1)
        else count (i+1, j) c
      val counts = tabulate 1 (0, m) (fn i =>
        let val start = lo + i*k
        in count (start, Int.min (start+k, hi)) 0
        end)
      (* val offsets = scan grain op+ 0 (0, m) (nth counts) *)
      val offsets = scanArr grain op+ 0 counts
      val result = Primitives.alloc (nth offsets m)
      fun filterSeq (i, j) c =
        if i >= j then ()
        else if g i then (upd result c (f i); filterSeq (i+1, j) (c+1))
        else filterSeq (i+1, j) c
    in
      parfor 1 (0, m) (fn i =>
        let val start = lo + i*k
        in filterSeq (start, Int.min (start+k, hi)) (nth offsets i)
        end);
      result
    end

end
