structure Merge :
sig
  type 'a seq = 'a ArraySequence.t

  val writeMergeSerial :
       ('a * 'a -> order)   (* compare *)
    -> 'a seq * 'a seq      (* (sorted) sequences to merge *)
    -> 'a seq               (* output *)
    -> unit

  val writeMerge :
       ('a * 'a -> order)   (* compare *)
    -> 'a seq * 'a seq      (* (sorted) sequences to merge *)
    -> 'a seq               (* output *)
    -> unit

  val mergeSerial : ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
  val merge : ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
end =
struct

  structure AS = Primitives.ArraySlice
  structure Seq = ArraySequence
  type 'a seq = 'a Seq.t

  (* fun report s1 s2 t =
    let
      val (_, i1, n1) = AS.base s1
      val (_, i2, n2) = AS.base s2
      val (_, i, n) = AS.base t

      fun str x = Int.toString x
    in
      print ("merging " ^
             str i1 ^ ":" ^ str (i1+n1) ^ " and " ^
             str i2 ^ ":" ^ str (i2+n2) ^ " into " ^
             str i ^ ":" ^ str (i+n) ^ "\n")
    end *)

  fun writeMergeSerial cmp (s1, s2) t =
    let
      (* val _ = (print ("serial "); report s1 s2 t) *)
      fun write i x = AS.update (t, i, x)

      val n1 = Seq.length s1
      val n2 = Seq.length s2

      (* i1 index into s1
       * i2 index into s2
       * j index into output *)
      fun loop i1 i2 j =
        if i1 = n1 then
          Seq.foreach (Seq.subseqIdxs s2 i2 n2) (fn (i, x) => write (i+j) x)
        else if i2 = n2 then
          Seq.foreach (Seq.subseqIdxs s1 i1 n1) (fn (i, x) => write (i+j) x)
        else
          let
            val x1 = Seq.nth s1 i1
            val x2 = Seq.nth s2 i2
          in
            case cmp (x1, x2) of
              LESS => (write j x1; loop (i1+1) i2 (j+1))
            | _    => (write j x2; loop i1 (i2+1) (j+1))
          end
    in
      loop 0 0 0
    end

  fun mergeSerial cmp (s1, s2) =
    let
      val out = AS.full (Primitives.alloc (Seq.length s1 + Seq.length s2))
    in
      writeMergeSerial cmp (s1, s2) out;
      out
    end

  fun writeMerge cmp (s1, s2) t =
    if Seq.length t <= 4096 then
      writeMergeSerial cmp (s1, s2) t
    else if Seq.length s1 = 0 then
      Seq.foreach s2 (fn (i, x) => AS.update (t, i, x))
    else
      let
        val n1 = Seq.length s1
        val n2 = Seq.length s2
        val mid1 = n1 div 2
        val pivot = Seq.nth s1 mid1
        val mid2 = BinarySearch.search cmp s2 pivot

        val l1 = Seq.subseqIdxs s1 0 mid1
        val r1 = Seq.subseqIdxs s1 (mid1+1) n1
        val l2 = Seq.subseqIdxs s2 0 mid2
        val r2 = Seq.subseqIdxs s2 mid2 n2

        val _ = AS.update (t, mid1+mid2, pivot)
        val tl = Seq.subseqIdxs t 0 (mid1+mid2)
        val tr = Seq.subseqIdxs t (mid1+mid2+1) (Seq.length t)
      in
        Primitives.par
          (fn _ => writeMerge cmp (l1, l2) tl,
           fn _ => writeMerge cmp (r1, r2) tr);
        ()
      end

  fun merge cmp (s1, s2) =
    let
      val out = AS.full (Primitives.alloc (Seq.length s1 + Seq.length s2))
    in
      writeMerge cmp (s1, s2) out;
      out
    end

end
