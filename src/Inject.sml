structure Inject =
struct
  structure A = Array
  structure AS = ArraySlice

  type 'a seq = 'a AS.slice

  fun writeMax (a, i) x =
    let
      val current = A.sub (a, i)
    in
      if x <= current then
        false
      else if current = Primitives.casArray (a, i) (current, x) then
        true
      else
        writeMax (a, i) x
    end

  fun inject s u =
    let
      val n = A.length s
      val m = A.length u

      val idx = SeqBasis.tabulate 10000 (0, n) (fn i => ~1)
      val _ =
        Primitives.parfor 10000 (0, m) (fn i =>
          let
            val (j, x) = A.sub (u, i)
          in
            writeMax (idx, j) i;
            ()
          end)

      (* now idx[i] is ~1 if no update, or u[idx[i]] = (i, x) is the winner
       * at position i *)
      val result = SeqBasis.tabulate 10000 (0, n) (fn i =>
        let
          val j = A.sub (idx, i)
        in
          if j = ~1 then A.sub (s, i)
          else #2 (A.sub (u, j))
        end)
    in
      result
    end

  fun ninject s u =
    let
      val n = A.length s
      val m = A.length u

      val t = Primitives.alloc n
      val alreadySet = Primitives.alloc n
      val _ = Primitives.parfor 10000 (0, n) (fn i =>
        ( A.update (t, i, A.sub (s, i))
        ; A.update (alreadySet, i, 0w0: Word8.word)
        ))
      (*
      val t = SeqBasis.tabulate 10000 (0, n) (fn i => A.sub (s, i))
      val alreadySet = SeqBasis.tabulate 10000 (0, n) (fn i => 0w0: Word8.word)
       *)
    in
      Primitives.parfor 10000 (0, m) (fn i =>
        let
          val (j, x) = A.sub (u, i)
        in
          if A.sub (alreadySet, j) = 0w0 andalso
             Primitives.casArray (alreadySet, j) (0w0, 0w1) = 0w0
          then A.update (t, j, x)
          else ()
        end);
      t
    end

end
