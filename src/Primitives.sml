structure Primitives =
struct
  val par = ForkJoin.fork

  fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) = par (fn _ => par (a, b), fn _ => par (c, d))
    in
      (ar, br, cr, dr)
    end

  structure Array = Array
  structure ArraySlice = ArraySlice

  val unsafeSub = Unsafe.Array.sub
  val unsafeUpdate = Unsafe.Array.update
  val alloc = ForkJoin.alloc

  val cas = MLton.Parallel.compareAndSwap
  val casArray = MLton.Parallel.arrayCompareAndSwap

  val numberOfProcessors = MLton.Parallel.numberOfProcessors

  fun forceCollect () =
    MLton.GC.collect ()
end
