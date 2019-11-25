val warmupFactor = CommandLineArgs.parseInt "warmup" 25

fun oneWarmup k i =
  let
    fun loop prev rest i =
      if i = 0 then prev :: rest
      else loop (prev ^ prev) (prev :: rest) (i-1)

    val x = loop ":)" [] k
    val c = String.sub (List.nth (x, i mod (List.length x)), 0)
  in
    Primitives.forceCollect ();
    c
  end

fun warmup () =
  let
    val P = Primitives.numberOfProcessors
    val r = ref #" "
  in
    ParUtil.parfor 1 (0, 2*P) (fn i => r := oneWarmup warmupFactor i);
    !r
  end

