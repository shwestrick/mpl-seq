structure S = SeqBasis
structure CLA = CommandLineArgs

structure A = Array
structure AS = ArraySlice
fun upd a i x = A.update (a, i, x)
fun nth a i   = A.sub (a, i)

fun test {task: unit -> 'a, check: 'a -> bool} =
  let
    val runs = CLA.parseInt "runs" 1
    val doCheck = CLA.parseFlag "check"

    fun loop tms k =
      let
        val (result, tm) = Util.getTime task
        val _ = print (Time.fmt 4 tm ^ "\n")
      in
        if k+1 >= runs
        then (result, tm :: tms)
        else loop (tm :: tms) (k+1)
      end

    val (result, tms) = loop [] 0

    val tot = List.foldl Time.+ Time.zeroTime tms
    val min = List.foldl Real.min Real.posInf (List.map Time.toReal tms)
    val max = List.foldl Real.max Real.negInf (List.map Time.toReal tms)
    val avg = Time.toReal tot / Real.fromInt (List.length tms)

  in
    print ("min " ^ Util.rtos 4 min ^ "\n");
    print ("max " ^ Util.rtos 4 max ^ "\n");
    print ("avg " ^ Util.rtos 4 avg ^ "\n");

    if not doCheck then ()
    else if check result then
      print ("correct? yes\n")
    else
      print ("correct? no\n");

    ()
  end

fun all (lo, hi) f =
  S.reduce 10000 (fn (a,b) => a andalso b) true (lo, hi) f

(* ========================================================================= *)

val n = CLA.parseInt "n" (1000*1000*100)
val grain = CLA.parseInt "grain" 10000

fun testTabulate () =
  let
    fun gen i = Real.fromInt i
    fun task () = S.tabulate grain (0, n) gen

    fun check result =
      Array.length result = n
      andalso
      all (0, n) (fn i => Real.== (nth result i, gen i))
  in
    test {task=task, check=check}
  end

fun testReduce () =
  let
    val input = S.tabulate 10000 (0, n) (fn i => 1.0)
    fun task () = S.reduce grain op+ 0.0 (0, n) (nth input)
    fun check result = Util.closeEnough (result, Real.fromInt n)
  in
    test {task=task, check=check}
  end

fun testScan () =
  let
    val input = S.tabulate 10000 (0, n) (fn i => 1.0)
    fun task () = S.scan grain op+ 0.0 (0, n) (nth input)
    fun goodIndex result i = Util.closeEnough (nth result i, Real.fromInt i)
    fun check result =
      Array.length result = n+1
      andalso
      all (0, n+1) (goodIndex result)
  in
    test {task=task, check=check}
  end

fun testFilter () =
  let
    fun gen i = Real.fromInt (i mod 2)
    fun keep x = Real.== (x, 1.0)
    val input = S.tabulate 10000 (0, n) gen
    fun task () = S.filter grain (0, n) (nth input) (keep o nth input)
    fun check result =
      let
        val correctLength =
          S.reduce 10000 op+ 0 (0, n) (fn i => if keep (gen i) then 1 else 0)
      in
        Array.length result = correctLength
        andalso
        S.reduce 10000 (fn (a, b) => a andalso b) true (0, correctLength)
        (fn i => Real.== (nth result i, 1.0))
      end
  in
    test {task=task, check=check}
  end

val tests =
  [ ("tabulate", testTabulate)
  , ("reduce", testReduce)
  , ("scan", testScan)
  , ("filter", testFilter)
  ]

val testName = CLA.parseString "test" ""

val (_, tm) = Util.getTime warmup
val _ = print ("warmup " ^ Time.fmt 4 tm ^ "s\n")

val _ =
  if testName = "" then
    (* run everything *)
    List.app (fn (name, b) => (print (name ^ "\n"); b(); print "\n")) tests
  else
    case List.find (fn (name,_) => name = testName) tests of
      NONE => Util.die ("unknown test name " ^ testName)
    | SOME (_, b) => b ()
