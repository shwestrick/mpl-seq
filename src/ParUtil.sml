structure ParUtil =
struct

  fun loop (lo, hi) b f =
    if (lo >= hi) then b else loop (lo+1, hi) (f (b, lo)) f

  fun forBackwards (i, j) f =
    if i = j then () else (f (j-1); forBackwards (i, j-1) f)

	fun for (i, j) f =
    if i >= j then () else (f i; for (i+1, j) f)

	fun parfor gran (i, j) f =
    if j-i <= gran then
      for (i, j) f
		else
     	let val mid = i + ((j-i) div 2)
   	  in Primitives.par (fn _ => parfor gran (i, mid) f,
                         fn _ => parfor gran (mid, j) f);
         ()
      end

end
