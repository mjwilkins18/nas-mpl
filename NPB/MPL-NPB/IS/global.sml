fun Old_forLoop((i : int, j : int), f : int -> unit) : unit = 
		if i >=j then ()
		else (
				f(i)
				;	Old_forLoop((i+1, j), f))

fun forLoop((i : int, j : int), f : int -> unit) : unit = 
  Util.for (i,j) f


fun lshift n =
	if n = 0 then 1
	else 2 * lshift( n - 1 )

val G = CommandLineArgs.parseInt "G" (1)
val P = MLton.Parallel.numberOfProcessors
