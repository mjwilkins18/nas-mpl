fun forLoop((i : int, j : int), f : int -> unit) : unit = 
	if i >=j then ()
	else ( f(i); forLoop((i+1, j), f) )

fun lshift n =
	if n = 0 then 1
	else 2 * lshift( n - 1 )

val G = CommandLineArgs.parseInt "G" (1)
