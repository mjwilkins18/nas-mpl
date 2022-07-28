

val clock = _import "get_wtime" public: unit -> real;
val CPS = _import "get_cps" public : unit -> real;


fun elapsed_time() : real =
	clock()


val START : real array = Array.array(64, 0.0)
val ELAPSED : real array = Array.array(64, 0.0)


fun timer_clear(n : int) : unit =
	Array.update(ELAPSED, n, 0.0)

fun timer_start(n : int) : unit = 
	Array.update(START, n, elapsed_time())

fun timer_stop(n : int) : unit = 
	let
		val t = elapsed_time() - Array.sub(START, n)
		val cur = Array.sub(ELAPSED, n)
	in
		Array.update(ELAPSED, n, t + cur)
	end

fun timer_read(n : int) : real = 
	Array.sub(ELAPSED, n)




