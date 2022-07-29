
val printLock : Word32.word ref = ref 0w0
val _ = MLton.Parallel.Deprecated.lockInit printLock
val myWorkerId = MLton.Parallel.processorNumber
val P = MLton.Parallel.numberOfProcessors

val doDebugMsg = false
val debug = doDebugMsg
fun dbg m =
		if not doDebugMsg then () else
			let
				val p = myWorkerId ()
				val _ = MLton.Parallel.Deprecated.takeLock printLock
				val msg = String.concat ["[", Int.toString p, "] ", m, "\n"]
			in
				( TextIO.output (TextIO.stdErr, msg)
					; TextIO.flushOut TextIO.stdErr
					; MLton.Parallel.Deprecated.releaseLock printLock
				)
			end

val _ = dbg("AAAAA\n")

(* TIMING PARAMS *)
val T_TOTAL 	= 0

(* END TIMING *)


val NZ = NA * (NONZER+1) * (NONZER+1) + (NA * (NONZER+1))
val FIRSTROW = 1;
val LASTROW  = NA;
val FIRSTCOL = 1;
val LASTCOL  = NA;

(*val for = Util.for*)

fun forLoop((i : int, j : int), f : int -> unit) : unit = 
		if i >=j then ()
		else (
				f(i)
				;	forLoop((i+1, j), f))



(* GLOBAL VARIABLES *)
val TRAN : real ref  = ref 314159265.0;
val AMULT : real  = 1220703125.0;



(* FFI *)
(* val ipow46 = _import "ipow46" public: real * int -> real; *)
val create_data = _import "makea" public: int * int * real array * int array * int array * int * int * int * int * int * real * int array * int array * real array * real array * int array * real * real ref * real -> unit;

val randlc = _import "randlc" public: real ref * real -> real;

val _ = dbg("DONE GLOBAL\n")
