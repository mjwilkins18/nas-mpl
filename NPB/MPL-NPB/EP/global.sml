val P = MLton.Parallel.numberOfProcessors

(*Granularity Control Variables*)
val OUTER_GRAIN = 8192
val FFTZ_GRAIN = 1

val NTHREAD = 16
val G = CommandLineArgs.parseInt "G" (1)
val IG = CommandLineArgs.parseInt "I" (64)
val INNER_GRAIN = IG
val NZ_GRAIN : int = G
val NX_GRAIN : int = G
val NY_GRAIN : int = G

(* val NZ_GRAIN : int = if (OUTER_GRAIN div (NX*NY)) > 1 then OUTER_GRAIN div (NX*NY) else 1
val NX_GRAIN : int = if (OUTER_GRAIN div (NZ*NY)) > 1 then OUTER_GRAIN div (NZ*NY) else 1
val NY_GRAIN : int = if (OUTER_GRAIN div (NX*NZ)) > 1 then OUTER_GRAIN div (NX*NZ) else 1 *)

(*val _ = print("NZ_G == " ^ istr(NZ_GRAIN) ^ "\n")*)


(* Extra Basic Math Functions *)
fun power b e = if e = 0 then 1 else b * power b (e-1);
fun powerReal (b : real,  e : int) : real = 
	if e = 0 then 
	  1.0 
        else 
	  b * powerReal(b, (e-1));

(* TIMER VALUES (used in timing.sml) *)
val TODO = ~1
val TIMERS_ENABLED = true

fun forLoop((i : int, j : int), f : int -> unit) : unit = 
		if i >=j then ()
		else (
				f(i)
				;	forLoop((i+1, j), f))

fun negForLoop((i : int, j : int), f : int -> unit) : unit = 
		if i <j then ()
		else (
				f(i)
				;	negForLoop((i-1, j), f))


val randlc_c = _import "randlc" public: real ref * real -> real;
val randlc_c_unit = _import "randlc_unit" public: real ref * real -> unit;
val vranlc_c = _import "vranlc_c" public: int * real ref * real * real array -> unit; 
val power_c = _import "power" public: real * int -> real;
val sqrt_c = _import "sqrt_c" public: real -> real;
val fabs_c = _import "fabs_c" public: real -> real;
