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

val _ = print("NZ_G == " ^ istr(NZ_GRAIN) ^ "\n")


(* MG Global Variables (Refs) *)
val is1 = ref 0
val is2 = ref 0
val is3 = ref 0
val ie1 = ref 0
val ie2 = ref 0
val ie3 = ref 0

(* Extra Basic Math Functions *)
fun power b e = if e = 0 then 1 else b * power b (e-1);
fun powerReal (b : real,  e : int) : real = 
	if e = 0 then 
	  1.0 
        else 
	  b * powerReal(b, (e-1));

(* TIMER VALUES (used in timing.sml) *)
val T_BENCH 	= 0
val T_INIT 	= 1
val T_FFT 		= 2
val T_EVOLVE	= 3
val T_CHECKSUM	= 4
val T_MAX		= 5

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


(* Definition and functions for the 3d coordinate arrays we'll be using*)
type c2dArr = (int * int * real array)
type c3dArr = (int * int * int * real array)

fun print3dSize ((width, height, depth, arr) : c3dArr) : unit = 
		print("W: " ^ istr(width) ^ " H: " ^ istr(height) ^ " D: " ^ istr(depth) ^ "\n") 

fun get3dElem ((width, height, depth, arr) : c3dArr, x, y, z) : real = 
		Array.sub(arr, height*depth*x + depth*y + z)
fun get2dElem ((width, height, arr) : c2dArr, x, y) : real = 
		Array.sub(arr, height*x + y)

fun set3dElem((width, height, depth, arr) : c3dArr, x : int, y : int, z : int, new : real) : unit =
                (*(print("i: " ^ padStr(istr(width), 3) ^ "k: " ^ padStr(istr(height), 3) ^ "j: " ^padStr(istr(depth), 3) ^ "\n");*)
		Array.update(arr, height*depth*x + depth*y + z, new)

fun set3dElemPrint((width, height, depth, arr) : c3dArr, x : int, y : int, z : int, new : real) : unit =
                (
		print("i: " ^ padStr(istr(width), 3) ^ " k: " ^ padStr(istr(height), 3) ^ " j: " ^padStr(istr(depth), 3) ^ "\n");
                print("x: " ^ padStr(istr(x), 3) ^ " y: " ^ padStr(istr(y), 3) ^ " z: " ^padStr(istr(z), 3) ^ "\n");
		Array.update(arr, height*depth*x + depth*y + z, new)
		)

fun set2dElem((width, height, arr) : c2dArr, x, y, new : real) : unit = 
		Array.update(arr, height*x + y, new)

fun set3dRow(input : c3dArr, x : int, y : int, new_row : real array, l : int, offset : int) : unit =
	let 
		val (width, height, depth, arr) = input
		(*val _ = print("x : " ^ istr(x) ^ " y: " ^ istr(y) ^ " width: " ^ istr(width) ^ " height: " ^ istr(height) ^ " depth: " ^ istr(depth) ^ "\n")*)
	in
		forLoop((0, l), fn z =>
			set3dElem(input, x, y, z, Array.sub(new_row, z+offset))
		)
	end

(*To print, use "get" followed by print("Real.toString(x)")*)

fun init3d(xsize, ysize, zsize) = 
		(xsize, ysize, zsize, Array.array(xsize*ysize*zsize, 0.0))
fun init2d(xsize, ysize) = 
		(xsize, ysize, Array.array(xsize*ysize, 0.0))

fun cGenFromInt(n : int) : real = (Real.fromInt n)
fun cGen2d(start : int, ySize : int, zSize : int) : (real array) vector = 
		Vector.tabulate ( ySize, fn yIdx => Array.tabulate(zSize, fn zIdx => cGenFromInt(start + (yIdx * zSize) + zIdx)))
fun cGen3d(xSize : int, ySize : int, zSize: int) : c3dArr =
		(xSize, ySize, zSize, Array.tabulate(xSize * ySize * zSize, fn idx => cGenFromInt(idx)))

fun copy3d(input : c3dArr) : c3dArr =
	let 
		val (width, height, depth, arr) = input
		val out = init3d(width, height, depth)
	in
		forLoop((0, width), fn i =>
			forLoop((0, height), fn j =>
				forLoop((0, depth), fn k =>
					set3dElem(out, i, j, k, get3dElem(input, i, j, k))
				)
			)
		)
	; 	out
	end


(* definition and functions for 2d/3d integer arrays *)
type i3dArr = (int * int * int * int array)
type i2dArr = (int * int * int array)

fun initI3d(xsize, ysize, zsize) = 
	(xsize, ysize, zsize, Array.array(xsize*ysize*zsize, 0))
fun initI2d(xsize, ysize) = 
	(xsize, ysize, Array.array(xsize*ysize, 0))

fun geti3dElem ((width, height, depth, arr) : i3dArr, x, y, z) : int = 
		Array.sub(arr, height*depth*x + depth*y + z)
fun geti2dElem ((width, height, arr) : i2dArr, x, y) : int =
		Array.sub(arr, height*x + y)

fun seti3dElem((width, height, depth, arr) : i3dArr, x : int, y : int, z : int, new : int) : unit =
		Array.update(arr, height*depth*x + depth*y + z, new)
fun seti2dElem((width, height, arr) : i2dArr, x, y, new) : unit =
		Array.update(arr, height*x + y, new)

(* special for loops for make MG array lists *)
fun negForLoopMakeU(i : int, j : int, m1 : int array, m2 : int array, m3 : int array, f) : c3dArr list  = 
		if i < j then (f(i, m1, m2, m3))
		else ( List.concat [negForLoopMakeU(i-1, j, m1, m2, m3, f), f(i, m1, m2, m3)] )

fun make_u_array(l : int, m1 : int array, m2 : int array, m3 : int array) : c3dArr list = [init3d(Array.sub(m3, l), Array.sub(m2, l), Array.sub(m1, l))]
(*
val FFTBLOCK = 16 (* DEFAULTS ARE BLOCK=16 and PAD=18*)
val FFTBLOCKPAD = 18

val	SEED =	314159265.0
val	A =	1220703125.0
val	PI =	3.141592653589793238
val	ALPHA =	0.000001
*)


(*val main_c = _import "main_c" public: unit -> int;*)
(*val setup_c = _import "setup_c" public: int array * int array * int array * int array * int array * int array * int ref * int ref * int ref * int * int ref * int ref * int ref * int ref * int ref * int ref -> unit;*)
(*val zero3_c = _import "zero3_c" public: array * int ref * int ref * int ref -> unit;*) 
val randlc_c = _import "randlc" public: real ref * real -> real;
val randlc_c_unit = _import "randlc_unit" public: real ref * real -> unit;
val vranlc_c = _import "vranlc_c" public: int * real ref * real * real array -> unit; 
val power_c = _import "power" public: real * int -> real;
val sqrt_c = _import "sqrt_c" public: real -> real;
val fabs_c = _import "fabs_c" public: real -> real;
