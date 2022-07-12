val P = MLton.Parallel.numberOfProcessors

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

(* TIMER VALUES (used in timing.sml) *)
val T_TOTAL 	= 0
val T_SETUP 	= 1
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

(* Complex number definition and functions *)

type dcomplex = (real * real)
fun cadd ((ar, ai) : dcomplex, (br, bi) : dcomplex) : dcomplex = 
		(ar + br, ai + bi)
	

fun csub ((ar, ai) : dcomplex, (br, bi) : dcomplex) : dcomplex = 
		(ar - br, ai - bi)

fun cmul ((ar, ai) : dcomplex, (br, bi) : dcomplex) : dcomplex = 
		let 
			val rl : real = (ar * br) - (ai * bi)
			val im : real = (ar * bi) + (ai * br)
		in
			(rl, im)
		end

fun crmul ((ar, ai) : dcomplex, b : real) : dcomplex = 
		(ar * b, ai * b)

fun crdiv ((ar, ai) : dcomplex, b : real) : dcomplex = 
		(ar / b, ai / b)


(* Definition and functions for the 3d coordinate arrays we'll be using*)
type c2dArr = (int * int * dcomplex array)
type c3dArr = (int * int * int * dcomplex array)

fun get3dElem ((width, height, depth, arr) : c3dArr, x, y, z) : dcomplex = 
		Array.sub(arr, height*depth*x + depth*y + z)
fun get2dElem ((width, height, arr) : c2dArr, x, y) : dcomplex = 
		Array.sub(arr, height*x + y)

fun set3dElem((width, height, depth, arr) : c3dArr, x : int, y : int, z : int, new : dcomplex) : unit =
		Array.update(arr, height*depth*x + depth*y + z, new)
fun set2dElem((width, height, arr) : c2dArr, x, y, new : dcomplex) : unit = 
		Array.update(arr, height*x + y, new)


fun printC((r, i) : dcomplex) =
		print("(" ^ Real.toString(r) ^ ", " ^ Real.toString(i) ^ ") ")

(* fun print3dArrSlice((width, height, depth, arr) : c3dArr, start, last) : unit =
	Vector.appi (fn (i, elem) => if i >= start * width andalso i < last * width then print() else ()) arr  *)


fun init3d(xsize, ysize, zsize) = 
		(xsize, ysize, zsize, Array.array(xsize*ysize*zsize, (0.0, 0.0)))
fun init2d(xsize, ysize) = 
		(xsize, ysize, Array.array(xsize*ysize, (0.0, 0.0)))

fun cGenFromInt(n : int) : dcomplex = 
		(Real.fromInt n, (Real.fromInt n) + 0.5)
fun cGen2d(start : int, ySize : int, zSize : int) : (dcomplex array) vector = 
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


(* definition and functions for 3d integer arrays *)
type i3dArr = (int * int * int * int array)
fun initI3d(xsize, ysize, zsize) = 
	(xsize, ysize, zsize, Array.array(xsize*ysize*zsize, 0))
fun geti3dElem ((width, height, depth, arr) : i3dArr, x, y, z) : int = 
		Array.sub(arr, height*depth*x + depth*y + z)
fun seti3dElem((width, height, depth, arr) : i3dArr, x : int, y : int, z : int, new : int) : unit =
		Array.update(arr, height*depth*x + depth*y + z, new)



val FFTBLOCK = 16 (* DEFAULTS ARE BLOCK=16 and PAD=18*)
val FFTBLOCKPAD = 18

val	SEED =	314159265.0
val	A =	1220703125.0
val	PI =	3.141592653589793238
val	ALPHA =	0.000001



val main_c = _import "main_c" public: unit -> int;
