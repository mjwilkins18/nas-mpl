val myWorkerId = MLton.Parallel.processorNumber

val printLock : Word32.word ref = ref 0w0
val _ = MLton.Parallel.Deprecated.lockInit printLock

val doDebugMsg = false
val debug = doDebugMsg
fun dbgmsg m =
		if not doDebugMsg then () else
			let
				val p = myWorkerId ()
				val _ = MLton.Parallel.Deprecated.takeLock printLock
				val msg = String.concat ["[", Int.toString p, "] ", m(), "\n"]
			in
				( TextIO.output (TextIO.stdErr, msg)
					; TextIO.flushOut TextIO.stdErr
					; MLton.Parallel.Deprecated.releaseLock printLock
				)
			end



fun pow2h(0, acc:int) =
		acc
	| pow2h(e : int, acc : int) =
		pow2h(e-1, acc*2)

fun pow2(e: int) = 
		pow2h(e, 1)

fun butterfly((u1r, u1i) :dcomplex, (x11r, x11i) : dcomplex, (x21r, x21i) : dcomplex) : dcomplex =
		(	(u1r * (x11r - x21r)) - (u1i * (x11i - x21i)),
			(u1r * (x11i - x21i)) + (u1i * (x11r - x21r)) 	)

		

fun ilog2Rec(1 :int, nn : int, lg : int) = 
		0
	| ilog2Rec(n :int, nn : int, lg : int) =
		case nn < n of
			true => ilog2Rec(n, nn*2, lg+1)
		| false => lg
		

fun ilog2 (n : int) =
		ilog2Rec(n, 2, 1)


(* val ipow46 = _import "ipow46" public: real * int -> real; *)



fun gen_initial_conditions(width : int, height : int, depth : int) : c3dArr =
		let 
			val ureal : real array = Array.array(width*height*depth, 0.0)
			val uimag : real array = Array.array(width*height*depth, 0.0)
			val _ = comp_init(ureal, uimag, width, height, depth)
		in
			(width, height, depth, Array.tabulate(width*height*depth, fn i => (Array.sub(ureal, i), Array.sub(uimag, i))))
		end



fun compute_exp(exps : real array) =
		let
			val ap = ~4.0 * ALPHA * PI * PI
			val _ = Array.update(exps, 0, 1.0)
			val _ = Array.update(exps, 1, Math.exp(ap)) (* TODO: THIS NEEDS TO USE EXPF SOMEHOW; probably use a FFI*)
		in
			forLoop((2, EXPMAX+1), fn i =>
					Array.update(exps, i, Array.sub(exps, i-1) * Array.sub(exps, 1)))
		end

(* fun compute_indexmap_widthLoop(indexmap : i3dArr, i, j, k, ij2) =
		let 
			val (width, height, depth, arr) = indexmap
			val kk = (k + 1 + 1 - 2 + (NZ div 2)) mod NZ - (NZ div 2)
		in
			seti3dElem(indexmap, k, j, i, kk*kk+ij2)
		end
fun compute_indexmap_heightLoop(indexmap : i3dArr, i, j, ii2) =
		let 
			val (width, height, depth, arr) = indexmap
			val jj = (j + 1 + 1 - 2 + (NY div 2)) mod NY - (NY div 2)
			val ij2 = jj * jj + ii2
		in
			forLoop((0, width), fn k => compute_indexmap_widthLoop(indexmap, i, j, k, ij2))
		end
fun compute_indexmap_depthLoop(indexmap : i3dArr, i) =
		let 
			val (width, height, depth, arr) = indexmap
			val ii = (i + 1 + 1 - 2 + (NX div 2)) mod NX - (NX div 2)
			val ii2 = ii * ii
		in
			forLoop((0, height), fn j => compute_indexmap_heightLoop(indexmap, i, j, ii2))
		end *)

fun compute_indexmap(indexmap : i3dArr) = 
		let 
			val (width, height, depth, arr) = indexmap
		in
			ForkJoin.parfor NX_GRAIN (0, depth) (fn i => 
					let 
						val ii = (i + 1 + 1 - 2 + (NX div 2)) mod NX - (NX div 2)
						val ii2 = ii * ii
					in
						forLoop((0, height), fn j => 
								let 
									val jj = (j + 1 + 1 - 2 + (NY div 2)) mod NY - (NY div 2)
									val ij2 = jj * jj + ii2
								in
									forLoop((0, width), fn k => 
											let
												val kk = (k + 1 + 1 - 2 + (NZ div 2)) mod NZ - (NZ div 2)
											in
												seti3dElem(indexmap, k, j, i, kk*kk+ij2)
											end)
								end)
					end
			)
			;	compute_exp(EX)
		end
	


fun evolve((width0, height0, depth0, u0Arr) : c3dArr, (width, height, depth, u1Arr) : c3dArr, t : int, (widthi, heighti, depthi, indexmap) : i3dArr) =
		let
			val u0 : c3dArr = (width0, height0, depth0, u0Arr)
			val u1 : c3dArr = (width, height, depth, u1Arr)
			val idxmap : i3dArr = (widthi, heighti, depthi, indexmap)

			val unequalWidth = width0 <> width orelse width <> widthi
			val unequalHeight = height0 <> height orelse height <> heighti
			val unequalDepth = depth0 <> depth orelse depth <> depthi
			val badDims = unequalWidth orelse unequalHeight orelse unequalDepth
			
		in
			if badDims then
				dbgmsg(fn _ => "passed bad array dimensions to evolve")
			else
				ForkJoin.parfor NZ_GRAIN (0, width) (fn k => 
						forLoop((0, height), fn j =>
								forLoop((0, depth), fn i => 
										let
											val u0elem = get3dElem(u0, k, j, i)
											val idx = geti3dElem(idxmap, k, j, i)
											val expn = Array.sub(EX, t*idx)
										in
											set3dElem(u1, k, j, i, crmul(u0elem, expn))
										end
									)
							)
				)
					
		end

fun printChecksum(i, (sumr, sumi) : dcomplex) =
		let
			val rchk = padStr((Real.fmt (StringCvt.SCI (SOME(12))) sumr), 20)
			val ichk = padStr((Real.fmt (StringCvt.SCI (SOME(12))) sumi), 20)
			val iter = padStr(istr(i), 5)
		in
			print("T = " ^ iter ^ "     Checksum = " ^ rchk ^ "\t" ^ ichk ^ "\n")
		end

fun within(v, (l, u)) =
		l <= v andalso v <= u
fun checksum(i : int, u1 : c3dArr) =
		let
			val chk : dcomplex ref = ref (0.0, 0.0)
		in (
				forLoop((1, 1024+1), fn j =>
						let 
							val q = j mod NX + 1
						in
							if within(q, (1, NX)) then
								let
									val r = (3 * j) mod NY + 1
								in
									if within(r, (1, NY)) then
										let
											val s = (5*j) mod NZ + 1
										in
											if within(s, (1, NZ)) then
												chk := cadd(!chk, get3dElem(u1, (s-1), (r-1), (q-1)))
											else()
										end
									else()
								end
							else ()
						end

					)
				;	Array.update(SUMS, i, cadd(Array.sub(SUMS, i), !chk)) (* inc SUMS[i] by chk*)
				; Array.update(SUMS, i, crdiv(Array.sub(SUMS, i), Real.fromInt(NTOTAL))) (* TODO: DEFINE NTOTAL*)
				; printChecksum(i, Array.sub(SUMS, i))
			)
		end

fun verify_all(data_real : real list, data_imag : real list, 0, epsilon : real) = true
	| verify_all(data_real, data_imag, i : int, epsilon : real) =
		let
			val (sumr, sumi) : dcomplex = Array.sub(SUMS, i)
			val datar = List.nth(data_real, i)
			val err_r = (sumr - datar) / datar
			val validr = Real.abs(err_r) < epsilon

			val datai = List.nth(data_imag, i)
			val err_i = (sumi - datai) / datai
			val validi : bool = Real.abs(err_i) < epsilon
			
		in
			case validr andalso validi of
				true => verify_all(data_real, data_imag, i-1, epsilon)
			|	false => false
		end

fun verify_S(epsilon : real) : (bool * char) = 
		let
			val vdata_real_s = [0.0,
					5.546087004964 * Math.pow(10.0, 2.0),
					5.546385409189 * Math.pow(10.0, 2.0),
					5.546148406171 * Math.pow(10.0, 2.0),
					5.545423607415 * Math.pow(10.0, 2.0),
					5.544255039624 * Math.pow(10.0, 2.0),
					5.542683411902 * Math.pow(10.0, 2.0)]
			val vdata_imag_s = [0.0,
					4.845363331978 * Math.pow(10.0, 2.0),
					4.865304269511 * Math.pow(10.0, 2.0),
					4.883910722336 * Math.pow(10.0, 2.0),
					4.901273169046 * Math.pow(10.0, 2.0),
					4.917475857993 * Math.pow(10.0, 2.0),
					4.932597244941 * Math.pow(10.0, 2.0)]
			val valid = verify_all(vdata_real_s, vdata_imag_s, NITER_DEFAULT, epsilon)
		in
			(valid, #"S")
		end

fun verify_W(epsilon : real) : (bool * char) = 
		let
			val vdata_real_w = [0.0,
					5.673612178944 * Math.pow(10.0, 2.0),
					5.631436885271 * Math.pow(10.0, 2.0),
					5.594024089970 * Math.pow(10.0, 2.0),
					5.560698047020 * Math.pow(10.0, 2.0),
					5.530898991250 * Math.pow(10.0, 2.0),
					5.504159734538 * Math.pow(10.0, 2.0)]
			val vdata_imag_w = [0.0,
					5.293246849175 * Math.pow(10.0, 2.0),
					5.282149986629 * Math.pow(10.0, 2.0),
					5.270996558037 * Math.pow(10.0, 2.0),
					5.260027904925 * Math.pow(10.0, 2.0),
					5.249400845633 * Math.pow(10.0, 2.0),
					5.239212247086 * Math.pow(10.0, 2.0)]
			val valid = verify_all(vdata_real_w, vdata_imag_w, NITER_DEFAULT, epsilon)
		in
			(valid, #"W")
		end

fun verify_A(epsilon : real) : (bool * char) = 
		let
			val vdata_real_a = [0.0,
					5.046735008193 * Math.pow(10.0, 2.0),
					5.059412319734 * Math.pow(10.0, 2.0),
					5.069376896287 * Math.pow(10.0, 2.0),
					5.077892868474 * Math.pow(10.0, 2.0),
					5.085233095391 * Math.pow(10.0, 2.0),
					5.091487099959 * Math.pow(10.0, 2.0)]
			val vdata_imag_a = [0.0,
					5.114047905510 * Math.pow(10.0, 2.0),
					5.098809666433 * Math.pow(10.0, 2.0),
					5.098144042213 * Math.pow(10.0, 2.0),
					5.101336130759 * Math.pow(10.0, 2.0),
					5.104914655194 * Math.pow(10.0, 2.0),
					5.107917842803 * Math.pow(10.0, 2.0)]
			val valid = verify_all(vdata_real_a, vdata_imag_a, NITER_DEFAULT, epsilon)
		in
			(valid, #"A")
		end

fun verify_B(epsilon : real) : (bool * char) = 
		let
			val vdata_real_b = [0.0,
					5.177643571579 * Math.pow(10.0, 2.0),
					5.154521291263 * Math.pow(10.0, 2.0),
					5.146409228649 * Math.pow(10.0, 2.0),
					5.142378756213 * Math.pow(10.0, 2.0),
					5.139626667737 * Math.pow(10.0, 2.0),
					5.137423460082 * Math.pow(10.0, 2.0),
					5.135547056878 * Math.pow(10.0, 2.0),
					5.133910925466 * Math.pow(10.0, 2.0),
					5.132470705390 * Math.pow(10.0, 2.0),
					5.131197729984 * Math.pow(10.0, 2.0),
					5.130070319283 * Math.pow(10.0, 2.0),
					5.129070537032 * Math.pow(10.0, 2.0),
					5.128182883502 * Math.pow(10.0, 2.0),
					5.127393733383 * Math.pow(10.0, 2.0),
					5.126691062020 * Math.pow(10.0, 2.0),
					5.126064276004 * Math.pow(10.0, 2.0),
					5.125504076570 * Math.pow(10.0, 2.0),
					5.125002331720 * Math.pow(10.0, 2.0),
					5.124551951846 * Math.pow(10.0, 2.0),
					5.124146770029 * Math.pow(10.0, 2.0)]
			val vdata_imag_b = [0.0,
					5.077803458597 * Math.pow(10.0, 2.0),
					5.088249431599 * Math.pow(10.0, 2.0),
					5.096208912659 * Math.pow(10.0, 2.0),
					5.101023387619 * Math.pow(10.0, 2.0),
					5.103976610617 * Math.pow(10.0, 2.0),
					5.105948019802 * Math.pow(10.0, 2.0),
					5.107404165783 * Math.pow(10.0, 2.0),
					5.108576573661 * Math.pow(10.0, 2.0),
					5.109577278523 * Math.pow(10.0, 2.0),
					5.110460304483 * Math.pow(10.0, 2.0),
					5.111252433800 * Math.pow(10.0, 2.0),
					5.111968077718 * Math.pow(10.0, 2.0),
					5.112616233064 * Math.pow(10.0, 2.0),
					5.113203605551 * Math.pow(10.0, 2.0),
					5.113735928093 * Math.pow(10.0, 2.0),
					5.114218460548 * Math.pow(10.0, 2.0),
					5.114656139760 * Math.pow(10.0, 2.0),
					5.115053595966 * Math.pow(10.0, 2.0),
					5.115415130407 * Math.pow(10.0, 2.0),
					5.115744692211 * Math.pow(10.0, 2.0)]
			val valid = verify_all(vdata_real_b, vdata_imag_b, NITER_DEFAULT, epsilon)
		in
			(valid, #"B")
		end

fun verify_C(epsilon : real) : (bool * char) = 
		let
			val vdata_real_c = [0.0,
					5.195078707457 * Math.pow(10.0, 2.0),
					5.155422171134 * Math.pow(10.0, 2.0),
					5.144678022222 * Math.pow(10.0, 2.0),
					5.140150594328 * Math.pow(10.0, 2.0),
					5.137550426810 * Math.pow(10.0, 2.0),
					5.135811056728 * Math.pow(10.0, 2.0),
					5.134569343165 * Math.pow(10.0, 2.0),
					5.133651975661 * Math.pow(10.0, 2.0),
					5.132955192805 * Math.pow(10.0, 2.0),
					5.132410471738 * Math.pow(10.0, 2.0),
					5.131971141679 * Math.pow(10.0, 2.0),
					5.131605205716 * Math.pow(10.0, 2.0),
					5.131290734194 * Math.pow(10.0, 2.0),
					5.131012720314 * Math.pow(10.0, 2.0),
					5.130760908195 * Math.pow(10.0, 2.0),
					5.130528295923 * Math.pow(10.0, 2.0),
					5.130310107773 * Math.pow(10.0, 2.0),
					5.130103090133 * Math.pow(10.0, 2.0),
					5.129905029333 * Math.pow(10.0, 2.0),
					5.129714421109 * Math.pow(10.0, 2.0)]
			val vdata_imag_c = [0.0,
					5.149019699238 * Math.pow(10.0, 2.0),
					5.127578201997 * Math.pow(10.0, 2.0),
					5.122251847514 * Math.pow(10.0, 2.0),
					5.121090289018 * Math.pow(10.0, 2.0),
					5.121143685824 * Math.pow(10.0, 2.0),
					5.121496764568 * Math.pow(10.0, 2.0),
					5.121870921893 * Math.pow(10.0, 2.0),
					5.122193250322 * Math.pow(10.0, 2.0),
					5.122454735794 * Math.pow(10.0, 2.0),
					5.122663649603 * Math.pow(10.0, 2.0),
					5.122830879827 * Math.pow(10.0, 2.0),
					5.122965869718 * Math.pow(10.0, 2.0),
					5.123075927445 * Math.pow(10.0, 2.0),
					5.123166486553 * Math.pow(10.0, 2.0),
					5.123241541685 * Math.pow(10.0, 2.0),
					5.123304037599 * Math.pow(10.0, 2.0),
					5.123356167976 * Math.pow(10.0, 2.0),
					5.123399592211 * Math.pow(10.0, 2.0),
					5.123435588985 * Math.pow(10.0, 2.0),
					5.123465164008 * Math.pow(10.0, 2.0)]
			val valid = verify_all(vdata_real_c, vdata_imag_c, NITER_DEFAULT, epsilon)
		in
			(valid, #"C")
		end


fun verify(d1 : int, d2 : int, d3 : int, niter : int) : (bool * char) =
		let
			val epsilon = 1.0 * Math.pow(10.0, ~12.0)
			val res = case (d1, d2, d3, niter) of
					(64, 64, 64, 6) => verify_S(epsilon)
				|	(128, 128, 32, 6) => verify_W(epsilon)
				|	(256, 256, 128, 6) => verify_A(epsilon)
				|	(512, 256, 256, 20) => verify_B(epsilon)
				| 	(512, 512, 512, 20) => verify_C(epsilon)
				|	_ => (false, #"U")
			val _ = case res of 
					(_, #"U") => print("Result verification failed\n")
				|	_ => print("Result verification successful\n")
			val _ = print("class = " ^ Char.toString(#2 res) ^ "\n")
		in
			res
		end


fun getSciCompStr((r, i) : dcomplex) =
		(Real.fmt (StringCvt.SCI (SOME(15))) r) ^ " " ^ (Real.fmt (StringCvt.SCI (SOME(15))) i) ^ "\n"

fun getFixCompStr((r, i) : dcomplex) =
		(Real.fmt (StringCvt.FIX (SOME(23))) r) ^ " " ^ (Real.fmt (StringCvt.FIX (SOME(23))) i) ^ "\n"

fun write3dArr(inArr : c3dArr, fileName) =
		let 
			val (width, height, depth, arr) = inArr
			val os = TextIO.openOut fileName
			val header = Int.toString(width) ^ " " ^ Int.toString(height) ^ " " ^ Int.toString(depth) ^ "\n\n"
			val _ = TextIO.output(os, header)
		in
			forLoop((0, 4), fn k =>
					forLoop((0, 4), fn j => (
								forLoop((0, 4), fn i =>
										TextIO.output(os, getSciCompStr(get3dElem(inArr, k, j, i)))
									)
								;	TextIO.output(os, "\n")
							)
						)
				)
			;	TextIO.closeOut os
		end

fun write3dArrFull(inArr : c3dArr, fileName) =
		let 
			val (width, height, depth, arr) = inArr
			val os = TextIO.openOut fileName
			val header = Int.toString(width) ^ " " ^ Int.toString(height) ^ " " ^ Int.toString(depth) ^ "-"
			val _ = TextIO.output(os, header)
		in
			forLoop((0, width), fn k =>
					forLoop((0, height), fn j => (
								forLoop((0, depth), fn i =>
										let
											val index = ""
											(* val index = "[" ^ istr(k) ^ "][" ^ istr(j) ^ "][" ^ istr(i) ^ "] " *)
										in
											TextIO.output(os, index ^ getFixCompStr(get3dElem(inArr, k, j, i)))
										end
									)
								(* ;	TextIO.output(os, "\n") *)
							)
						)
				)
			;	TextIO.closeOut os
		end

fun printi3d(inArr : i3dArr) = 
		let 
			val (width, height, depth, arr) = inArr

			val header = "INTARR: " ^ Int.toString(width) ^ " " ^ Int.toString(height) ^ " " ^ Int.toString(depth) ^ "\n\n"
			val _ = print header
		in
			forLoop((0, 4), fn k =>
					forLoop((0, 4), fn j => (
								forLoop((0, 4), fn i =>
										print(Int.toString(geti3dElem(inArr, k, j, i)) ^ " ")
									)
								;	print("\n")
							)
						)
				)
		end

fun print_timers() =
		let 
			val timers = #["          total ",
				"          setup ",
				"            fft ",
				"         evolve ",
				"       checksum "]
		in
			forLoop((0, T_MAX), fn i =>
					let 
						val time = timer_read(i)
					in
						print("timer [" ^ istr(i) ^ "]" ^ Vector.sub(timers, i) ^ ": " ^ padStr(rstr(time), 16) ^ "\n")
					end
				)
		end


val _ = dbgmsg(fn _ => "STARTING HELPERS.SML\n")


(* val _ = print(rstr(clock())) *)

val _ = dbgmsg(fn _ => "GRAINS: Z = " ^ istr(NZ_GRAIN) ^ ", Y = " ^ istr(NY_GRAIN) ^ ", X = " ^ istr(NX_GRAIN) ^ "\n")




val _ = dbgmsg(fn _ => "DONE WITH HELPERS.SML\n")
