
fun fft_init(n : int) =
		let
			val m = ilog2(n)
			val _ = Array.update(U, 0, (Real.fromInt(m), 0.0))
			val ku : int ref = ref 1
			val ln : int ref = ref 1
		in
			forLoop((0, m), fn j => (
						let 
							val t = PI / Real.fromInt(!ln)
						in
							forLoop((0, (!ln)), fn i : int =>
									let 
										val ti = Real.fromInt(i) * t
									in
										Array.update(U, i + !ku, (Math.cos(ti), Math.sin(ti)))
									end
								)
							;	ku := !ku + !ln (* TODO: not a great use of refs rip *)
							; 	ln := 2 * !ln
						end
					))
		end

(* fun isHyphen c = c = #"-"
fun parseToken(tokens, i) : dcomplex =
		let 
			val realList = List.map Char.toString (Seq.toList(Seq.nth tokens (i*2)))
			val imagList = List.map Char.toString (Seq.toList(Seq.nth tokens ((i*2)+1) ))
			val r : real = Option.getOpt(Real.fromString(String.concatWith "" realList), 0.0)
			val i : real = Option.getOpt(Real.fromString(String.concatWith "" imagList), 0.0)
		in
			(r, i)
		end
fun read_initial_conditions(fileName : string) : c3dArr =
		let
			val contents = ReadFile.contentsSeq fileName
			val split = Tokenize.tokensSeq isHyphen contents
			val dims = Tokenize.tokensSeq Char.isSpace (Seq.nth split 0)
			val tokens = Tokenize.tokensSeq Char.isSpace (Seq.nth split 1) 
		
			(*TODO: FIX THIS AND GET WIDESPREAD ACCLAIM AND FANS AND SPEND LOTS OF MONEY*)
			val widthList = List.map Char.toString (Seq.toList(Seq.nth dims 0))
			val width = Option.getOpt(Int.fromString(String.concatWith "" widthList), 0)
			val heightList = List.map Char.toString (Seq.toList(Seq.nth dims 1))
			val height = Option.getOpt(Int.fromString(String.concatWith "" heightList), 0)
			val depthList = List.map Char.toString (Seq.toList(Seq.nth dims 2))
			val depth = Option.getOpt(Int.fromString(String.concatWith "" depthList), 0)
		
			val even = (Seq.length tokens) mod 2 = 0
			val numElems = (Seq.length tokens) div 2
		in
			if even then
				let 
					val out : dcomplex array = Array.tabulate(numElems, fn i => parseToken(tokens, i))
				in
					(width, height, depth, out)
				end
			else 
				let
					val _ = dbgmsg(fn _ => "didn't read an even number of tokens :(((")
				in
					(0, 0, 0, Array.array(0, (~1.0, ~1.0)))
				end
		end *)
		

fun fftzk_loop(lk, ny, i11, i12, i21, i22, u1 : dcomplex, x : c2dArr, y : c2dArr) =
		forLoop((0, lk), fn k => 
				forLoop((0, ny), fn j => 
						let 
							val x11 = get2dElem(x, i11+k, j)
							val x21 = get2dElem(x, i12+k, j)
						in
							(
								set2dElem(y, i21 + k, j, cadd(x11, x21));
								set2dElem(y, i22 + k, j, butterfly(u1, x11, x21))
							)
						end
					))


fun getUElem(u: dcomplex array, idx, sign) :dcomplex =
		let 
			val (r, i) = Array.sub(u, idx)
		in
			case sign >= 1 of
				true => (r, i)
			| 	false => (r, ~i)
		end


fun fftzi_loop(is, li, lk, lj, ny, n1, u : dcomplex array, x : c2dArr, y: c2dArr) =
		let 
			val i = ref 0
			val i11 = ref 0
			val i12 = ref 0
			val i21 = ref 0
			val i22 = ref 0
			val _ = dbgmsg(fn _ => "in fftzI_loop")
			val u1 : dcomplex ref = ref (0.0, 0.0)
		in
			forLoop((0, li), fn i => (
						dbgmsg(fn _ => "i = " ^ istr(i) ^ " < " ^ istr(li));
						i11 := i * lk;
						i12 := !i11 + n1;
						i21 := i * lj;
						i22 := !i21 + lk;
						u1 := getUElem(u, li + (i), is);
						fftzk_loop(lk, ny, !i11, !i12, !i21, !i22, !u1, x, y);
						dbgmsg(fn _ => "done with fftzk, i="^istr(i)^", li="^istr(li)^", "))
				)
		end

fun fftz2(is : int, l , m, n, ny : int, u : (dcomplex array), x : c2dArr, y : c2dArr ) =
		let 
			val n1 = n div 2
			val lk = case l - 1 = 0 of
					true => 1
				| false => pow2(l-1)
			val li = case m - l = 0 of
					true => 1
				| false => pow2(m-l)
			val lj = 2*lk
			val _ = dbgmsg(fn _ => "in fftz2")
		in
			fftzi_loop(is, li, lk, lj, ny, n1, u, x, y)
		end

(* TODO: add forLoop call *)
fun fftzl_loop(is : int, m, n, u : (dcomplex array), x : c2dArr, y : c2dArr) =
		let
			val l = ref 1
		in
			while !l <= m do (
					dbgmsg(fn _ => "before 1st fftz2");
					fftz2(is, !l, m, n, FFTBLOCK, u, x, y)
					;	dbgmsg(fn _ => "before 2nd fftz2")
					; 	case !l + 1 <= m of
						true => fftz2(is, !l + 1, m, n, FFTBLOCK, u, y, x)
					|	false => ()
					;	dbgmsg(fn _ => "after 2st fftz2")
					;	l := !l + 2
				)
		end

(* TODO: add forLoop call *)
fun cfftz(is, m, n, x, y) = (
			dbgmsg(fn _ => "in cfftz\n");
			fftzl_loop(is, m, n, U, x, y)
			; dbgmsg(fn _ => "after fftzl_loop! (in cfftz)")
			;	if m mod 2 = 1 then
				forLoop((0, n), 
					fn j => forLoop((0, FFTBLOCK), 
							fn i => set2dElem(x, j, i, get2dElem(y, j, i)))
					)
			else ()
		)

(**************************************
 *				  CFFTS1 			*
 ***************************************)

fun load1(k : int, jj : int, d : int vector, x : c3dArr, y0 : c2dArr) =
		let 
			val d0 = Vector.sub(d, 0)
			val (width, height, yarr) = y0
			val _ = dbgmsg(fn _ => "LOAD1")
		in
			forLoop((0, FFTBLOCK), fn j =>
					forLoop((0, d0), fn i => 
							set2dElem(y0, i, j, get3dElem(x, k, (j + jj), i))
						)
				)
		end

fun store1(k : int, jj : int, d : int vector, x : c3dArr, y0 : c2dArr) =
		let 
			val d0 = Vector.sub(d, 0)
			val _ = dbgmsg(fn _ => "STORE1")
		in

			forLoop((0, FFTBLOCK), fn j =>
					forLoop((0, d0), fn i => 
							set3dElem(x, k, j + jj, i, get2dElem(y0, i, j))
						)
				)
		end

fun jjloop1(k : int, is,  d : int vector, x : c3dArr, xout : c3dArr) : unit =
		let 
			val d0 = Vector.sub(d, 0)
			val logd0 = ilog2(d0)
			val d1 = Vector.sub(d, 1)
			val y0 = init2d(NX, FFTBLOCKPAD)
			val y1 = init2d(NX, FFTBLOCKPAD)
			val jj = ref 0
		in
			while !jj <= d1 - FFTBLOCK do (* useful bc has custom inc *)
				(
					load1(k, !jj, d, x, y0)
					;	dbgmsg(fn _ => "jj = " ^ istr(!jj) ^ " ")
					;	cfftz(is, logd0, d0, y0, y1)
					;	store1(k, !jj, d, xout, y0)
					; 	jj := !jj + FFTBLOCK
				)
		end

fun cffts1(is : int, d : int vector, xin : c3dArr, xout : c3dArr) = 
		let
			val d2 = Vector.sub(d, 2)
		in
			ForkJoin.parfor NZ_GRAIN (0, d2) (fn idx => jjloop1(idx, is, d, xin, xout))
		end


(**************************************
 *				  CFFTS2 			*
 ***************************************)

fun load2(k : int, ii : int, d : int vector, x : c3dArr, y0 : c2dArr) =
		let 
			val d1 = Vector.sub(d, 1)
		in
			ForkJoin.parfor INNER_GRAIN (0, d1) (fn j =>
					forLoop((0, FFTBLOCK), fn i =>
							set2dElem(y0, j, i, get3dElem(x, k, j, (i + ii)))
						)
			)
		end

fun store2(k : int, ii : int, d : int vector, x : c3dArr, y0 : c2dArr) =
		let 
			val d1 = Vector.sub(d, 1)
		in
			ForkJoin.parfor INNER_GRAIN (0, d1) (fn j =>
					forLoop((0, FFTBLOCK), fn i =>
							set3dElem(x, k, j, i + ii, get2dElem(y0, j, i))
						)
			)
		end

fun iiloop2(k : int, is,  d : int vector, x : c3dArr, xout : c3dArr) : unit =
		let 
			val d1 = Vector.sub(d, 1)
			val logd1 = ilog2(d1)
			val d0 = Vector.sub(d, 0)
			val y0 = init2d(NX, FFTBLOCKPAD)
			val y1 = init2d(NX, FFTBLOCKPAD)
			val ii = ref 0
		in
			while !ii <= d0 - FFTBLOCK do 
				(
					load2(k, !ii, d, x, y0)
					;	cfftz(is, logd1, d1, y0, y1)
					;	store2(k, !ii, d, xout, y0)
					; 	ii := !ii + FFTBLOCK
				)
		end

fun cffts2(is : int, d : int vector, xin : c3dArr, xout : c3dArr) = 
		let
			val d2 = Vector.sub(d, 2)
			val g = 1
		in
			ForkJoin.parfor NZ_GRAIN (0, d2) (fn k => iiloop2(k, is, d, xin, xout))
		end


(**************************************
 *				  CFFTS3 			*
 ***************************************)

fun load3(j : int, ii : int, d : int vector, x : c3dArr, y0 : c2dArr) =
		let 
			val d2 = Vector.sub(d, 2)
		in
			ForkJoin.parfor INNER_GRAIN (0, d2) (fn k =>
					forLoop((0, FFTBLOCK), fn i =>
							set2dElem(y0, k, i, get3dElem(x, k, j, (i + ii)))
						)
			)
		end

fun store3(j : int, ii : int, d : int vector, x : c3dArr, y0 : c2dArr) =
		let 
			val d2 = Vector.sub(d, 2)
		in
			ForkJoin.parfor INNER_GRAIN (0, d2) (fn k =>
					forLoop((0, FFTBLOCK), fn i =>
							set3dElem(x, k, j, i + ii, get2dElem(y0, k, i))
						))
		end

fun iiloop3(j : int, is,  d : int vector, x : c3dArr, xout : c3dArr) : unit =
		let 
			val d2 = Vector.sub(d, 2)
			val logd2 = ilog2(d2)
			val d0 = Vector.sub(d, 0)
			val y0 = init2d(NX, FFTBLOCKPAD)
			val y1 = init2d(NX, FFTBLOCKPAD)
			val ii = ref 0
		in
			while !ii <= d0 - FFTBLOCK do 
				(
					load3(j, !ii, d, x, y0)
					;	cfftz(is, logd2, d2, y0, y1)
					;	store3(j, !ii, d, xout, y0)
					; 	ii := !ii + FFTBLOCK
				)
		end

fun cffts3(is : int, d : int vector, xin : c3dArr, xout : c3dArr) = 
		let
			val d1 = Vector.sub(d, 1)
			val g = 1
		in
			ForkJoin.parfor NY_GRAIN (0, d1) (fn idx => iiloop3(idx, is, d, xin, xout))
		end



fun fft(1 : int, input : c3dArr, output : c3dArr) : c3dArr = 
		let 
			val _ = dbgmsg (fn _ => "before cffts1")
			val _ = cffts1(1, d, input, input)
			(* val _ = write3dArr(input, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S1_out_mpl.txt") *)
			(* val _ = print(".") *)
			val _ = dbgmsg (fn _ => "before cffts2")
			val _ = cffts2(1, d, input, input)
			(* val _ = write3dArr(input, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S2_out_mpl.txt") *)
			(* val _ = print(".") *)
			val _ = dbgmsg (fn _ => "before cffts3")
			val _ = cffts3(1, d, input, output)
			(* val _ = write3dArr(output, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S3_out_mpl.txt") *)
			(* val _ = print(". ") *)
			val _ = dbgmsg (fn _ => "after cffts3")
		in
			output 
		end
	| fft (dir : int, input : c3dArr, output : c3dArr) : c3dArr = 
		let 
			val _ = dbgmsg ( fn _ => "in negative direction fft!")
			val _ = cffts3(~1, d, input, input)
			(* val _ = write3dArr(input, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S-3_out_mpl.txt") *)
			(* val _ = print(".") *)
			val _ = cffts2(~1, d, input, input)
			(* val _ = write3dArr(input, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S-2_out_mpl.txt") *)
			(* val _ = print(".") *)
			val _ = cffts1(~1, d, input, output)
			(* val _ = write3dArr(input, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S-1_out_mpl.txt") *)
			(* val _ = print(". ") *)
		in
			output
		end


val _ = dbgmsg(fn _ => "START FT.SML\n")





(*********************************** Setup ***********************************)
val _ = print ("\n\n NAS Parallel Benchmarks 3.0 structured MPL version" ^ 
		" - FT Benchmark\n\n")
val _ = print (" Size                : " ^ padStr(istr(NX), 3) ^ "x" ^ padStr(istr(NY), 3) ^ "x" ^ padStr(istr(NZ), 3) ^ "\n")
val _ = print (" Iterations          :     " ^ padStr(istr(NITER_DEFAULT), 7)^"\n")



(* val _ = print("Using " ^ istr(P) ^ " processors\n") *)

(* val _ = print "computing indexmap\n" *)
val indexmap = initI3d(NZ, NY, NX)
val _ = compute_indexmap(indexmap)

(* val _ = print("~generating~ conditions\n") *)
val T_GEN_START = clock()
val u1 :c3dArr = gen_initial_conditions(NZ, NY, NX)
(* val u1 :c3dArr = read_initial_conditions(init_conditions_file) *)
val T_GEN_END = clock()
(* val _ = print("done generating! (took " ^ rstr(T_GEN_END-T_GEN_START) ^ ")\n") *)
(* val uCopy : c3dArr = copy3d(u1) *)
(* val _ = print("done copying!\n") *)
(* val _ = print "writing conditions\n"
val _ = write3dArrFull(u1, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S_out_mpl.txt") *)

val u0 : c3dArr = init3d(#1 u1, #2 u1, #3 u1)
(* val _ = print ("U1 width="^Int.toString(#1 u1)^", height="^Int.toString(#2 u1)^", depth="^Int.toString(#3 u1)^"\n") *)

(* val _ = dbgmsg(fn _ => "initializing roots of unity\n") *)
val _ = fft_init(NX)

(* val _ = print "doing fft!\n" *)

val _ = fft(1, u1, u0)

(* val _ = write3dArrFull(u0, "ft_single_out_mpl.txt") *)


(***************************************  
			NOW ACTUALLY DO IT
****************************************)
(* val _ = print "------Now doing for real!------\n" *)

val _ = dbgmsg(fn _ =>"computing indexmap\n")

val _ = timer_start(T_TOTAL)

val _ = if TIMERS_ENABLED then timer_start(T_SETUP) else ()

(* val indexmap = initI3d(NZ, NY, NX) *)
val _ = compute_indexmap(indexmap)

(* val _ = print "reading conditions\n" *)
val u1 :c3dArr = gen_initial_conditions(NZ, NY, NX)
val u2 : c3dArr = init3d(#1 u1, #2 u1, #3 u1)

(* val _ = print "initializing roots of unity\n" *)
val _ = fft_init(NX)

val _ = if TIMERS_ENABLED then (timer_stop(T_SETUP); timer_start(T_FFT)) else ()

(* val _ = if debug then (
			printi3d(indexmap)
			;	print "\nUnity Array:\n"
			;	forLoop((30, 35), fn i => print("[" ^ Int.toString(i) ^ "]: " ^ getSciCompStr(Array.sub(U, i)) ^ "\n"))
			;	print "\nExp:\n"
			; 	forLoop((30, 35), fn i => print("[" ^ Int.toString(i) ^ "]: " ^ (Real.fmt (StringCvt.SCI (SOME(15))) (Array.sub(EX, i))) ^ "\n"))
			;	print "\n\n"
		) else () *)

(* val _ = print "doing fft!\n" *)

val _ = fft(1, u1, u0)

val _ = if TIMERS_ENABLED then timer_stop(T_FFT) else ()

val _ = forLoop((1, NITER_DEFAULT+1), fn iter => (
				if TIMERS_ENABLED then timer_start(T_EVOLVE) else ()
				;	evolve(u0, u1, iter, indexmap)
				;	if TIMERS_ENABLED then (timer_stop(T_EVOLVE); timer_start(T_FFT)) else ()
				(* ;	write3dArr(u1, "/home/generic/luke/NPB/MPL-NPB/FT/ft_evolve_out_mpl.txt") *)
				;	fft(~1, u1, u2)
				;	if TIMERS_ENABLED then (timer_stop(T_FFT); timer_start(T_CHECKSUM)) else ()
				; 	checksum(iter, u2)
				;	if TIMERS_ENABLED then timer_stop(T_CHECKSUM) else ()
			)
		)
val (valid, class) = verify(NX, NY, NZ, NITER_DEFAULT)

val _ = timer_stop(T_TOTAL)

val total_time = timer_read(T_TOTAL)


(* val _ = print("Time: " ^ rstr(T_TOTAL) ^ "\n")
(* val _ = print ("Class = " ^ Char.toString(class) ^ ", ") *)

val _ = case valid of 
		true => print "GOOD\n"
	|	false => print "BAD\n" *)

val _ = print_results("FT", class, NX, NY, NZ, NITER_DEFAULT, P, total_time, "          floating point", valid, "3.0 structured")


val _ = if TIMERS_ENABLED then print_timers() else ()

(* val _ = write3dArr(u2, "/home/generic/luke/NPB/MPL-NPB/FT/ft_S_final_out_mpl.txt") *)


(* val _ = print "writing to file!\n"
val _ = write3dArr(u0, "/home/generic/luke/NPB/MPL-NPB/FT/ft_single_out_mpl.txt") *)


