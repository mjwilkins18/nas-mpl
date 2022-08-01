(************Functions***************)
fun shift(lastrow : int, firstrow : int, firstcol : int, rowstr : int array, colidx : int array) = 
	forLoop((1, lastrow - firstrow + 2), fn j =>
	(
		forLoop((Array.sub(rowstr, j), Array.sub(rowstr, j+1)), fn k =>
		(
			Array.update(colidx, k, Array.sub(colidx, k) - firstcol + 1)
		))
	))

fun conj_grad(colidx : int array, rowstr : int array, x : real array, z : real array, a : real array, p : real array, q : real array, r : real array) =
	let
	  val cgitmax = 25
	  val rho : real ref = ref 0.0
	  val retsum : real ref = ref 0.0
	in
	(
	  forLoop((1, NA + 2), fn j =>
	  (
		Array.update(q, j, 0.0);	
	      	Array.update(z, j, 0.0);	
	      	Array.update(r, j, Array.sub(x, j));	
	      	Array.update(p, j, Array.sub(r, j))
	  ));
         
	  forLoop((1, LASTCOL-FIRSTCOL+2), fn j => 
	  (
		rho := !rho + Array.sub(r,j)*Array.sub(r,j)
	  ));

	  forLoop((1, cgitmax+1), fn cgit =>
	  (
		let 
		  val rho0 = !rho
		  val d = ref 0.0
		  val alpha = ref 0.0
		  val beta = ref 0.0
		in
		  rho := 0.0;
		  forLoop((1, (LASTROW-FIRSTROW+2)), fn j =>
		  (
			let
			  val sum = ref 0.0
			in

			  forLoop((Array.sub(rowstr, j), Array.sub(rowstr, j+1)), fn k => 
			  (	
				sum := !sum + Array.sub(a, k)*Array.sub(p, Array.sub(colidx, k))
			  ));
				
			  Array.update(q, j, !sum)
			end
		  ));
		
		  forLoop((1, LASTCOL-FIRSTCOL+2), fn j => 
		  (
			d := !d + Array.sub(p,j)*Array.sub(q,j)
		  ));

		  alpha := rho0 / !d;

		  forLoop((1, LASTCOL-FIRSTCOL+2), fn j =>
		  (
			Array.update(z, j, Array.sub(z, j) + !alpha*Array.sub(p, j));
			Array.update(r, j, Array.sub(r, j) - !alpha*Array.sub(q, j));
			rho := !rho + Array.sub(r,j)*Array.sub(r,j)
		  ));
			
		  beta := !rho / rho0;

		  forLoop((1, LASTCOL-FIRSTCOL+2), fn j =>
		  (
			Array.update(p, j, Array.sub(r, j) + !beta*Array.sub(p, j))
		  ))

	  	end
	  ));

	  forLoop((1, LASTROW-FIRSTROW+2), fn j =>
	  (
		let
		  val d = ref 0.0
		in
		  forLoop((Array.sub(rowstr, j), Array.sub(rowstr, j+1)), fn k =>
		  (
			d := !d + Array.sub(a, k)*Array.sub(z, Array.sub(colidx, k))
		  ));

		  Array.update(r, j, !d)
		end
	  ));

	  forLoop((1, LASTCOL-FIRSTCOL+2), fn j =>
	  (
		let
		  val d = Array.sub(x, j) - Array.sub(r, j)
	 	in
		  retsum := !retsum + d*d
		end
	  )); 
	  
	  Math.sqrt(!retsum)
	)
	end



fun do_cg_iter(iter : int, colidx : int array, rowstr : int array, x : real array, z : real array, a : real array, p : real array, q : real array, r : real array) = 
	let 
	  val rnorm : real = conj_grad(colidx, rowstr, x, z, a, p, q, r)
	  val norm_temp11 : real ref = ref 0.0
	  val norm_temp12 : real ref = ref 0.0
	in
	  (* parallelizable *)
	  forLoop((1, (LASTCOL-FIRSTCOL+2)), fn j =>
	  ( 
		let 
	    	  val xj = Array.sub(x, j)
	    	  val zj = Array.sub(z, j)
	  	in
	    	  norm_temp11 := !norm_temp11 + (xj * zj);
	    	  norm_temp12 := !norm_temp12 + (zj * zj)
	  	end
	  ));
	  
	  norm_temp12 := 1.0 / Math.sqrt(!norm_temp12);
	  print_iter(iter, rnorm, SHIFT + 1.0 / !norm_temp11); (* print zeta *)
	  forLoop((1, LASTCOL-FIRSTCOL+2), fn j =>
	  (
		let 
		  val zj = Array.sub(z, j)
		in
		  Array.update(x, j, !norm_temp12 * zj)
		end	
	  ));
	  
	  SHIFT + (1.0 / !norm_temp11) (* zeta *)
	end



(*************MAIN***************)
val _ = dbg("STARTING MAIN\n")
val zeta = randlc(TRAN, AMULT)

val colidx : int array = Array.array(NZ+1, 0)
val rowstr : int array = Array.array(NA+2, 0)
val arow : int array = Array.array(NZ+1, 0)
val acol : int array = Array.array(NZ+1, 0)
val iv : int array = Array.array(2*NA+2, 0)

val v : real array = Array.array(NA+2, 0.0)
val aelt : real array = Array.array(NZ+1, 0.0)
val a : real array = Array.array(NZ+1, 0.0)
val x : real array = Array.array(NA+2+1, 1.0)
val q : real array = Array.array(NA+2+1, 0.0)
val z : real array = Array.array(NA+2+1, 0.0)
val r : real array = Array.array(NA+2+1, 0.0)
val p : real array = Array.array(NA+2+1, 0.0)


val _ = print ("\n\n NAS Parallel Benchmarks 3.0 structured MPL version" ^ 
		" - CG Benchmark\n")
val _ = print (" Size                : " ^ padStr(istr(NA), 11) ^ "\n")
val _ = print (" Iterations          : " ^ padStr(istr(NITER), 5)^"\n")



val _ = dbg("PRE-CREATE\n")
(* TODO: just edit makea to allocate its own damn memory *)
val _ = makea_c(NA, NZ, a, colidx, rowstr, NONZER, FIRSTROW, LASTROW, FIRSTCOL, LASTCOL, RCOND, arow, acol, aelt, v, iv, SHIFT, TRAN, AMULT)

(* Necessary: edit makea to do the index shifting and all that jazz  *)
(* MW: Doing it in SML here instead: *)
val _ = shift(LASTROW, FIRSTROW, FIRSTCOL, rowstr, colidx)

(*val _ = forLoop((0, NZ+1), fn q => print("index: " ^ istr(q) ^ " colidx val: " ^ istr(Array.sub(colidx, q)) ^ "\n"))*)

val _ = dbg("PRE_FIRST_ITER, rowstr " ^ istr(Array.sub(rowstr, 1)))
(* Do one iteration to touch all the data *)
val _ = do_cg_iter(0, colidx, rowstr, x, z, a, p, q, r)


(* START TIMER *)
val _ = dbg("starting iters\n")
val _ = timer_clear(T_TOTAL)
val _ = timer_start(T_TOTAL)

(* now do actual iterations-1, only need return value from last iter *)
val _ = forLoop((1, NITER), fn iter => let val _ = do_cg_iter(iter, colidx, rowstr, x, z, a, p, q, r) in () end )
val zeta = do_cg_iter(NITER, colidx, rowstr, x, z, a, p, q, r)

val _ = timer_stop(T_TOTAL)
val _ = dbg("AFTER ITERS")
val total_time = timer_read(T_TOTAL)

val (class, zeta_verify) = get_verification_info(NA, NONZER, NITER, Real.trunc(SHIFT))
val epsilon = 1.0 * Math.pow(10.0, ~10.0)

val verified = class <> #"U" andalso Real.abs(zeta - zeta_verify) <= epsilon

val _ = print_verification(verified, zeta, zeta_verify)

val _ = print_results("CG", class, NA, 0, 0, NITER, P, total_time, "          floating point", verified, "3.0 structured")



(* STOP TIMER *)

(* verify w/ ffi here *)




