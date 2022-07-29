(************Functions***************)
fun conj_grad(colidx : int array, rowstr : int array, x : real array, z : real array, a : real array, p : real array, q : real array, r : real array) =
	let
	  val cgitmax = 25
	  val rho : real ref = ref 0.0
	  val retsum : real ref = ref 0.0
	in
	(
	  dbg("    FIRST FOR LOOP");
	  forLoop((1, NA + 1), fn j =>
	  (
		let
		  val xj = Array.sub(x, j)
		in
	      	  Array.update(q, j, 0.0);	
	      	  Array.update(z, j, 0.0);	
	      	  Array.update(r, j, xj);	
	      	  Array.update(p, j, xj)
	    	end
	  ));
         
	  rho := dot(r, r, 1, LASTCOL-FIRSTCOL+2);
	  
	  forLoop((1, cgitmax+1), fn cgit =>
	  (
		let 
		  val rho0 = !rho
		  val d : real ref = ref 0.0
		  val alpha = ref 0.0
		  val beta = ref 0.0
		in
		  rho := 0.0;
		  (* This loop can be unrolled for possible performance improvements *)
		  dbg("    SECOND FOR LOOP");
		  forLoop((1, (LASTROW-FIRSTROW+1)), fn j =>
			let
			  val sum : real ref = ref 0.0
			  val _ = dbg("j = " ^ istr(j))
			  val start : int = Array.sub(rowstr, j)
			  val stop : int = Array.sub(rowstr, j+1)
			in
			  dbg("    THIRD FOR LOOP");
			  forLoop((start, stop), fn k => 
			  (	
				let
				  val _ = dbg("k = " ^ istr(k))
				  val ak = Array.sub(a, k)
				  val _ = dbg("sub a done")
				  val pcol = Array.sub(p, Array.sub(colidx, k))
				in
				  sum := !sum + ak*pcol
				end
			  ));
				
			  Array.update(q, j, !sum)
			end);
		
			d := dot(p, q, 1, LASTCOL-FIRSTCOL+2);
			alpha := rho0 / !d;
			
			(* TODO: make a func to parallel do each together? *)
			(* need good parallel reduction *)
			Array.modifyi (fn (i, elem) => 
				if i >= 1 andalso i < LASTCOL-FIRSTCOL+2 then 
				  elem + !alpha * Array.sub(p, i) 
				else elem) z;
			Array.modifyi (fn (i, elem) => 
				if i >= 1 andalso i < LASTCOL-FIRSTCOL+2 then 
				  elem - !alpha*Array.sub(q, i) 
				else elem) r;

			rho := dot(r, r, 1, LASTCOL-FIRSTCOL+2);
			beta := !rho / rho0;
			Array.modifyi (fn (i, elem) => 
				if i >= 1 andalso i < LASTCOL-FIRSTCOL+2 then 
				  Array.sub(r, i) + !alpha * elem 
				else elem) p
	  	end
	  ));

	  forLoop((1, LASTROW-FIRSTROW+2), fn j =>
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
	  print_iter(iter, rnorm, SHIFT + 1.0 / !norm_temp11);
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
val _ = create_data(NA, NZ, a, colidx, rowstr, NONZER, FIRSTROW, LASTROW, FIRSTCOL, LASTCOL, RCOND, arow, acol, aelt, v, iv, SHIFT, TRAN, AMULT)

(* Necessary: edit makea to do the index shifting and all that jazz  *)

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




