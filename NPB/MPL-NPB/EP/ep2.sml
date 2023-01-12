(***************** Functions *********************)

fun startingSeedForLoop((i : int, j : int), ik : int ref, kk : int ref, t1 : real ref, t2 : real ref, t3 : real ref) : unit =
	if i > j then ()
	else
	(
		ik := !kk div 2;
		if (2 * !ik <> !kk) then (t3 := randlc_c(t1, !t2)) else ();
		if !ik <> 0 then
		(
			t3 := randlc_c(t2, !t2);
			kk := !ik;
			startingSeedForLoop((i+1, j), ik, kk, t1, t2, t3)
		)
		else ()
	)
		 

(********************* Setup *********************)
val _ = print ("\n\n NAS Parallel Benchmarks 3.0 MPL Version" ^ " - EP Benchmark\n\n")
val _ = print ("Class                : " ^ CLASS ^ "\n") 

val MK = 16
val MM = M - MK
val NN = lshift(MM)
val NK = lshift(MK)
val NQ = 10
val EPSILON = 1.0e~8
val A = 1220703125.0
val S = 271828183.0

val x = Array.array(2*NK, 0.0)
val q = Array.array(NQ, 0.0) 

val size = rstr(powerReal(2.0, M+1))

val _ = print(" Number of random points generated: " ^ size ^ "\n")

(******************* Initialization *************)
val _ = timer_start(T_INIT)

val t1 = ref 0.0
val t2 = ref 0.0
val t3 = ref 0.0
val t4 = ref 0.0
val x1 = ref 0.0
val x2 = ref 0.0
val tm = ref 0.0
val an = ref 0.0
val tt = ref 0.0
val gc = ref 0.0

val dum0 = ref 1.0
val dum1 = ref 1.0
val dum2 = ref 1.0
val dum_array = Array.array(1, 0.0)


val np = NN

val _ = vranlc_c(0, dum0, !dum1, dum_array)
val _ = randlc_c_unit(dum1, !dum2)

val _ = forLoop((0, 2*NK), fn i => Array.update(x, i, ~1.0e99))

val _ = timer_stop(T_INIT)

val _ = timer_clear(T_BENCH)
val _ = timer_clear(T_GAUS)
val _ = timer_clear(T_RAND)

(******************* Benchmark **** *************)
val _ = timer_start(T_BENCH)

val _ = vranlc_c(0, t1, A, x)

val _ = t1 := A

val _ = forLoop((1, MK+1+1), fn i => t2 := randlc_c(t1, !t1))

val _ = an := !t1
val _ = tt := S

val k_offset = ~1

fun tup_add(x : real*real, y : real*real) = ((#1 x) + (#1 y), (#2 x) + (#2 y));

(*Begin Parallel Region*)

val sxy_tuple = ref (0.0, 0.0);

fun parallelRegion() =
	let
	  val qq = Array.array(NQ, 0.0)
	  val lock_1 = lock_init()
	  val lock_2 = lock_init()
	in
	  sxy_tuple := (SeqBasis.reduce G tup_add (0.0, 0.0) (1, (np+1)) (fn k =>
	  (     let
		val kk = ref 0 
	  	val t1 = ref 0.0
	  	val t2 = ref 0.0
	  	val t3 = ref 0.0
	  	val t4 = ref 0.0
	  	val x1 = ref 0.0
	  	val x2 = ref 0.0
	  	val ik = ref 0
	  	val l = ref 0
		val sx_local = ref 0.0
		val sy_local = ref 0.0
                val qq_local = Array.array(NQ, 0.0) 
		val xprime = Array.array(2*NK, 0.0)
		
		in (
		kk := k_offset + k;
		t1 := S;
		t2 := !an;
		
		startingSeedForLoop((1, 100), ik, kk, t1, t2, t3);
		(* ^ This could be a target for further parallelization, 
		but it seems like each thread is doing this sequentially *)
		
		(*Random Number Generation*) 
		timer_start(T_RAND);
		vranlc_c_minus_one(2*NK, t1, A, xprime);
		timer_stop(T_RAND);
		
		(*Gaussian Deviates Computation*)
		timer_start(T_GAUS);
		forLoop((0, NK), fn i => (* Supposedly not vectorizable, double check with Mike *)
		(

			
			x1 := 2.0 * Array.sub(xprime, 2*i) - 1.0;
			x2 := 2.0 * Array.sub(xprime, 2*i+1) - 1.0;
			t1 := powerReal(!x1, 2) + powerReal(!x2, 2);
			if (!t1 <= 1.0) then
			(
				t2 := sqrt_c(~2.0 * log_c(!t1) / !t1);
				t3 := !x1 * !t2;
				t4 := !x2 * !t2;
				l := max_c(fabs_c(!t3), fabs_c(!t4));
				Array.update(qq_local, !l, Array.sub(qq_local, !l) + 1.0);
				sx_local := !sx_local + !t3;
				sy_local := !sy_local + !t4 
			)
			else ();
			()	
		));
		lock(lock_1);
                forLoop((0, NQ), fn i => (Array.update(qq, i, (Array.sub(qq_local, i) + Array.sub(qq, i)))));
		unlock(lock_1);
       		
                timer_stop(T_GAUS);
		
                (!sx_local, !sy_local))
		end
	  )) )

          ;     forLoop((0, NQ), fn i =>
	  (
		Array.update(q, i, (Array.sub(qq, i) + Array.sub(q, i)))	
	  ))
	end

val _ = parallelRegion()

val sx = (#1 (!sxy_tuple))
val sy = (#2 (!sxy_tuple))

(*End Parallel Region*) 

val _ = forLoop((0, NQ), fn i => (gc := !gc + Array.sub(q, i)))

val _ = timer_stop(T_BENCH)

(************* Verification *********************)
val t = timer_read(T_BENCH)
val tinit = timer_read(T_INIT)
val trand = timer_read(T_RAND)
val tgaus = timer_read(T_GAUS)

fun get_verify_value 24 = (~3.247834652034740e3, ~6.958407078382297e3)
  | get_verify_value 25 = (~2.863319731645753e3, ~6.320053679109499e3)
  | get_verify_value 28 = (~4.295875165629892e3, ~1.580732573678431e4)
  | get_verify_value 30 = (4.033815542441498e4, ~2.660669192809235e4)
  | get_verify_value 32 = (4.764367927995374e4, ~8.084072988043731e4)
  | get_verify_value _ = raise Fail "Unknown Class Size: Cannot Verify"

val verify_value = get_verify_value(M)
val vv1 = #1 verify_value
val vv2 = #2 verify_value

val valid = ref false
val epsilon = EPSILON


val _ = if Real.<=(fabs_c( (sx - vv1) / sx), epsilon) andalso Real.<=(fabs_c( (sy - vv2) / sy), epsilon) then
	(
		valid := true;
		print(" VERIFICATION SUCCESSFUL\n")
	)
	else
	(
		print(" VERIFICATION FAILED\n")
	)

val _ = print("\n\nEP Benchmark Results: \n")
val _ = print("CPU Time = " ^ rstr(t)  ^ "\n")
val _ = print("N = 2^" ^ istr(M)  ^ "\n")
val _ = print("No. Gaussian Pairs = " ^ rstr(!gc)  ^ "\n")
val _ = print("Sums = " ^ rstr(sx) ^ " " ^ rstr(sy) ^ "\n")
val _ = print("Counts:\n")
val _ = forLoop((0, NQ), fn i => ( print(istr(i) ^ " " ^ rstr(Array.sub(q, i)) ^ "\n") ))

val _ = print("Time in seconds = " ^ rstr(t) ^ "\n");
(*val _ = print("Gaussian pairs: " ^ rstr(tgaus) ^ "\n");
val _ = print("Random Numbers: " ^ rstr(trand) ^ "\n");*)
