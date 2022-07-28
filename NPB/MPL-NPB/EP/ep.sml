(********************* Setup *********************)
val _ = print ("\n\n NAS Parallel Benchmarks 3.0 MPL Version" ^ " - EP Benchmark\n\n")
val _ = print ("Class                : " ^ CLASS ^ "\n") 

(******************* Initialization *************)
val _ = timer_start(TODO)
val _ = timer_stop(TODO)

(******************* Benchmark **** *************)
val _ = timer_start(T_BENCH)

val _ = timer_stop(T_BENCH)

(************* Verification *********************)
val t = timer_read(T_BENCH)
val tinit = timer_read(T_INIT)

fun get_verify_value "S" = 0.5307707005573e~4
  | get_verify_value "A" = 0.2433365309e~5
  | get_verify_value "B" = 0.180056440132e~5
  | get_verify_value "C" = 0.570674826298e~6
  | get_verify_value _ = raise Fail "Unknown Class Size: Cannot Verify"

val verify_value = get_verify_value(CLASS)
val valid = ref false

val _ = if Real.<=(fabs_c(!rnm2 - verify_value), epsilon) then
	(
		valid := true;
		print(" VERIFICATION SUCCESSFUL\n");
		print(" L2 Norm is: " ^ rstr(!rnm2) ^ "\n");
		print(" Error is: " ^ rstr(!rnm2 - verify_value) ^ "\n")
	)
	else
	(
		print(" VERIFICATION FAILED\n");
		print(" L2 Norm is: " ^ rstr(!rnm2) ^ "\n");
		print(" The correct L2 Norm is: " ^ rstr(verify_value) ^ "\n")
	
	)


