val args = CommandLine.arguments()
val class = List.hd (  String.explode(  List.hd  args  )  ) : char


fun total_keys_assigner(class) =
    if class = #"S" then 16 else
    if class= #"W" then 20 else
    if class = #"A" then 23 else
    if class = #"B" then 25 else
    if class = #"C" then 27 else
    raise Fail "Unknown Class"

val total_keys_log_2 = total_keys_assigner class


fun max_key_assigner(class) =
    if class = #"S" then 11 else
    if class= #"W" then 16 else
    if class = #"A" then 19 else
    if class = #"B" then 21 else
    if class = #"C" then 23 else
    raise Fail "Unknown Class"

val max_key_log_2 = max_key_assigner class


fun num_buckets_assigner(class) =
    if class = #"S" then 9 else
    if class= #"W" then 10 else
    if class = #"A" then 10 else
    if class = #"B" then 10 else
    if class = #"C" then 10 else
    raise Fail "Unknown Class"

val num_buckets_log_2 = num_buckets_assigner class


fun lshift n =
    if n = 0 then 1
    else 2 * lshift( n - 1 )

val total_keys = lshift total_keys_log_2 
val max_key = lshift max_key_log_2
val num_buckets = lshift num_buckets_log_2
val num_keys = total_keys
val size_of_buffers = num_keys


val passed_verification = ref 0

val max_iterations = 10
val test_array_size = 5

val key_array = Array.array (size_of_buffers, 0)
val key_buff1 = Array.array (size_of_buffers, 0)
val key_buff2 = Array.array (size_of_buffers, 0)
val key_buff_ptr_global = key_buff1
val partial_verify_vals = Array.array (test_array_size, 0)

val bucket_size = Array.array (num_buckets, 0)
val bucket_ptrs = Array.array (num_buckets, 0)


val test_index_array = Array.array (test_array_size, 0)
val test_rank_array = Array.array (test_array_size, 0)

val s_test_index_array = Array.fromList [ 48427, 17148, 23627, 62548, 4431 ]
val s_test_rank_array = Array.fromList [ 0, 18, 346, 64917, 65463 ]

val w_test_index_array = Array.fromList [ 357773, 934767, 875723, 898999, 404505 ]
val w_test_rank_array = Array.fromList [ 1249, 11698, 1039987, 1043896, 1048018 ]

val a_test_index_array = Array.fromList [ 2112377, 662041, 5336171, 3642833, 4250760 ]
val a_test_rank_array = Array.fromList [ 104, 17523, 123928, 8288932, 8388264 ]

val b_test_index_array = Array.fromList [ 41869, 812306, 5102857, 18232239, 26860214 ]
val b_test_rank_array = Array.fromList [ 33422937, 10244, 59149, 33135281, 99 ]

val c_test_index_array = Array.fromList [ 44172927, 72999161, 74326391, 129606274, 21736814 ]
val c_test_rank_array = Array.fromList [ 61147, 882988, 266290, 133997595, 133525895 ]


fun forLoop((i : int, j : int), f : int -> unit) : unit = 
                 if i >=j then ()
                 else ( f(i); forLoop((i+1, j), f) )

fun forLoopArg((i : int, j : int), f : int * int ref -> unit, arg : int ref) : unit = 
                 if i >=j then ()
                 else ( f(i, arg); forLoopArg((i+1, j), f, arg) )


fun get_class_index_array (c : char) : int array =
	case c of
		#"S" => s_test_index_array
		| #"W" => w_test_index_array
		| #"A" => a_test_index_array
		| #"B" => b_test_index_array
		| #"C" => c_test_index_array


fun get_class_rank_array (c : char) : int array =
	case c of
		#"S" => s_test_rank_array
		| #"W" => w_test_rank_array
		| #"A" => a_test_rank_array
		| #"B" => b_test_rank_array
		| #"C" => c_test_rank_array


fun test_index_array_init (i : int) : unit =
	let
	val index = get_class_index_array(class)
	in Array.update(test_index_array, i, Array.sub(index, i))
	end
	
val _ = forLoop((0,test_array_size), test_index_array_init)


fun test_rank_array_init (i : int) : unit =
	let
	val rank = get_class_rank_array(class)
	in Array.update(test_rank_array, i, Array.sub(rank, i))
	end
	
val _ = forLoop((0,test_array_size), test_rank_array_init)


val _ = timer_clear(0)


val ffi_create_seq = _import "create_seq": int * int * int array-> unit;

val _ = ffi_create_seq(max_key, num_keys, key_array)


val ffi_rank = _import "rank":  int * int * int * int array * int * int * int *
				int array * int array * int array * int array *
				int * char * int array * int array * int ref -> unit;

fun call_ffi_rank(iter : int, pv : int ref) : unit = 
	ffi_rank(iter, 
		max_key_log_2,
		num_buckets_log_2,
		key_array,
		max_iterations,
		max_key,
		test_array_size,
		partial_verify_vals,
		test_index_array,
		key_buff1,
		key_buff2,
		num_keys,
		class,
		test_rank_array,
		key_buff_ptr_global,
		pv)

(*
val _ = call_ffi_rank(1, passed_verification)

val passed_verification = ref 0
*)


val _ = print( "\n\n NAS Parallel Benchmarks Parallel ML version - IS Benchmark\n\n" )
val _ = print(" Size:  " ^ Int.toString(total_keys) ^ "  (class " ^ Char.toString(class) ^ ")\n")
val _ = print(" Iterations:   " ^ Int.toString(max_iterations) ^ "\n")


fun non_s_printout() : unit = 
	if (class <> #"S")
	then print("\n   iteration\n")
	else ()

val _  = non_s_printout


val _ = timer_start(0)


fun rank_loop(x: int, pv : int ref) : unit = 
	let val _ = 
		if (class <> #"S")
		then print("        " ^ Int.toString(x) ^ "\n")
	    	else ()
	    val _ = call_ffi_rank(x, pv)
	in ()
	end


(*
val _ = forLoopArg((1, max_iterations + 1), rank_loop, passed_verification)
*)



	






fun rank(iteration : int) : unit = 
    let
	val prv_buff1 = Array.array(max_key, 0)
	
	fun first_master() = 	
		let
		val _ = Array.update(key_array, iteration, iteration)
		val _ = Array.update(key_array, (iteration + max_iterations), (max_key - iteration))
		
		fun load_partial_array (i : int) = 
			Array.update(partial_verify_vals, i, 
				Array.sub(key_array, Array.sub(test_index_array, i)))

		fun clear_work_array (i : int) = 
			Array.update(key_buff1, i, 0)
		
		in (
		forLoop((0, test_array_size), load_partial_array);
		forLoop((0, max_key), clear_work_array)
		)
		end

	fun for() = 
		let	
		fun rank_all_keys (i : int) = 

		    let
		    in(
			Array.update(key_buff2, i, Array.sub(key_array, i));
			Array.update(prv_buff1, Array.sub(key_buff2, i),
					 (Array.sub(prv_buff1, Array.sub(key_buff2, i)) + 1))
		    )
		    end	
		
		in (
		forLoop((0, num_keys), rank_all_keys) (* ParFor *)	
		)
		end

	fun blank() = 
		let
		fun prv_update (i : int) =

		    let
			val prv_val0 = Array.sub(prv_buff1, (i + 1))
			val prv_val1 = Array.sub(prv_buff1, i)
		    in (
			Array.update(prv_buff1, (i + 1), (prv_val0 + prv_val1))
		    )
		    end

		in (
		forLoop((0, (max_key - 1)), prv_update)
		)
		end


	fun critical() = 
		let
		fun key_buff_update (i : int)  =

		    let
			val prv_val = Array.sub(prv_buff1, i)
			val key_val = Array.sub(key_buff1, i)
		    in (
			Array.update(key_buff1, i, (prv_val + key_val))
		    )
		    end

		in (
		forLoop((0, max_key), key_buff_update)
		)
		end
	
	fun second_master() = 
		let
		val ffi_partial_verify = _import "partial_verification" : int * char * int * int *
									  int array * int array *
									  int array * int ref -> unit;
		in (
		ffi_partial_verify (iteration, class, test_array_size, num_keys, key_buff1, 
				    test_rank_array, partial_verify_vals, passed_verification)
		)
		end
	
    in (
	
	
	(* Master Section *)
	first_master();

	(* Barrier *)
	(* prv zeroing done above *)

	(* For Nowait *)
	for();

	(* No OMP Signature *)
   	blank();

	(* Critical Section *) (* something in this section is causing key_buff to get fucked up*)
	critical();

	(* Barrier *)

	(* Master Section *)
	second_master()	
    )
    end


val _ = rank(1)

val _ = passed_verification :=  0

val _ = forLoop((1, max_iterations + 1), rank)


(*
fun par_loop(x: int) : unit = 
	let val _ = 
		if (class <> #"S")
		then print("        " ^ Int.toString(x) ^ "\n")
	    	else ()
	    val _ = rank(x)
	in ()
	end


val _ = ForkJoin.parfor 1 (1, max_iterations + 1) par_loop
*)

(* TODO get number of threads *)
val p = MLton.Parallel.numberOfProcessors


val _ = timer_stop(0)

val total_time = timer_read(0)


val ffi_full_verify = _import "full_verify": int * int array * int array * 
						int array * int ref-> unit;
val _ = ffi_full_verify (num_keys, key_array, key_buff1, key_buff2, passed_verification)


(* Final Printout *)
val _ = print_results("IS", class, total_keys, 0, 0, 
			max_iterations, p, total_time, 
			"keys ranked", (!passed_verification = ((5*max_iterations) + 1)), "3.0 structured")





(* The following line has been recommended, but this works without it so far*)
(*OS.Process.exit OS.Process.success : unit*)
