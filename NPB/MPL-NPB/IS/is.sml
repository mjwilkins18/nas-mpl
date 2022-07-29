val num_keys = total_keys
val size_of_buffers = num_keys

val passed_verification = ref 0

val max_iterations = 10
val test_array_size = 5

val key_array = Array.array (size_of_buffers, 0)
val key_buff1 = Array.array (size_of_buffers, 0)
val key_buff2 = Array.array (size_of_buffers, 0)

val partial_verify_vals = Array.array (test_array_size, 0)

val _ = timer_clear(0)

val ffi_create_seq = _import "create_seq": int * int * int array-> unit;

val _ = ffi_create_seq(max_key, num_keys, key_array)


val _ = print( "\n\n NAS Parallel Benchmarks Parallel ML version - IS Benchmark\n\n" )
val _ = print(" Size:  " ^ Int.toString(total_keys) ^ "  (class " ^ Char.toString(CLASS) ^ ")\n")
val _ = print(" Iterations:   " ^ Int.toString(max_iterations) ^ "\n")
val _  = if (CLASS <> #"S")
	 then print("\n   iteration\n")
	 else ()


val _ = timer_start(0)


fun rank(iteration : int,
	 lock_1 : int ref,
	 lock_2 : int ref,
	 lock_3 : int ref,
	 lock_4 : int ref,
	 pop_lock : int ref,
	 population : int ref) : unit = 
    
	
    let
	val prv_buff1 = Array.array(max_key, 0)
	
	fun first_master() = 	
		let
		val _ = Array.update(key_array, iteration, iteration)
		val _ = Array.update(key_array, (iteration + max_iterations), 
							(max_key - iteration))
		
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
		val l = lock_init()
		fun rank_all_keys (i : int) = 

		    let
		    in(
			Array.update(key_buff2, i, Array.sub(key_array, i));
			lock(l);
			Array.update(prv_buff1, Array.sub(key_buff2, i),
					 (Array.sub(prv_buff1, Array.sub(key_buff2, i)) + 1));
			unlock(l); ()
			)
		    end	
		
		in (
		ForkJoin.parfor G (0, num_keys) rank_all_keys	
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
		ffi_partial_verify (iteration, CLASS, test_array_size, num_keys, key_buff1, 
				    test_rank_array, partial_verify_vals, passed_verification)
		)
		end
	
    in (
	
	
	(* Master Section *)
	
	single_lock(lock_1, first_master);
	(* Barrier implied by single*)
	(* prv zeroing done above *)

	(* For Nowait *)
	for();
	(* No OMP Signature *)
   	blank();
	(* Critical Section *) 
	
	lock( lock_2 );
	critical();
	unlock( lock_2 );
	(* Barrier *)
	
(*	barrier(lock_3, pop_lock, population, max_iterations); *)
	(* Master Section *)

	single_lock(lock_4, second_master);
	()
    )
    end



val _ = rank(1, ref 0, ref 0, ref 0, ref 0, ref 0, ref (max_iterations - 1) )

val _ = passed_verification :=  0

val lock_1 = lock_init()
val lock_2 = lock_init()
val lock_3 = lock_init()
val lock_4 = lock_init()
val pop_lock = lock_init()
val population = lock_init()




fun par_loop(x: int) : unit = ( 

	if (CLASS <> #"S")
	then print("        " ^ Int.toString(x) ^ "\n")
	else ();
	lock_1 := 0;
	lock_3 := 0;
	lock_4 := 0;
	pop_lock := 0;
	population := 0;

	rank(x, lock_1, lock_2, lock_3, lock_4, pop_lock, population)

	)

val _ = forLoop((1, max_iterations + 1), par_loop) 

val _ = timer_stop(0)

val total_time = timer_read(0)


val ffi_full_verify = _import "full_verify": int * int array * int array * 
						int array * int ref-> unit;
val _ = ffi_full_verify (num_keys, key_array, key_buff1, key_buff2, passed_verification)


(* Final Printout *)
val _ = print_results("IS", CLASS, total_keys, 0, 0, 
			max_iterations, P, total_time, 
			"keys ranked", (!passed_verification = ((5*max_iterations) + 1)), "3.0 structured")


(* The following line has been recommended, but this works without it so far*)
(*OS.Process.exit OS.Process.success : unit*)
