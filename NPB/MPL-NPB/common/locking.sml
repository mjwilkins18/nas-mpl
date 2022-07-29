fun lock_init() = 
	ref 0

fun lock (lock_ref : int ref) : unit =
	while (MLton.Parallel.compareAndSwap lock_ref (0, 1) <> 0) do ()

fun unlock (lock_ref : int ref) : int = 
	MLton.Parallel.compareAndSwap lock_ref (1, 0)



fun single_lock (lock_ref : int ref,
		single_function : unit -> unit ) : int = 
	
	if MLton.Parallel.compareAndSwap lock_ref (0, 1) = 0
	then (single_function(); MLton.Parallel.compareAndSwap lock_ref (1, 2))
	else (while MLton.Parallel.compareAndSwap lock_ref (2, 2) <> 2 do (); 0)


fun single_nowait (lock_ref : int ref,
			single_function : unit -> unit) = 
	if (MLton.Parallel.compareAndSwap lock_ref (0,1) = 0)
	then single_function()
	else ()


fun barrier (lock_ref : int ref,
		population_lock : int ref,
		population : int ref,
		pop_target : int ) : int = 
	
	if MLton.Parallel.compareAndSwap lock_ref (0, 1) = 0
	then (
		
		lock(population_lock);
		population := !population + 1;
		unlock(population_lock);		
		while (pop_target <> !population) do ();
		MLton.Parallel.compareAndSwap lock_ref (1, 2))
	else (	lock(population_lock);
		population := !population + 1;
		unlock(population_lock);
		while MLton.Parallel.compareAndSwap lock_ref (2, 2) <> 2 do (); 
		0)


