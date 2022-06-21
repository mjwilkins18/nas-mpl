
fun istr(i : int) = Int.toString(i)
fun rstr(r : real) = Real.toString(r)

fun nspace(0) = ""
|	nspace(n) = " " ^ nspace(n-1)


fun padStr(s : string, nchars : int) =
	let
		val padn = nchars - String.size(s)
	in
		if padn >= 0 then nspace(padn) ^ s else s
	end


fun print_results(name : string, 
				class : char, 
				n1 : int, 
				n2 : int, 
				n3 : int, 
				niter : int, 
				nthreads : int, 
				t : real, 
				(* mops : real,  *)
				optype : string, 
				passed_verification : bool,
				npb_version: string) = (

    print( "\n\n" ^ name ^ " Benchmark Completed\n"); 
    print( " Class           =                        " ^ Char.toString(class) ^ "\n");

    if n2 = 0 andalso n3 = 0 then
        print( " Size            =             " ^ padStr(istr(n1), 12) ^ "\n")
    else
        print (" Size            =              " ^ padStr(istr(n1), 3) ^ "x" ^ padStr(istr(n2), 3) ^ "x" ^ padStr(istr(n3), 3) ^ "\n");

    print( " Iterations      =             " ^ padStr(istr(niter), 12) ^ "\n");
    
    print( " Threads         =             " ^ padStr(istr(nthreads), 12) ^ "\n");
 
    print( " Time in seconds =             " ^ padStr(rstr(t), 12) ^ "\n");

    (* print( " Mop/s total     =             " ^ padStr(rstr(mops), 12) ^ "\n" ); *)

    print( " Operation type  = " ^ padStr(optype, 24) ^ "\n");

    if passed_verification then
        print( " Verification    =               SUCCESSFUL\n" )
    else
        print( " Verification    =             UNSUCCESSFUL\n" );

    print( " Version         =           " ^ padStr(npb_version, 12) ^ "\n" )

				)





