(*val ffi_main = _import "main_c": unit -> int;
val return_value = ffi_main()
val _ = print (Int.toString(return_value) ^ "\n")*) 

val args = CommandLine.arguments()
val class = List.hd (  String.explode(  List.hd  args  )  ) : char
val _ = print("Starting Class: " ^ Char.toString class ^ "\n")


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


val _ = print("Total Keys Log 2: " ^ Int.toString total_keys_log_2 ^ "\n");
val _ = print("Max Key Log 2: " ^ Int.toString max_key_log_2 ^ "\n");
val _ = print("Num Buckets Log 2: " ^ Int.toString num_buckets_log_2 ^ "\n");

fun lshift n =
    if n = 0 then 1
    else 2 * lshift( n - 1 )

val total_keys = lshift total_keys_log_2 
val max_key = lshift max_key_log_2
val num_buckets = lshift num_buckets_log_2
val num_keys = total_keys
val size_of_buffers = num_keys

val _ = print("Total Keys: " ^ Int.toString total_keys ^ "\n")
val _ = print("Max Key: " ^ Int.toString max_key ^ "\n")
val _ = print("Num Buckets: " ^ Int.toString num_buckets ^ "\n")


val passed_verification = 0


val max_iterations = 10
val test_array_size = 5

val key_array = Array.array (size_of_buffers, 0)
val key_buff1 = Array.array (size_of_buffers, 0)
val key_buff2 = Array.array (size_of_buffers, 0)
val partial_verify_vals = Array.array (test_array_size, 0)

val bucket_size = Array.array (num_buckets, 0)
val bucket_ptrs = Array.array (num_buckets, 0)


val test_index_array = Array.array (test_array_size, 0)
val test_index_array = Array.array (test_array_size, 0)

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


(* Here RandLC is defined. However, it is only ever called by the function create_seq,
so I won't make an ffi call to randlc, but rather only to create seq*)


(* My ffi call to create_seq isn't actually changing key_array, 
so first I'm going to check that there are no problems with
randlc, here is the code to check that it works from the sml side*)

val ffi_randlc = _import "randlc": real ref * real ref -> real;

val seed = 314159265.00 : real
val ref_seed = ref seed

val a = 1220703125.00 : real
val ref_a = ref a

val return = ffi_randlc(ref_seed, ref_a)


val _ = print("Sample Randlc call result: " ^ Real.toString(return) ^ "\n")

(*This works, and gives "Garrett! Here: 0.794521911189"

From the C side, I added these lines at the start of main:
//Start
588 double gseed = 314159265.00;
589 double ga = 1220703125.00;
590 double rand_result = randlc(&gseed, &ga);
591 printf("Garrett! Here: %f\n", rand_result);
592 //End//

This gave "Garrett! Here: 0.794522"
which I take to mean randlc works as an ffi call
So now, I need to figure out why this isn't working
*)





val _ = print("key_array[0] before Create_seq call: " ^ 
	      Int.toString ( Array.sub (key_array, 0)) ^
	      "\n")

val ffi_create_seq = _import "create_seq": real * real * int * int * int array-> unit;

val _ = ffi_create_seq(314159265.00, 1220703125.00, max_key, num_keys, key_array)

val _ = print("key_array[0] after Create_seq call: " ^ 
	      Int.toString ( Array.sub (key_array, 0)) ^
	      "\n")



(*
val _ = print(Int.toString ( Array.sub (s_test_index_array, 0)) ^ "\n")
val _ = print(Int.toString ( Array.sub (s_test_index_array, 1)) ^ "\n")
val _ = print(Int.toString ( Array.sub (s_test_index_array, 2)) ^ "\n")
val _ = print(Int.toString ( Array.sub (s_test_index_array, 3)) ^ "\n")
val _ = print(Int.toString ( Array.sub (s_test_index_array, 4)) ^ "\n")
*)


(* 
fun foo (a, n) = 
	val _ = print ( sub (a, n) )
	val n = n + 1
	if n < length (a) then
		val _ = foo(a, n)
	else 
		val _ = print "Done"

fun prin_array vr_a = 
	val _ = foo (vr_a, 0)

val _ = prin_array(s_test_index_array)
*)




(* Then a typedef for int size, and some global definitions for the arrays and pointers used later*)
(* Then function prototypes for the later functions*)
(* Then def Randlc, which is a random number generator, this can be an ffi call*)
(* Then def create_seq, which populates the key array using randlc, this can be an ffi call*)
(* Then def full_verify, which accesses the global info to check that our operations were successful, can be an ffi call*)
(* Then def rank, which actually performs the work of the IS benchmark. This is timed when called, and will need to be ported*)
	(* rank consists of several major sections: *)

(* Then def main, which times, runs, and prints the whole thing*)

(* The following line has been recommended, but this works without it so far*)
(*OS.Process.exit OS.Process.success : unit
*)
