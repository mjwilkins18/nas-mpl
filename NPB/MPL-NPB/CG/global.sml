(* PARAM VALUES FOR CLASS S *)
val NA = 1400
val NONZER = 7
val NITER = 15
val SHIFT = 10.0
val RCOND = 0.1

(* END PARAMS *)


val NZ = NA * (NONZER+1) * (NONZER+1) + (NA * (NONZER+1))
val FIRSTROW = 1;
val LASTROW  = NA;
val FIRSTCOL = 1;
val LASTCOL  = NA;

val for = Util.for




(* GLOBAL VARIABLES *)
val TRAN : real ref  = ref 314159265.0;
val AMULT : real ref  = ref 1220703125.0;



(* FFI *)
(* val ipow46 = _import "ipow46" public: real * int -> real; *)
val makea = _import "makea" public: int * int * real array * int array * int array * int * int * int * int * int * real * int array * int array * real array * real array * int array * real * real ref * real -> unit;

val randlc = _import "randlc" public: real ref * real -> real;
