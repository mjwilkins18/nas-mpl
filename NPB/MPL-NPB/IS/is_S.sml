val ffi_main = _import "main_c": unit -> int;
val return_value = ffi_main()
val _ = print (Int.toString(return_value) ^ "\n") 
