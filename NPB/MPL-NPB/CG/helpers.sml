



fun dot(v1 : real array, v2 : real array, start, last) : real = 
	let 
		val mul : real array = Array.tabulate(last, (fn j =>
			let
				val i = j + start
				val v1elem : real = Array.sub(v1, i)
				val v2elem : real = Array.sub(v2, i)
			in 
				v1elem * v2elem
			end))
	in
		Array.foldl (fn (a, b) => a+b) 0.0 mul
	end

fun print_iter(0 : int, rnorm : real, zeta : real) = 
		()
	| print_iter(iter : int, rnorm : real, zeta : real) = 
		let
			val firstIter = if iter = 1 then print("   iteration           ||r||                 zeta\n") else ()
			val itStr = padStr(istr(iter), 5)
			val rnormStr = padStr((Real.fmt (StringCvt.SCI (SOME(14))) rnorm), 20)
			val zetaStr = padStr((Real.fmt (StringCvt.SCI (SOME(13))) zeta), 20)
		in
			print("    " ^ itStr ^ "       " ^ rnormStr ^ zetaStr ^ "\n")
		end


fun get_verification_info(1400, 7, 15, 10) =
			(#"S", 8.5971775078648)
	|	get_verification_info(7000, 8, 15, 12) =
			(#"W", 10.362595087124)
	|	get_verification_info(14000, 11, 15, 20) =
			(#"A", 17.130235054029)
	|	get_verification_info(75000, 13, 75, 60) =
			(#"B", 22.712745482631)
	|	get_verification_info(150000, 15, 75, 110) =
			(#"C", 28.973605592845)
	|	get_verification_info(na, nonzer, niter, shift) =
			(#"U", ~1.0)

fun print_verification(true, zeta : real, zeta_verify : real) =
	let 
		val zetaStr = padStr((Real.fmt (StringCvt.SCI (SOME(12))) zeta), 20)
		val errStr = padStr((Real.fmt (StringCvt.SCI (SOME(12))) (zeta - zeta_verify)), 20)
	in
		(	print(" VERIFICATION SUCCESSFUL\n")
	    ;	print(" Zeta is    " ^ zetaStr ^ "\n")
	    ;	print(" Error is   " ^ errStr ^ "\n")
		)
	end
	| print_verification(false, zeta : real, zeta_verify : real) =
	let 
		val zetaStr = padStr((Real.fmt (StringCvt.SCI (SOME(12))) zeta), 20)
		val trueZetaStr = padStr((Real.fmt (StringCvt.SCI (SOME(12))) zeta_verify), 20)
	in
		(	print(" VERIFICATION FAILED\n")
	    ;	print(" Zeta                " ^ zetaStr ^ "\n")
	    ;	print(" The correct zeta is " ^ trueZetaStr ^ "\n")
		)
	end

val _ = dbg("DONE HELPERS\n")
(* val ZETA   = randlc( &tran, amult ); *)	
