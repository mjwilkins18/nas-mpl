

(* Draft done *)
fun conj_grad(colidx : int array, 
		rowstr : int array, 
		x : real array,
		z : real array,
		a : real array,
		p : real array,
		q : real array,
		r : real array) =
	let
		val cgitmax = 25
		val rho : real ref = ref 0.0
		val retsum : real ref = ref 0.0
	in
		(
			for (1, NA + 1) (fn j =>
					let
						val xj = Array.sub(x, j)
					in
						Array.update(q, j, 0.0)
						;	Array.update(z, j, 0.0)
						;	Array.update(r, j, xj)
						;	Array.update(p, j, xj)
					end
			)
		;	rho := dot(r, r, 1, LASTCOL-FIRSTCOL+2)
		;	for (1, cgitmax+1) (fn cgit =>
				let 
					val rho0 = !rho
					val d : real ref = ref 0.0
					val alpha = ref 0.0
					val beta = ref 0.0
				in
					rho := 0.0
					(* This loop can be unrolled for possible performance improvements *)
				;	for (1, (LASTROW-FIRSTROW+1)) (fn j =>
						let
							val sum : real ref = ref 0.0
							val start : real = Array.sub(rowstr, j)
							val end : real = Array.sub(rowstr, j+1)
						in
							for (start, end) (fn k => 
									let
										val ak = Array.sub(a, k)
										val pcol = Array.sub(p, Array.sub(colidx, k))
									in
										sum := !sum + ak*pcol
									end
							)
							;	Array.update(q, j, !sum)
						end)
				;	d := dot(p, q, 1, LASTCOL-FIRSTCOL+2)
				;	alpha := rho0 / !d
				(* TODO: make a func to parallel do each together? *)
				(* ! need good parallel reduction *)
				;	Array.modifyi (fn (i, elem) => 
						if i >= 1 andalso i < LASTCOL-FIRSTCOL+2 then elem + alpha * Array.sub(p, i) else elem) z
				; 	Array.modifyi (fn (i, elem) => 
						if i >= 1 andalso i < LASTCOL-FIRSTCOL+2 then elem - alpha*Array.sub(q, i) else elem) r

				;	rho := dot(r, r, 1, LASTCOL-FIRSTCOL+2)
				; 	beta := !rho / rho0
				;	Array.modifyi (fn (i, elem) => 
						if i >= 1 andalso i < LASTCOL-FIRSTCOL+2 then Array.sub(r, i) + alpha * elem else elem) p
				end)
		;	for (1, LASTROW-FIRSTROW+2) (fn j =>
				let
					val d = Array.sub(x, j) - Array.sub(r, j)
				in
					retsum := !retsum + d*d
				end)
		Math.sqrt(!retsum)
		)
	end




(* ------- MAIN ------ *)

val zeta = randlc(TRAN, AMULT)

val a : real array = Array.array(NZ+1, 0.0)
val colidx : real array = Array.array(NZ+1, 0.0)
val rowstr : real array = Array.array(NA+2, 0.0)
val x : real array = Array.array(NA+2+1, 1.0)
val q : real array = Array.array(NA+2+1, 0.0)
val z : real array = Array.array(NA+2+1, 0.0)
val r : real array = Array.array(NA+2+1, 0.0)
val p : real array = Array.array(NA+2+1, 0.0)

val arow : real array = Array.array(NZ+1, 0.0)
val acol : real array = Array.array(NZ+1, 0.0)
val aelt : real array = Array.array(NZ+1, 0.0)
val v : real array = Array.array(NA+2, 0.0)
val iv : real array = Array.array(2*NA+2, 0.0)


(* TODO: just edit makea to allocate its own damn memory *)
val _ = makea(NA, NZ, a, colidx, rowidx, NONZER, 1, NA, 1, NA, RCOND, arow, acol, aelt, v, iv, SHIFT)
(* TODO ALSO EDIT makea to do the index shifting and all that jazz  *)

val rnorm = conj_grad(colidx, rowstr, x, z, a, p, q, r)







