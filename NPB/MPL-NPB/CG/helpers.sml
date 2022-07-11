
fun dot(v1 : real array, v2 : real array, start, last) : real = 
	let 
		val mul : real array = Array.tabulate last (fn j =>
			let
				val i = j + start
				val v1elem : real = Array.sub(v1, i)
				val v2elem : real = Array.sub(v2, i)
			in 
				v1elem * v2elem
			end)
	in
		Array.foldl + 0.0 mul
	end



(* val ZETA   = randlc( &tran, amult ); *)