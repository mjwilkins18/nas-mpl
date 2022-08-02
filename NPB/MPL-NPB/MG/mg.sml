(******************* Functions *******************)

fun setup (nx : int array, ny : int array, nz : int array, m1 : int array, m2 : int array, m3 : int array, n1 : int ref, n2 : int ref, n3 : int ref, lt : int, is1 : int ref, is2 : int ref, is3 : int ref, ie1 : int ref, ie2 : int ref, ie3 : int ref) : unit = 
	let
	  val k = 0
	in
        (
	  negForLoop((lt-1, 1), fn k =>
	  (	
		Array.update(nx, k, Array.sub(nx, k+1) div 2); 
		(*print("nx[" ^ istr(k) ^ "] : " ^ istr(Array.sub(nx,k)) ^ "\n");*)
		Array.update(ny, k, Array.sub(ny, k+1) div 2);
		Array.update(nz, k, Array.sub(nz, k+1) div 2) 
	  ));
	  
	  forLoop((1, lt+1), fn k =>
	  (
		Array.update(m1, k, Array.sub(nx, k) + 2);
		(*print("m1[" ^ istr(k) ^ "] : " ^ istr(Array.sub(m1,k)) ^ "\n");*)
		Array.update(m2, k, Array.sub(nz, k) + 2);
		Array.update(m3, k, Array.sub(ny, k) + 2)
	  ));
	
	  is1 := 1;
	  ie1 := Array.sub(nx, lt);
	  n1 := Array.sub(nx, lt) + 2;
	 
	  is2 := 1;
	  ie2 := Array.sub(ny, lt);
	  n2 := Array.sub(ny, lt) + 2;
 

	  is3 := 1;
	  ie3 := Array.sub(nz, lt);
	  n3 := Array.sub(nz, lt) + 2
(*	
	  ;	print("SETUP\n");
	 	print(istr(lt) ^ " " ^ istr(Array.sub(nx, lt)) ^ " " ^ istr(Array.sub(ny, lt)) ^ " " ^ istr(Array.sub(nz, lt)) ^ " " ^ istr(!n1) ^ " " ^ istr(!n2) ^ " " ^ istr(!n3) ^ " "  ^ istr(!is1) ^ " " ^ istr(!is2) ^ " " ^ istr(!is3) ^ " " ^ istr(!ie1) ^ " " ^ istr(!ie2) ^ " " ^ istr(!ie3) ^ "\n")
*)
	)
	end

fun zero3 (z : c3dArr, n1 : int, n2 : int, n3 : int) : unit =
	  ForkJoin.parfor G(0, n3) (fn i3 =>
		forLoop((0, n2), fn i2 =>
			forLoop((0, n1), fn i1 =>
				set3dElem(z, i3, i2, i1, 0.0)
			)
		)
	  )

fun bubbleForLoop1 ((k : int, j : int), ten : c2dArr, j1 : i2dArr, j2 : i2dArr, j3 : i2dArr, m : int, ind : int) : unit = 
	forLoop((k, j), fn i =>
	(
	  if(get2dElem(ten, i, ind) > get2dElem(ten, i+1, ind)) then
	    	let
		  val temp = get2dElem(ten, i+1, ind)
		  val j1_temp = geti2dElem(j1, i+1, ind)
		  val j2_temp = geti2dElem(j2, i+1, ind)
		  val j3_temp = geti2dElem(j3, i+1, ind)
	    	in
		  set2dElem(ten, i+1, ind, get2dElem(ten, i, ind));
		  set2dElem(ten, i, ind, temp);

		  seti2dElem(j1, i+1, ind, geti2dElem(j1, i, ind));
		  seti2dElem(j1, i, ind, j1_temp);

		  seti2dElem(j2, i+1, ind, geti2dElem(j2, i, ind));
		  seti2dElem(j2, i, ind, j2_temp);

		  seti2dElem(j3, i+1, ind, geti2dElem(j3, i, ind));
		  seti2dElem(j3, i, ind, j3_temp)
		end
	  else ()
	))

fun bubbleForLoop2 ((k : int, j : int), ten : c2dArr, j1 : i2dArr, j2 : i2dArr, j3 : i2dArr, m : int, ind : int) : unit = 
	forLoop((k, j), fn i =>
	(
	  (* Note: the difference between these functions is the ">" vs. "<" in the if statement below *)
	  if(get2dElem(ten, i, ind) < get2dElem(ten, i+1, ind)) then
	    	let
		  val temp = get2dElem(ten, i+1, ind)
		  val j1_temp = geti2dElem(j1, i+1, ind)
		  val j2_temp = geti2dElem(j2, i+1, ind)
		  val j3_temp = geti2dElem(j3, i+1, ind)
	    	in
		  set2dElem(ten, i+1, ind, get2dElem(ten, i, ind));
		  set2dElem(ten, i, ind, temp);

		  seti2dElem(j1, i+1, ind, geti2dElem(j1, i, ind));
		  seti2dElem(j1, i, ind, j1_temp);

		  seti2dElem(j2, i+1, ind, geti2dElem(j2, i, ind));
		  seti2dElem(j2, i, ind, j2_temp);

		  seti2dElem(j3, i+1, ind, geti2dElem(j3, i, ind));
		  seti2dElem(j3, i, ind, j3_temp)
		end
	  else ()
	))

fun bubble (ten : c2dArr, j1 : i2dArr, j2 : i2dArr, j3 : i2dArr, m : int, ind : int) = 
	  if ind = 1 then
	    bubbleForLoop1((0, m-1), ten, j1, j2, j3, m, ind)
	  else
	    bubbleForLoop2((0, m-1), ten, j1, j2, j3, m, ind)

fun comm3 (u : c3dArr, n1 : int, n2 : int, n3 : int, kk : int) : unit = 
	(
	ForkJoin.parfor G (1, n3-1) (fn i3 =>
	(
		forLoop((1, n2-1), fn i2 =>
		(
			(*print("First spot i3=" ^ padStr(istr(i3), 3) ^ " i2=" ^ padStr(istr(i2), 3) ^ "\n");*)
			set3dElem(u, i3, i2, n1-1, get3dElem(u, i3, i2, 1));
			set3dElem(u, i3, i2, 0, get3dElem(u, i3, i2, n1-2))
			(*; print("u1f: " ^ rstr(get3dElem(u, i3, i2, n1-1)) ^ " u1b: " ^ rstr(get3dElem(u, i3, i2, 0)) ^ "\n")*)
		));
		forLoop((0, n1), fn i1 =>
		(
			(*print("Second spot i3=" ^ istr(i3) ^ " i1=" ^ istr(i1) ^ "\n");*)
			set3dElem(u, i3, n2-1, i1, get3dElem(u, i3, 1, i1));
			set3dElem(u, i3, 0, i1, get3dElem(u, i3, n2-2, i1))  
		))
	));
	ForkJoin.parfor G (0, n2) (fn i2 =>
	(
		forLoop((0,n1), fn i1 =>
		(
			(*print("Third spot i2=" ^ istr(i2) ^ " i1=" ^ istr(i1) ^ "\n");*)
			set3dElem(u, n3-1, i2, i1, get3dElem(u, 1, i2, i1));
			set3dElem(u, 0, i2, i1, get3dElem(u, n3-2, i2, i1))
		))
	))	
	)

fun zran3 (z : c3dArr, n1 : int, n2 : int, n3 : int, nx : int, ny : int, k : int) : unit =
	let
	  val MM = 10
	  val A = powerReal(5.0, 13)
	  val A = ref A
	  val X = 314159265.0

	  val i0 = 0
	  val m0 = ref 0
	  val m1 = ref 0
          val i1_gb = ref (MM - 1)
          val i0_gb = ref (MM - 1)
	  
	  val i1 = 0 
	  val i2 = 0 
	  val i3 = 0 
	  val d1 = 0 
	  val e1 = 0 
	  val e2 = 0 
	  val e3 = 0 

	  val xx = ref 0.0 
	  val x0 = ref 0.0 
	  val x1 = ref 0.0 
	  val a1 = 0.0 
	  val a2 = 0.0 
	  val ai = 0.0 

	  val ten = init2d(MM, 2)

	  val i = 0
	  val j1 = initI2d(MM, 2)
	  val j2 = initI2d(MM, 2)
	  val j3 = initI2d(MM, 2)

	  val jg = initI3d(4, MM, 2)

	  val rdummy = 0.0
	  val a1 = power_c(!A, nx)
	  val a2 = power_c(!A, nx*ny)
	  	
	  val _ = zero3(z, n1, n2, n3)

	  val i = !is1 - 1 + nx * (!is2 - 1 + ny * (!is3 - 1))

	  val ai = power_c(!A, i)
	  val d1 = !ie1 - !is1 +1
	  val e1 = !ie1 - !is1 +2
	  val e2 = !ie2 - !is2 +2
	  val e3 = !ie3 - !is3 +2
	  val x0 = ref X
	  val rdummy = randlc_c(x0, ai) 
	  (*val _ = print("rdummy: " ^ rstr(rdummy) ^ "\n") *)

	in(
	  forLoop((1, e3), fn i3 =>
	  (
		x1 := !x0;
		forLoop((1, e2), fn i2 =>
		(
			let
			  val y = Array.array(d1+1, 0.0)
			in 
			  xx := !x1;	
			  vranlc_c(d1, xx, !A, y);
			  (*forLoop((0, d1+1), fn q =>
			  (
			  	print("index: " ^ istr(q) ^ "vranlc: " ^ rstr(Array.sub(y, q)) ^ "\n")
			  ));*)
			  
			  forLoop((1, d1+1), fn i =>
			  (
				set3dElem(z, i3, i2, i, Array.sub(y,i))
			  ));			

			  (*set3dRow(z, i3, i2, y, d1, 1);*)
			  randlc_c_unit(x1, a1)
			end
		));

		randlc_c_unit(x0, a2)
	  ));

	  forLoop((0, MM), fn i =>
	  (
		set2dElem(ten, i, 1, 0.0);
		seti2dElem(j1, i, 1, 0);
		seti2dElem(j2, i, 1, 0);
		seti2dElem(j3, i, 1, 0);
		set2dElem(ten, i, 0, 1.0);
		seti2dElem(j1, i, 0, 0);
		seti2dElem(j2, i, 0, 0);
		seti2dElem(j3, i, 0, 0)
	  ));
				
	  (*forLoop((0, MM), fn i =>
	  (
		print("[" ^ rstr(get2dElem(ten, i, 0)) ^ "][" ^ rstr(get2dElem(ten, i, 1)) ^ "]\n")
	  ));*)
	  forLoop((1, n3-1), fn i3 =>
	  (
		forLoop((1, n2-1), fn i2 =>
		(
			forLoop((1, n1-1), fn i1 =>
			(
				(*print("z: " ^ rstr(get3dElem(z, i3, i2, i1)) ^ "\n");*)
				if(get3dElem(z, i3, i2, i1) > get2dElem(ten, 0, 1)) then
				    (set2dElem(ten, 0, 1, get3dElem(z, i3, i2, i1));
				    seti2dElem(j1, 0, 1, i1);
				    seti2dElem(j2, 0, 1, i2);
				    seti2dElem(j3, 0, 1, i3);
				    bubble(ten, j1, j2, j3, MM, 1))
				else ();
				if(get3dElem(z, i3, i2, i1) < get2dElem(ten, 0, 0)) then
				  (
				  (*print(rstr(get3dElem(z, i3, i2, i1)) ^ "from z is less than " ^ rstr(get2dElem(ten, 0, 0)) ^ "from ten\n");*)
				  set2dElem(ten, 0, 0, get3dElem(z, i3, i2, i1));
				  seti2dElem(j1, 0, 0, i1);
				  seti2dElem(j2, 0, 0, i2);
				  seti2dElem(j3, 0, 0, i3);
				  (*print("ten[0][0]: " ^ rstr(get2dElem(ten, 0, 0)) ^ "\n");
				  *)
				  bubble(ten, j1, j2, j3, MM, 0))
				else ()
				
			))
		))
	  ));


	  negForLoop((MM-1, 0), fn i =>
	  (
	  	let
		  val best1 = get3dElem(z, geti2dElem(j3, !i1_gb, 1), geti2dElem(j2, !i1_gb, 1), geti2dElem(j1, !i1_gb, 1))
		  val best2 = get3dElem(z, geti2dElem(j3, !i0_gb, 0), geti2dElem(j2, !i0_gb, 0), geti2dElem(j1, !i0_gb, 0))
		in
		(
		  if(Real.==(best1, get3dElem(z, geti2dElem(j3, !i1_gb, 1), geti2dElem(j2, !i1_gb, 1), geti2dElem(j1, !i1_gb, 1)))) then
		  (
			seti3dElem(jg, 0, i, 1, 0);
			seti3dElem(jg, 1, i, 1, !is1 - 1 + geti2dElem(j1, !i1_gb, 1));
			seti3dElem(jg, 2, i, 1, !is2 - 1 + geti2dElem(j2, !i1_gb, 1));
			seti3dElem(jg, 3, i, 1, !is3 - 1 + geti2dElem(j3, !i1_gb, 1));
			i1_gb := !i1_gb - 1
		  )
		  else
		  (
			seti3dElem(jg, 0, i, 1, 0);
			seti3dElem(jg, 1, i, 1, 0);
			seti3dElem(jg, 2, i, 1, 0);
			seti3dElem(jg, 3, i, 1, 0)
		  );

		  set2dElem(ten, i, 1, best1);

		  if(Real.==(best2, get3dElem(z, geti2dElem(j3, !i0_gb, 0), geti2dElem(j2, !i0_gb, 0), geti2dElem(j1, !i0_gb, 0)))) then
		  (
			
			seti3dElem(jg, 0, i, 0, 0);
			seti3dElem(jg, 1, i, 0, !is1 - 1 + geti2dElem(j1, !i0_gb, 0));
			seti3dElem(jg, 2, i, 0, !is2 - 1 + geti2dElem(j2, !i0_gb, 0));
			seti3dElem(jg, 3, i, 0, !is3 - 1 + geti2dElem(j3, !i0_gb, 0));
			i0_gb := !i0_gb - 1
		  )
		  else
		  (
			
			seti3dElem(jg, 0, i, 0, 0);
			seti3dElem(jg, 1, i, 0, 0);
			seti3dElem(jg, 2, i, 0, 0);
			seti3dElem(jg, 3, i, 0, 0)
		  );

		  set2dElem(ten, i, 0, best2)
		)
		end
	  ));

	  m1 := !i1_gb + 1;
	  m0 := !i0_gb + 1;

	  (*
	  print(" negative charges at\n");
	  forLoop((0, MM), fn i =>
	  (
		if i mod 5 = 0 then print("\n") else ();
		print(" (" ^ istr(geti3dElem(jg, 1, i, 0)) ^ "," ^ istr(geti3dElem(jg, 2, i, 0)) ^ "," ^ istr(geti3dElem(jg, 3, i, 0)) ^ ")")
	  ));
	  print("\n");
	  
	  print(" positive charges at\n");
	  forLoop((0, MM), fn i =>
	  (
		if i mod 5 = 0 then print("\n") else ();
		print(" (" ^ istr(geti3dElem(jg, 1, i, 1)) ^ "," ^ istr(geti3dElem(jg, 2, i, 1)) ^ "," ^ istr(geti3dElem(jg, 3, i, 1)) ^ ")")
	  ));
	  print("\n");

	  print(" small random numbers were\n");
	  negForLoop((MM-1, 0), fn i =>
	  (
		print(" " ^ rstr(get2dElem(ten, i, 0)))
	  ));
	  
	  print("\n");
	  
	  print(" large random numbers were\n");
	  negForLoop((MM-1, 0), fn i =>
	  (
		print(" " ^ rstr(get2dElem(ten, i, 1)))
	  ));

	  print("\n");
	  *)
	  (*print("m1 : " ^ istr(!m1) ^ "\n");*)
	  (*print("m0 : " ^ istr(!m0) ^ "\n");*)

	  ForkJoin.parfor G (0, n3) (fn i3 =>
	  (
		forLoop((0, n2), fn i2 =>
		(
			forLoop((0, n1), fn i1 =>
			(
				set3dElem(z, i3, i2, i1, 0.0)
			))
		))
	  ));	

	  negForLoop((MM-1, !m0), fn i =>
	  (
		(*
		print("iter: " ^ istr(i) ^ "Location: [" ^ istr(geti2dElem(j3, i, 0)) ^ "," ^ istr(geti2dElem(j2, i, 0)) ^ "," ^ istr(geti2dElem(j1, i, 0)) ^ "]\n");
		*)
		set3dElem(z, geti2dElem(j3, i, 0), geti2dElem(j2, i, 0), geti2dElem(j1, i, 0), ~1.0)
	  ));
	
	  negForLoop((MM-1, !m1), fn i  =>
	  (
		set3dElem(z, geti2dElem(j3, i, 1), geti2dElem(j2, i, 1), geti2dElem(j1, i, 1), 1.0)
	  ));

	  comm3(z, n1, n2, n3, k)
	)
	end
 
fun norm2u3 (r : c3dArr, n1 : int, n2 : int, n3 : int, rnm2 : real ref, rnmu : real ref, nx : int, ny : int, nz : int) =
	let
	  val s = ref 0.0
	  val tmp = ref 0.0
	  val n = nx*ny*nz
	in(
	  forLoop((1, n3-1), fn i3 =>
	  (
		let
	  	  val a = ref 0.0
		in
		  forLoop((1, n2-1), fn i2 =>
		  (
			forLoop((1, n3-1), fn i1 =>
			(
				let
				  val s_mul = get3dElem(r, i3, i2, i1) * get3dElem(r, i3, i2, i1)
				in
				  a := fabs_c(get3dElem(r, i3, i2, i1));
				  s := !s + s_mul;
				  if (Real.>(!a, !tmp)) then
				  	  tmp := !a
				  else ()
				end
			))
		  ))
		end
	  ));
	  
	  rnmu := !tmp;
	  rnm2 := sqrt_c(!s/Real.fromInt(n))

(*	 
	  ;	print("rnmu: " ^ rstr(!rnmu) ^ " rnm2: " ^ rstr(!rnm2) ^ "\n")
*)
	)
	end	

fun resid (u : c3dArr, v : c3dArr, r : c3dArr, n1 : int, n2 : int, n3 : int, a : real array, k : int) =
	(
	ForkJoin.parfor G (1, n3-1) (fn i3 =>
	(
		let
	  	  val u1 = Array.array(M, 0.0)
	  	  val u2 = Array.array(M, 0.0)
		in
		  forLoop((1, n2-1), fn i2 =>
		  (
			forLoop((0, n1), fn i1 =>
			(
				Array.update(u1, i1, get3dElem(u, i3, i2-1, i1) + get3dElem(u, i3, i2+1, i1) + get3dElem(u, i3-1, i2, i1) + get3dElem(u, i3+1, i2, i1));
				Array.update(u2, i1, get3dElem(u, i3-1, i2-1, i1) + get3dElem(u, i3-1, i2+1, i1) + get3dElem(u, i3+1, i2-1, i1) + get3dElem(u, i3+1, i2+1, i1))
			));
			forLoop((1, n1-1), fn i1 =>
			(
(*
				print("v: " ^ rstr(get3dElem(v, i3, i2, i1)) ^ " u: " ^ rstr(get3dElem(u, i3, i2, i1)) ^ " a0: " ^ rstr(Array.sub(a, 0)) ^ " a2: " ^ rstr(Array.sub(a, 2)) ^ " a3: " ^ rstr(Array.sub(a, 3)) ^ "\n");
*)
				set3dElem(r, i3, i2, i1, get3dElem(v, i3, i2, i1) - Array.sub(a, 0) * get3dElem(u, i3, i2, i1) - Array.sub(a, 2) * (Array.sub(u2, i1) + Array.sub(u1, i1-1) + Array.sub(u1, i1+1)) - Array.sub(a, 3)*(Array.sub(u2, i1-1) + Array.sub(u2, i1+1)))
(*			
			;	print("resid: " ^ rstr(get3dElem(r, i3, i2, i1)) ^ "\n")
*)			
			))
		  ))
		end
	));
	  
	comm3(r, n1, n2, n3, k)
	)

fun rprj3(r : c3dArr, m1k : int, m2k : int, m3k : int, s : c3dArr, m1j : int, m2j : int, m3j : int, k) = 
	let
	  val d1 = if m1k = 3 then 2 else 1
	  val d2 = if m2k = 3 then 2 else 1
	  val d3 = if m3k = 3 then 2 else 1
	in
	(
	  ForkJoin.parfor G (1, m3j-1) (fn j3 =>
	  (
		let
	  	  val i3 = ref 0
	  	  val i2 = ref 0
	  	  val i1 = ref 0
	  	  val x1 = Array.array(M, 0.0)
	  	  val y1 = Array.array(M, 0.0)
	  	  val x2 = ref 0.0
	  	  val y2 = ref 0.0
		in
		  i3 := 2*j3-d3;
		  forLoop((1, m2j-1), fn j2 =>
		  (
			i2 := 2*j2-d2;
			
			forLoop((1, m1j), fn j1 =>
			(
				i1 := 2*j1-d1;
				Array.update(x1, !i1, get3dElem(r, !i3+1, !i2, !i1) + get3dElem(r, !i3+1, !i2+2, !i1) + get3dElem(r, !i3, !i2+1, !i1) + get3dElem(r, !i3+2, !i2+1, !i1));
				Array.update(y1, !i1, get3dElem(r, !i3, !i2, !i1) + get3dElem(r, !i3+2, !i2, !i1) + get3dElem(r, !i3, !i2+2, !i1) + get3dElem(r, !i3+2, !i2+2, !i1))
			));

			forLoop((1, m1j-1), fn j1 =>
			(
				i1 := 2*j1-d1;
				y2 := get3dElem(r, !i3, !i2, !i1+1) + get3dElem(r, !i3+2, !i2, !i1+1) + get3dElem(r, !i3, !i2+2, !i1+1) + get3dElem(r, !i3+2, !i2+2, !i1+1);
				x2 := get3dElem(r, !i3+1, !i2, !i1+1) + get3dElem(r, !i3+1, !i2+2, !i1+1) + get3dElem(r, !i3, !i2+1, !i1+1) + get3dElem(r, !i3+2, !i2+1, !i1+1);
				set3dElem(s, j3, j2, j1, 0.5 * get3dElem(r, !i3+1, !i2+1, !i1+1) + 0.25 * (get3dElem(r, !i3+1, !i2+1, !i1) + get3dElem(r, !i3+1, !i2+1, !i1+2) + !x2) + 0.125 * (Array.sub(x1,!i1) + Array.sub(x1, !i1+2) + !y2) + 0.0625 * (Array.sub(y1, !i1) + Array.sub(y1, !i1+2)))
			))
		  ))
	  	end
	  ));

	  comm3(s, m1j, m2j, m3j, k-1)
	)
	end

fun psinv(r : c3dArr, u : c3dArr, n1 : int, n2 : int, n3 : int, c : real array, k : int) = 
	(
	ForkJoin.parfor G (1, n3-1) (fn i3 =>
	(
	let
	  val r1 = Array.array(M, 0.0)
	  val r2 = Array.array(M, 0.0)
	  (*val _ = print("n1: " ^ istr(n1) ^ " n2: " ^ istr(n2) ^ " n3: " ^ istr(n3) ^ "\n")*)
	in
	  forLoop((1, n2-1), fn i2 =>
	  (
		forLoop((0, n1), fn i1 =>
		(
			Array.update(r1, i1, get3dElem(r, i3, i2-1, i1) + get3dElem(r, i3, i2+1, i1) + get3dElem(r, i3-1, i2, i1) + get3dElem(r, i3+1, i2, i1));
			Array.update(r2, i1, get3dElem(r, i3-1, i2-1, i1) + get3dElem(r, i3-1, i2+1, i1) + get3dElem(r, i3+1, i2-1, i1) + get3dElem(r, i3+1, i2+1, i1))
		));
			
		forLoop((1, n1-1), fn i1 =>
		(
			set3dElem(u, i3, i2, i1, get3dElem(u, i3, i2, i1) + (Array.sub(c, 0) * get3dElem(r, i3, i2, i1)) + (Array.sub(c, 1) * (get3dElem(r, i3, i2, i1-1) + get3dElem(r, i3, i2, i1+1) + Array.sub(r1,i1))) + (Array.sub(c, 2) * (Array.sub(r2, i1) + Array.sub(r1, i1-1) + Array.sub(r1, i1+1)))) 
		))
	  ))
	end
	));

	  comm3(u, n1, n2, n3, k)
	)

fun interp(z : c3dArr, mm1 : int, mm2 : int, mm3 : int, u : c3dArr, n1 : int, n2 : int, n3 : int, k : int) = 
	let
	  val d1 = if n1=3 then 2 else 1
	  val d2 = if n2=3 then 2 else 1
	  val d3 = if n3=3 then 2 else 1
	  val t1 = if n1=3 then 1 else 0
	  val t2 = if n2=3 then 1 else 0
	  val t3 = if n3=3 then 1 else 0
	  
	in
	  if (n1 <> 3 andalso n2 <> 3 andalso n3 <> 3) then
	  (
		ForkJoin.parfor G (0, mm3-1) (fn i3 =>
		(
			let
	  		  val z1 = Array.array(M, 0.0)
	  		  val z2 = Array.array(M, 0.0)
	  		  val z3 = Array.array(M, 0.0)
			in
			  forLoop((0, mm2-1), fn i2 =>
			  (
				forLoop((0, mm1), fn i1 =>
				(
					Array.update(z1, i1, get3dElem(z, i3, i2+1, i1) + get3dElem(z, i3, i2, i1));
					Array.update(z2, i1, get3dElem(z, i3+1, i2, i1) + get3dElem(z, i3, i2, i1));
					Array.update(z3, i1, get3dElem(z, i3+1, i2+1, i1) + get3dElem(z, i3+1, i2, i1) + Array.sub(z1, i1))
					(*
					; print("z1: " ^ rstr(Array.sub(z1, i1)) ^ "\n")
					; print("z2: " ^ rstr(Array.sub(z2, i1)) ^ "\n")
					; print("z3: " ^ rstr(Array.sub(z3, i1)) ^ "\n")
					*)
				));

				forLoop((0, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3, 2*i2, 2*i1, get3dElem(u, 2*i3, 2*i2, 2*i1) + get3dElem(z, i3, i2, i1));
					set3dElem(u, 2*i3, 2*i2, 2*i1+1, get3dElem(u, 2*i3, 2*i2, 2*i1+1) + 0.5 * (get3dElem(z, i3, i2, i1+1) + get3dElem(z, i3, i2, i1)))
					(*
					; print(rstr(get3dElem(u, 2*i3, 2*i2, 2*i1)) ^ "\n")
					; print(rstr(get3dElem(u, 2*i3, 2*i2, 2*i1+1)) ^ "\n")
					*)
				));
			
				forLoop((0, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3, 2*i2+1, 2*i1, get3dElem(u, 2*i3, 2*i2+1, 2*i1) + 0.5 * Array.sub(z1, i1));
					set3dElem(u, 2*i3, 2*i2+1, 2*i1+1, get3dElem(u, 2*i3, 2*i2+1, 2*i1+1) + 0.25 * (Array.sub(z1, i1) + Array.sub(z1, i1+1)))
					(*					
					; print(rstr(get3dElem(u, 2*i3, 2*i2+1, 2*i1)) ^ "\n")
					; print(rstr(get3dElem(u, 2*i3, 2*i2+1, 2*i1+1)) ^ "\n")
					*)
				));

				forLoop((0, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3+1, 2*i2, 2*i1, get3dElem(u, 2*i3+1, 2*i2, 2*i1) + 0.5 * Array.sub(z2, i1));
					set3dElem(u, 2*i3+1, 2*i2, 2*i1+1, get3dElem(u, 2*i3+1, 2*i2, 2*i1+1) + 0.25 * (Array.sub(z2, i1) + Array.sub(z2, i1+1)))
					(*
					; print(rstr(get3dElem(u, 2*i3+1, 2*i2, 2*i1)) ^ "\n")
					; print(rstr(get3dElem(u, 2*i3+1, 2*i2, 2*i1+1)) ^ "\n")
					*)
				));
				
				forLoop((0, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3+1, 2*i2+1, 2*i1, get3dElem(u, 2*i3+1, 2*i2+1, 2*i1) + 0.25 * Array.sub(z3, i1));
					set3dElem(u, 2*i3+1, 2*i2+1, 2*i1+1, get3dElem(u, 2*i3+1, 2*i2+1, 2*i1+1) + 0.125 * (Array.sub(z3,i1) + Array.sub(z3, i1+1)))
					(*
					; print(rstr(get3dElem(u, 2*i3+1, 2*i2+1, 2*i1)) ^ "\n")
					; print(rstr(get3dElem(u, 2*i3+1, 2*i2+1, 2*i1+1)) ^ "\n")
					*)
				))
		
				(*	
				; print("break\n")
				*)
			  ))
			end
		))
	  )
	  else
          (
		ForkJoin.parfor G (d3, mm3-1) (fn i3 =>
		(
			forLoop((d2, mm2-1), fn i2 =>
			(
				forLoop((d1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-d3-1, 2*i2-d2-1, 2*i1-d1-1, get3dElem(u, 2*i3-d3-1, 2*i2-d2-1, 2*i1-d1-1) + get3dElem(z, i3-1, i2-1, i1-1))
				));
				
				forLoop((1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-d3-1, 2*i2-d2-1, 2*i1-t1-1, get3dElem(u, 2*i3-d3-1, 2*i2-d2-1, 2*i1-t1-1) + 0.5 * (get3dElem(z, i3-1, i2-1, i1) + get3dElem(z, i3-1, i2-1, i1-1)))
				))
			));
			
			forLoop((1, mm2-1), fn i2 =>
			(
				forLoop((d1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-d3-1, 2*i2-t2-1, 2*i1-d1-1, get3dElem(u, 2*i3-d3-1, 2*i2-t2-1, 2*i1-d1-1) + 0.5 * (get3dElem(z, i3-1, i2, i1-1) + get3dElem(z, i3-1, i2-1, i1-1)))
				));
				
				forLoop((1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-d3-1, 2*i2-t2-1, 2*i1-t1-1, get3dElem(u, 2*i3-d3-1, 2*i2-t2-1, 2*i1-t1-1) + 0.25 * (get3dElem(z, i3-1, i2, i1) + get3dElem(z, i3-1, i2-1, i1) + get3dElem(z, i3-1, i2, i1-1) + get3dElem(z, i3-1, i2-1, i1-1)))
				))
			))
		));

		ForkJoin.parfor G (1, mm3-1) (fn i3 =>
		(
			forLoop((d2, mm2-1), fn i2 =>
			(
				forLoop((d1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-t3-1, 2*i2-d2-1, 2*i1-d1-1, get3dElem(u, 2*i3-t3-1, 2*i2-d2-1, 2*i1-d1-1) + 0.5 * (get3dElem(z, i3, i2-1, i1-1) + get3dElem(z, i3-1, i2-1, i1-1)))
				));
				
				forLoop((1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-t3-1, 2*i2-d2-1, 2*i2-t1-1, get3dElem(u, 2*i3-t3-1, 2*i2-d2-1, 2*i2-t1-1) + 0.25 * (get3dElem(z, i3, i2-1, i1) + get3dElem(z, i3, i2-1, i1-1) + get3dElem(z, i3-1, i2-1, i1) + get3dElem(z, i3-1, i2-1, i1-1)))
				))
			));
			
			forLoop((1, mm2-1), fn i2 =>
			(
				forLoop((d1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-t3-1, 2*i2-t2-1, 2*i1*d1-1, get3dElem(u, 2*i3-t3-1, 2*i2-t2-1, 2*i1*d1-1) + 0.25 * (get3dElem(z, i3, i2, i1-1) + get3dElem(z, i3, i2-1, i1-1) + get3dElem(z, i3-1, i2, i1-1) + get3dElem(z, i3-1, i2-1, i1-1)))
				));
				
				forLoop((1, mm1-1), fn i1 =>
				(
					set3dElem(u, 2*i3-t3-1, 2*i2-t2-1, 2*i1-t1-1, get3dElem(u, 2*i3-t3-1, 2*i2-t2-1, 2*i1-t1-1) + 0.125 * (get3dElem(z, i3, i2, i1) + get3dElem(z, i3, i2-1, i1) + get3dElem(z, i3, i2, i1-1) + get3dElem(z, i3, i2-1, i1-1) + get3dElem(z, i3-1, i2, i1) + get3dElem(z, i3-1, i2-1, i1) + get3dElem(z, i3-1, i2, i1-1) + get3dElem(z, i3-1, i2-1, i1-1)))
				))
			))
		))	
	  )
	end

fun mg3P(u : c3dArr list, v : c3dArr, r : c3dArr list, a : real array, c : real array, n1 : int, n2 : int, n3 : int, lt : int, lb : int, m1 : int array, m2 : int array, m3 : int array) =
	let
	  val k_arg1 = lb
	  val k_arg2 = lt
	in
	(
	  (*print("lt: " ^ istr(lt) ^ " lb: " ^ istr(lb) ^ "\n");*)
	  negForLoop((lt, lb+1), fn k =>
	  (
		rprj3(List.nth(r, k), Array.sub(m1, k), Array.sub(m2, k), Array.sub(m3, k), List.nth(r, k-1), Array.sub(m1, k-1), Array.sub(m2, k-1), Array.sub(m3, k-1), k)
	  ));
	  
	  zero3(List.nth(u, k_arg1), Array.sub(m1, k_arg1), Array.sub(m2, k_arg1), Array.sub(m3, k_arg1));
	  psinv(List.nth(r, k_arg1), List.nth(u, k_arg1), Array.sub(m1, k_arg1), Array.sub(m2, k_arg1), Array.sub(m3, k_arg1), c, k_arg1);

	  forLoop((lb+1, lt), fn k =>
	  (
		zero3(List.nth(u, k), Array.sub(m1, k), Array.sub(m2, k), Array.sub(m3, k));

		interp(List.nth(u, k-1), Array.sub(m1, k-1), Array.sub(m2, k-1), Array.sub(m3, k-1), List.nth(u, k), Array.sub(m1, k), Array.sub(m2, k), Array.sub(m3, k), k);

		resid(List.nth(u, k), List.nth(r, k), List.nth(r, k), Array.sub(m1, k), Array.sub(m2, k), Array.sub(m3, k), a, k);

	 	psinv(List.nth(r, k), List.nth(u, k), Array.sub(m1, k), Array.sub(m2, k), Array.sub(m3, k), c, k)
	  ));

	  interp(List.nth(u, lt-1), Array.sub(m1, lt-1), Array.sub(m2, lt-1), Array.sub(m3, lt-1), List.nth(u, lt), n1, n2, n3, k_arg2);
	  resid(List.nth(u, lt), v, List.nth(r, lt), n1, n2, n3, a, k_arg2);
	  psinv(List.nth(r, lt), List.nth(u, lt), n1, n2, n3, c, k_arg2)
	)
	end


(********************* Setup *********************)
val _ = print ("\n\n NAS Parallel Benchmarks 3.0 MPL Version" ^ " - MG Benchmark\n\n")
val _ = print ("Class                : " ^ CLASS ^ "\n") 
val _ = print (" Size                : " ^ padStr(istr(NX), 3) ^ "x" ^ padStr(istr(NY), 3) ^ "x" ^ padStr(istr(NZ), 3) ^ "\n")
val _ = print (" Iterations          :     " ^ padStr(istr(NIT), 7)^"\n")


(******************* Initialization *************)
val _ = timer_clear(T_BENCH)
val _ = timer_clear(T_INIT)
val _ = timer_start(T_INIT)

val lt = LT
val nit = NIT

val nx = Array.array(MAXLEVEL+1, 0)
val ny = Array.array(MAXLEVEL+1, 0)
val nz = Array.array(MAXLEVEL+1, 0)

val _ = Array.update(nx, lt, NX)
val _ = Array.update(ny, lt, NY)
val _ = Array.update(nz, lt, NZ)

val a = Array.array(4, 0.0)
val c = Array.array(4, 0.0)
 
val _ = Array.update(a, 0, ~8.0/3.0)
val _ = Array.update(a, 1, 0.0)
val _ = Array.update(a, 2, 1.0/6.0)
val _ = Array.update(a, 3, 1.0/12.0)

fun smallClass(class: string) = 
  class = "A" orelse class = "S" orelse class = "W";
if smallClass(CLASS) then
let
 val _ = Array.update(c, 0, ~3.0/8.0)
 val _ = Array.update(c, 1, 1.0/32.0)
 val _ = Array.update(c, 2, ~1.0/64.0)
 val _ = Array.update(c, 3, 0.0)
in()
end
else
let
 val _ = Array.update(c, 0, ~3.0/17.0)
 val _ = Array.update(c, 1, 1.0/33.0)
 val _ = Array.update(c, 2, ~1.0/61.0)
 val _ = Array.update(c, 3, 0.0)
 in()
end;

val NM = (2 + (power 2 (LM-1)))
val NV = (2 + (power 2 (NDIM1-1))*(2 + (power 2 (NDIM2-1)))*(2+(power 2 (NDIM3-1))))
val NR = ((8*(NV+(NM*NM)+5*NM+7*LM)) div 7)
val NM2 = (2*NM*NM)
val debug_vec = Array.array(8, 0)
val m1 = Array.array(MAXLEVEL+1, 0)
val m2 = Array.array(MAXLEVEL+1, 0)
val m3 = Array.array(MAXLEVEL+1, 0)

val rnm2 = ref 0.0
val rnmu = ref 0.0
val epsilon = 0.00000001

val lb = 1
val n1 = ref 0
val n2 = ref 0
val n3 = ref 0 

val _ = setup(nx, ny, nz, m1, m2, m3, n1, n2, n3, lt, is1, is2, is3, ie1, ie2, ie3)

val u = negForLoopMakeU(lt, 1, m1, m2, m3, make_u_array)

val v = init3d(Array.sub(m3, lt), Array.sub(m2, lt), Array.sub(m1, lt))

val r = negForLoopMakeU(lt, 1, m1, m2, m3, make_u_array)

(*
val _ = print("u size: ")
val _ = print3dSize(List.nth(u, lt))
val _ = print("r size: ")
val _ = print3dSize(List.nth(r, lt))

val _ = print("n1 : " ^ padStr(istr(!n1), 3) ^ "\n")
val _ = print("n2 : " ^ padStr(istr(!n2), 3) ^ "\n")
val _ = print("n3 : " ^ padStr(istr(!n3), 3) ^ "\n")
val _ = print("u length : " ^ padStr(istr(List.length(u)), 3) ^ "\n")
val _ = print("v size: ")
val _ = print3dSize(v)
*)

val _ = zero3(List.nth(u, lt), !n1, !n2, !n3)
val _ = zran3(v, !n1, !n2, !n3, Array.sub(nx, lt), Array.sub(ny, lt), lt)

val _ = norm2u3(v, !n1, !n2, !n3, rnm2, rnmu, Array.sub(nx, lt), Array.sub(ny, lt), Array.sub(nz, lt))

val _ = resid(List.nth(u, lt), v, List.nth(r, lt), !n1, !n2, !n3, a, lt)
val _ = norm2u3(List.nth(r, lt), !n1, !n2, !n3, rnm2, rnmu, Array.sub(nx, lt), Array.sub(ny, lt), Array.sub(nz, lt)) 

val _ = mg3P(u, v, r, a, c, !n1, !n2, !n3, lt, lb, m1, m2, m3)
val _ = resid(List.nth(u, lt), v, List.nth(r, lt), !n1, !n2, !n3, a, lt)

val _ = setup(nx, ny, nz, m1, m2, m3, n1, n2, n3, lt, is1, is2, is3, ie1, ie2, ie3)

val _ = zero3(List.nth(u, lt), !n1, !n2, !n3)

val _ = zran3(v, !n1, !n2, !n3, Array.sub(nx, lt), Array.sub(ny, lt), lt)

val _ = timer_stop(T_INIT)


(******************* Benchmark **** *************)
val _ = timer_start(T_BENCH)

val _ = resid(List.nth(u, lt), v, List.nth(r, lt), !n1, !n2, !n3, a, lt)
val _ = norm2u3(List.nth(r, lt), !n1, !n2, !n3, rnm2, rnmu, Array.sub(nx, lt), Array.sub(ny, lt), Array.sub(nz, lt))

val _ = forLoop((1, nit+1), fn it => (mg3P(u, v, r, a, c, !n1, !n2, !n3, lt, lb, m1, m2, m3); resid(List.nth(u, lt), v, List.nth(r, lt), !n1, !n2, !n3, a, lt)))

val _ = norm2u3(List.nth(r, lt), !n1, !n2, !n3, rnm2, rnmu, Array.sub(nx, lt), Array.sub(ny, lt), Array.sub(nz, lt))

val _ = timer_stop(T_BENCH)

(************* Verification *********************)
val t = timer_read(T_BENCH)
val tinit = timer_read(T_INIT)

fun get_verify_value "S" = 0.5307707005573e~4
  | get_verify_value "A" = 0.2433365309e~5
  | get_verify_value "B" = 0.180056440132e~5
  | get_verify_value "C" = 0.570674826298e~6
  | get_verify_value _ = raise Fail "Unknown Class Size: Cannot Verify"

val verify_value = get_verify_value(CLASS)
val valid = ref false

val _ = if Real.<=(fabs_c(!rnm2 - verify_value), epsilon) then
	(
		valid := true;
		print(" VERIFICATION SUCCESSFUL\n");
		print(" L2 Norm is: " ^ rstr(!rnm2) ^ "\n");
		print(" Error is: " ^ rstr(!rnm2 - verify_value) ^ "\n")
	)
	else
	(
		print(" VERIFICATION FAILED\n");
		print(" L2 Norm is: " ^ rstr(!rnm2) ^ "\n");
		print(" The correct L2 Norm is: " ^ rstr(verify_value) ^ "\n")
	
	)

val _ = print_results("MG", List.nth(String.explode(CLASS), 0), NX, NY, NZ, NIT, P, t, "          floating point", !valid, "3.0 structured")






