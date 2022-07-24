double	randlc(X, A)
double *X;
double *A;
{
      static int        KS=0;
      static double	R23, R46, T23, T46;
      double		T1, T2, T3, T4;
      double		A1;
      double		A2;
      double		X1;
      double		X2;
      double		Z;
      int     		i, j;

      if (KS == 0) 
      {
        R23 = 1.0;
        R46 = 1.0;
        T23 = 1.0;
        T46 = 1.0;
    
        for (i=1; i<=23; i++)
        {
          R23 = 0.50 * R23;
          T23 = 2.0 * T23;
        }
        for (i=1; i<=46; i++)
        {
          R46 = 0.50 * R46;
          T46 = 2.0 * T46;
        }
        KS = 1;
      }

/*  Break A into two parts such that A = 2^23 * A1 + A2 and set X = N.  */

      T1 = R23 * *A;
      j  = T1;
      A1 = j;
      A2 = *A - T23 * A1;

/*  Break X into two parts such that X = 2^23 * X1 + X2, compute
    Z = A1 * X2 + A2 * X1  (mod 2^23), and then
    X = 2^23 * Z + A2 * X2  (mod 2^46).                            */

      T1 = R23 * *X;
      j  = T1;
      X1 = j;
      X2 = *X - T23 * X1;
      T1 = A1 * X2 + A2 * X1;
      
      j  = R23 * T1;
      T2 = j;
      Z = T1 - T23 * T2;
      T3 = T23 * Z + A2 * X2;
      j  = R46 * T3;
      T4 = j;
      *X = T3 - T46 * T4;
      return(R46 * *X);
} 




void	create_seq( long MAX_KEY, long NUM_KEYS, int* Key_array )
{

	double seed = 314159265.00;
	double a = 1220703125.00; 
	double x;
	int  i;
	long k;	

        k = MAX_KEY/4;

	for (i=0; i<NUM_KEYS; i++)
	{
	    x = randlc(&seed, &a);
	    x += randlc(&seed, &a);
    	    x += randlc(&seed, &a);
	    x += randlc(&seed, &a);  

            Key_array[i] = k*x;
	}	

}


void full_verify(int NUM_KEYS, int* key_array, 
	int* key_buff_ptr_global, int* key_buff2, int* pv)
{
    int*    passed_verification = pv;
    int    i, j;
    int    k;
    int    m, unique_keys;

    
/*  Now, finally, sort the keys:  */
    for( i=0; i<NUM_KEYS; i++ )
        key_array[--key_buff_ptr_global[key_buff2[i]]] = key_buff2[i];


/*  Confirm keys correctly sorted: count incorrectly sorted keys, if any */

    j = 0;
    for( i=1; i<NUM_KEYS; i++ )
        if( key_array[i-1] > key_array[i] ){
	    //printf("Error when comparing %d > %d\n", key_array[i-1], key_array[i]);
            j++;
	}
	
	
    if( j != 0 )
    {
        printf( "Full_verify: number of keys out of sort: %d\n",
                j );
    }
    else {
	(*passed_verification)++;
	}  
}



void rank(      int iteration, 
		int MAX_KEY_LOG_2, 
		int NUM_BUCKETS_LOG_2,
		int* key_array,
		int MAX_ITERATIONS,
		int MAX_KEY,
		int TEST_ARRAY_SIZE,
		int* partial_verify_vals,
		int* test_index_array,
		int* key_buff1,
		int* key_buff2,
 		int NUM_KEYS,
		char CLASS,
		int* test_rank_array,
		int* key_buff_ptr_global,
		int* pv)
{
    int*    passed_verification = pv;
    int    i, j, k;
    int    l, m;

    int    shift = MAX_KEY_LOG_2 - NUM_BUCKETS_LOG_2;
    int    key;
    int    min_key_val, max_key_val;

    int	prv_buff1[MAX_KEY];

#pragma omp master
  {
    key_array[iteration] = iteration;
    key_array[iteration+MAX_ITERATIONS] = MAX_KEY - iteration;

/*  Determine where the partial verify test keys are, load into  */
/*  top of array bucket_size                                     */
    for( i=0; i<TEST_ARRAY_SIZE; i++ )
        partial_verify_vals[i] = key_array[test_index_array[i]];

/*  Clear the work array */
    for( i=0; i<MAX_KEY; i++ )
        key_buff1[i] = 0;
  }
#pragma omp barrier  

  for (i=0; i<MAX_KEY; i++)
      prv_buff1[i] = 0;

/*  Copy keys into work array; keys in key_array will be reused each iter. */
#pragma omp for nowait
    for( i=0; i<NUM_KEYS; i++ ) {
        key_buff2[i] = key_array[i];

/*  Ranking of all keys occurs in this section:                 */

/*  In this section, the keys themselves are used as their 
    own indexes to determine how many of each there are: their
    individual population                                       */

        prv_buff1[key_buff2[i]]++;  /* Now they have individual key   */
    }
                                       /* population                     */
    for( i=0; i<MAX_KEY-1; i++ )   
        prv_buff1[i+1] += prv_buff1[i];  


#pragma omp critical
    {
	for( i=0; i<MAX_KEY; i++ )
	    key_buff1[i] += prv_buff1[i];
    }

/*  To obtain ranks of each key, successively add the individual key
    population, not forgetting to add m, the total of lesser keys,
    to the first key population                                          */

#pragma omp barrier    
#pragma omp master
  {
    
/* This is the partial verify test section */
/* Observe that test_rank_array vals are   */
/* shifted differently for different cases */
    for( i=0; i<TEST_ARRAY_SIZE; i++ )
    {                                             
        k = partial_verify_vals[i];          /* test vals were put here */
        if( 0 <= k  &&  k <= NUM_KEYS-1 )
            switch( CLASS )
            {
                case 'S':
                    if( i <= 2 )
                    {
                        if( key_buff1[k-1] != test_rank_array[i]+iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'W':
                    if( i < 2 )
                    {
                        if( key_buff1[k-1] != 
                                          test_rank_array[i]+(iteration-2) )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'A':
                    if( i <= 2 )
        	    {
                        if( key_buff1[k-1] != 
                                          test_rank_array[i]+(iteration-1) )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
        	    }
                    else
                    {
                        if( key_buff1[k-1] != 
                                          test_rank_array[i]-(iteration-1) )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'B':
                    if( i == 1 || i == 2 || i == 4 )
        	    {
                        if( key_buff1[k-1] != test_rank_array[i]+iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
        	    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'C':
                    if( i <= 2 )
        	    {
                        if( key_buff1[k-1] != test_rank_array[i]+iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
        	    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
            }        
    }




/*  Make copies of rank info for use by full_verify: these variables
    in rank are local; making them global slows down the code, probably
    since they cannot be made register by compiler                        */

    if( iteration == MAX_ITERATIONS ) 
        key_buff_ptr_global = key_buff1;

  } /* end master */
}






void partial_verification(
int iteration,
char CLASS,
long TEST_ARRAY_SIZE,
long NUM_KEYS,
int* key_buff1,
int* test_rank_array,
int* partial_verify_vals,
int* pv
){

/*
for(int g = 0; g  < TEST_ARRAY_SIZE; g++) {
printf("%d: %ld\n", g, partial_verify_vals[g]);
}
*/


long i;
long k;
int* passed_verification = pv;


/* This is the partial verify test section */
/* Observe that test_rank_array vals are   */
/* shifted differently for different cases */
    for( i=0; i<TEST_ARRAY_SIZE; i++ )
    {                                             
        k = partial_verify_vals[i];          /* test vals were put here */
        if( 0 <= k  &&  k <= NUM_KEYS-1 )
            switch( CLASS )
            {
                case 'S':
                    if( i <= 2 )
                    {
                        if( key_buff1[k-1] != test_rank_array[i]+iteration )
                        {
				printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'W':
                    if( i < 2 )
                    {
                        if( key_buff1[k-1] != 
                                          test_rank_array[i]+(iteration-2) )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'A':
                    if( i <= 2 )
        	    {
                        if( key_buff1[k-1] != 
                                          test_rank_array[i]+(iteration-1) )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
        	    }
                    else
                    {
                        if( key_buff1[k-1] != 
                                          test_rank_array[i]-(iteration-1) )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'B':
                    if( i == 1 || i == 2 || i == 4 )
        	    {
                        if( key_buff1[k-1] != test_rank_array[i]+iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
        	    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
                case 'C':
                    if( i <= 2 )
        	    {
                        if( key_buff1[k-1] != test_rank_array[i]+iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
        	    }
                    else
                    {
                        if( key_buff1[k-1] != test_rank_array[i]-iteration )
                        {
                            printf( "Failed partial verification: "
                                  "iteration %d, test key %d\n", 
                                   iteration, i );
                        }
                        else
                            (*passed_verification)++;
                    }
                    break;
            }        
    }
} 
