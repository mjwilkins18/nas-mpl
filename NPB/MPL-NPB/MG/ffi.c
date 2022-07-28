#include "ffi-help.h"

/* parameters */
#define T_BENCH	1
#define	T_INIT	2


#if defined(USE_POW)
#define r23 pow(0.5, 23.0)
#define r46 (r23 * r23)
#define t23 pow(2.0, 23.0)
#define t46 (t23 * t23)
#else
#define r23 (0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5)
#define r46 (r23 * r23)
#define t23 (2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0)
#define t46 (t23 * t23)
#endif



/* functions prototypes */
//void setup_c(int nx[], int ny[], int nz[], int m1[], int m2[], int m3[], int *n1, int *n2, int *n3, int lt, int *is1, int *is2, int *is3, int *ie1, int *ie2, int *ie3);

void setup_c(long *nx, long *ny, long *nz, long *m1, long *m2, long *m3, long *n1, long *n2, long *n3, long lt, long *is1, long *is2, long *is3, long *ie1, long *ie2, long *ie3) {

    int k;

    for ( k = lt-1; k >= 1; k--) {
	nx[k] = nx[k+1]/2;
	ny[k] = ny[k+1]/2;
	nz[k] = nz[k+1]/2;
    }

    for (k = 1; k <= lt; k++) {
	m1[k] = nx[k]+2;
	printf("m1[%d] = %d\n", k, m1[k]);
	m2[k] = nz[k]+2;
	m3[k] = ny[k]+2;
    }

    *is1 = 1;
    *ie1 = nx[lt];
    *n1 = nx[lt]+2;
    *is2 = 1;
    *ie2 = ny[lt];
    *n2 = ny[lt]+2;
    *is3 = 1;
    *ie3 = nz[lt];
    *n3 = nz[lt]+2;

}


/*
void zero3_c(double ***z, long *n1, long *n2, long *n3) {


    int i1, i2, i3;
#pragma omp parallel for private(i1,i2,i3)
    for (i3 = 0;i3 < *n3; i3++) {
	for (i2 = 0; i2 < *n2; i2++) {
            for (i1 = 0; i1 < *n1; i1++) {
		z[i3][i2][i1] = 0.0;
	    }
	}
    }
}
*/

double randlc(double *x, double a)
{

	/*c---------------------------------------------------------------------
	c---------------------------------------------------------------------*/

	/*c---------------------------------------------------------------------
	c
	c   This routine returns a uniform pseudorandom double precision number in the
	c   range (0, 1) by using the linear congruential generator
	c
	c   x_{k+1} = a x_k  (mod 2^46)
	c
	c   where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
	c   before repeating.  The argument A is the same as 'a' in the above formula,
	c   and X is the same as x_0.  A and X must be odd double precision integers
	c   in the range (1, 2^46).  The returned value RANDLC is normalized to be
	c   between 0 and 1, i.e. RANDLC = 2^(-46) * x_1.  X is updated to contain
	c   the new seed x_1, so that subsequent calls to RANDLC using the same
	c   arguments will generate a continuous sequence.
	c
	c   This routine should produce the same results on any computer with at least
	c   48 mantissa bits in double precision floating point data.  On 64 bit
	c   systems, double precision should be disabled.
	c
	c   David H. Bailey     October 26, 1990
	c
	c---------------------------------------------------------------------*/
	double t1, t2, t3, t4, a1, a2, x1, x2, z;

	/*c---------------------------------------------------------------------
	c   Break A into two parts such that A = 2^23 * A1 + A2.
	c---------------------------------------------------------------------*/
	t1 = r23 * a;
	a1 = (int)t1;
	a2 = a - t23 * a1;

	/*c---------------------------------------------------------------------
	c   Break X into two parts such that X = 2^23 * X1 + X2, compute
	c   Z = A1 * X2 + A2 * X1  (mod 2^23), and then
	c   X = 2^23 * Z + A2 * X2  (mod 2^46).
	c---------------------------------------------------------------------*/
	t1 = r23 * (*x);
	x1 = (int)t1;
	x2 = (*x) - t23 * x1;
	t1 = a1 * x2 + a2 * x1;
	t2 = (int)(r23 * t1);
	z = t1 - t23 * t2;
	t3 = t23 * z + a2 * x2;
	t4 = (int)(r46 * t3);
	(*x) = t3 - t46 * t4;

	return (r46 * (*x));
}

void randlc_unit(double *x, double a){
	/* Same as randlc, but tossing the return value to deal with SML stupidity (avoiding dummy return values as seen in C code) */
	double answer = randlc(x, a);
	return;
}

void vranlc_c(int n, double *x_seed, double a, double* y)
{

	/*c---------------------------------------------------------------------
	c---------------------------------------------------------------------*/

	/*c---------------------------------------------------------------------
	c
	c   This routine generates N uniform pseudorandom double precision numbers in
	c   the range (0, 1) by using the linear congruential generator
	c
	c   x_{k+1} = a x_k  (mod 2^46)
	c
	c   where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
	c   before repeating.  The argument A is the same as 'a' in the above formula,
	c   and X is the same as x_0.  A and X must be odd double precision integers
	c   in the range (1, 2^46).  The N results are placed in Y and are normalized
	c   to be between 0 and 1.  X is updated to contain the new seed, so that
	c   subsequent calls to VRANLC using the same arguments will generate a
	c   continuous sequence.  If N is zero, only initialization is performed, and
	c   the variables X, A and Y are ignored.
	c
	c   This routine is the standard version designed for scalar or RISC systems.
	c   However, it should produce the same results on any single processor
	c   computer with at least 48 mantissa bits in double precision floating point
	c   data.  On 64 bit systems, double precision should be disabled.
	c
	c---------------------------------------------------------------------*/
	
	int i;
	double x, t1, t2, t3, t4, a1, a2, x1, x2, z;

	/*c---------------------------------------------------------------------
	c   Break A into two parts such that A = 2^23 * A1 + A2.
	c---------------------------------------------------------------------*/
	t1 = r23 * a;
	a1 = (int)t1;
	a2 = a - t23 * a1;
	x = *x_seed;

	/*c---------------------------------------------------------------------
	c   Generate N results.   This loop is not vectorizable.
	c---------------------------------------------------------------------*/
	for (i = 1; i <= n; i++)
	{

		/*c---------------------------------------------------------------------
		c   Break X into two parts such that X = 2^23 * X1 + X2, compute
		c   Z = A1 * X2 + A2 * X1  (mod 2^23), and then
		c   X = 2^23 * Z + A2 * X2  (mod 2^46).
		c---------------------------------------------------------------------*/
		t1 = r23 * x;
		x1 = (int)t1;
		x2 = x - t23 * x1;
		t1 = a1 * x2 + a2 * x1;
		t2 = (int)(r23 * t1);
		z = t1 - t23 * t2;
		t3 = t23 * z + a2 * x2;
		t4 = (int)(r46 * t3);
		x = t3 - t46 * t4;
		y[i] = r46 * x;
	}
	*x_seed = x;
}

double power( double a, int n ) {

/*--------------------------------------------------------------------
c-------------------------------------------------------------------*/

/*--------------------------------------------------------------------
c     power  raises an integer, disguised as a double
c     precision real, to an integer power
c-------------------------------------------------------------------*/
    double aj;
    int nj;
    double rdummy;
    double power;

    power = 1.0;
    nj = n;
    aj = a;

    while (nj != 0) {
	if( (nj%2) == 1 ) rdummy =  randlc( &power, aj );
	rdummy = randlc( &aj, aj );
	nj = nj/2;
    }
    
    return (power);
}

double sqrt_c(double a) {

/*--------------------------------------------------------------------
c-------------------------------------------------------------------*/

/*--------------------------------------------------------------------
c     sqrt  takes the sqrt of a double and returns a double
c-------------------------------------------------------------------*/
	return sqrt(a);
}

double fabs_c(double a) { 

/*--------------------------------------------------------------------
c-------------------------------------------------------------------*/

/*--------------------------------------------------------------------
c     fabs  takes the absolute value of a double
c-------------------------------------------------------------------*/
	return fabs(a);

}






