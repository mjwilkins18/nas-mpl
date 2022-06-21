
#include "ffi-help.h"

/*
 */
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

#define SEED 314159265.0
#define A 1220703125.0
#define PI 3.141592653589793238
#define ALPHA 1.0e-6

/*c---------------------------------------------------------------------
c---------------------------------------------------------------------*/

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

/*c---------------------------------------------------------------------
c---------------------------------------------------------------------*/

void vranlc(int n, double *x_seed, double a, double y[])
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

static void ipow46(double a, int exponent, double *result)
{

	/*--------------------------------------------------------------------
	c-------------------------------------------------------------------*/

	/*--------------------------------------------------------------------
	c compute a^exponent mod 2^46
	c-------------------------------------------------------------------*/

	double dummy, q, r;
	int n, n2;

	/*--------------------------------------------------------------------
	c Use
	c   a^n = a^(n/2)*a^(n/2) if n even else
	c   a^n = a*a^(n-1)       if n odd
	c-------------------------------------------------------------------*/
	*result = 1;
	if (exponent == 0)
		return;
	q = a;
	r = 1;
	n = exponent;

	while (n > 1)
	{
		n2 = n / 2;
		if (n2 * 2 == n)
		{
			dummy = randlc(&q, q);
			n = n2;
		}
		else
		{
			dummy = randlc(&r, q);
			n = n - 1;
		}
	}
	dummy = randlc(&r, q);
	*result = r;
}

static double get(double *arr, int width, int height, int depth, int x, int y, int z)
{
	return arr[height * depth * x + depth * y + z];
}

static void set(double *arr, int width, int height, int depth, int x, int y, int z, double val)
{
	arr[height * depth * x + depth * y + z] = val;
}

int compute_initial_conditions(Pointer ur, Pointer ui, int width, int height, int depth)
{

	/*--------------------------------------------------------------------
	c-------------------------------------------------------------------*/

	/*--------------------------------------------------------------------
	c Fill in array u0 with initial conditions from
	c random number generator
	c-------------------------------------------------------------------*/

	double *ureal = (double *)ur; // convert to good types
	double *uimag = (double *)ui;

	int NX = depth;
	int NY = height;
	int NZ = width;
	int MAXDIM = NX;

	int k;
	double x0, start, an, dummy;
	double *tmp = malloc((NX * 2 * MAXDIM + 1) * sizeof(double));
	int i, j, t;

	start = SEED;
	/*--------------------------------------------------------------------
	c Jump to the starting element for our first plane.
	c-------------------------------------------------------------------*/
	ipow46(A, (1 - 1) * 2 * NX * NY + (1 - 1) * 2 * NX, &an);
	dummy = randlc(&start, an);
	ipow46(A, 2 * NX * NY, &an);

	/*--------------------------------------------------------------------
	c Go through by z planes filling in one square at a time.
	c-------------------------------------------------------------------*/
	for (k = 0; k < width; k++)
	{
		x0 = start;
		vranlc(2 * NX * NY, &x0, A, tmp);

		t = 1;
		for (j = 0; j < height; j++)
			for (i = 0; i < depth; i++)
			{
				ureal[(k * height * depth) + (j * depth) + i] = tmp[t++];
				uimag[(k * height * depth) + (j * depth) + i] = tmp[t++];
			}

		if (k != NZ)
			dummy = randlc(&start, an);
	}

	free(tmp);

	return 1;
	// if (WRITE_TO_FILE == 1 && !written)
	// {
	// 	written = 1;
	// 	printf("Writing to file!\n");
	// 	write3dArr(u0, "/home/generic/luke/NPB/MPL-NPB/FT/inputs/ft_C_input.txt");
	// 	printf("Done!\n");
	// }
}
