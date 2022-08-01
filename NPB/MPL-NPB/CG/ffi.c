#include "ffi.h"


typedef int boolean;

#define TRUE	1
#define FALSE	0

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))
#define	pow2(a) ((a)*(a))

#define r23 (0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5 * 0.5)
#define r46 (r23 * r23)
#define t23 (2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0 * 2.0)
#define t46 (t23 * t23)


#define SEED 314159265.0
#define PI 3.141592653589793238
#define ALPHA 1.0e-6

void makea(int64_t n, int64_t nz, double a[], int64_t colidx[], int64_t rowstr[],
		  int64_t nonzer, int64_t firstrow, int64_t lastrow, int64_t firstcol,
		  int64_t lastcol, double rcond, int64_t arow[], int64_t acol[],
		  double aelt[], double v[], int64_t iv[], double shift, double* tran, 
		  double amult );
static void sparse(double a[], int64_t colidx[], int64_t rowstr[], int64_t n,
		   int64_t arow[], int64_t acol[], double aelt[],
		   int64_t firstrow, int64_t lastrow,
		   double x[], boolean mark[], int64_t nzloc[], int64_t nnza);
static void sprnvc(int64_t n, int64_t nz, double v[], int64_t iv[], int64_t nzloc[],
		   int64_t mark[], double* tran, double amult);
static int64_t icnvrt(double x, int64_t ipwr2);
static void vecset(int64_t n, double v[], int64_t iv[], int64_t *nzv, int64_t i, double val);

double randlc(double *x, double a);
void vranlc(int64_t n, double *x_seed, double a, double y[]);



/*---------------------------------------------------------------------
c       generate the test problem for benchmark 6
c       makea generates a sparse matrix with a
c       prescribed sparsity distribution
c
c       parameter    type        usage
c
c       input
c
c       n            i           number of cols/rows of matrix
c       nz           i           nonzeros as declared array size
c       rcond        r*8         condition number
c       shift        r*8         main diagonal shift
c
c       output
c
c       a            r*8         array for nonzeros
c       colidx       i           col indices
c       rowstr       i           row pointers
c
c       workspace
c
c       iv, arow, acol i
c       v, aelt        r*8
c---------------------------------------------------------------------*/
void makea(
    int64_t n,
    int64_t nz,
    double a[],		/* a[1:nz] */
    int64_t colidx[],	/* colidx[1:nz] */
    int64_t rowstr[],	/* rowstr[1:n+1] */
    int64_t nonzer,
    int64_t firstrow,
    int64_t lastrow,
    int64_t firstcol,
    int64_t lastcol,
    double rcond,
    int64_t arow[],		/* arow[1:nz] */
    int64_t acol[],		/* acol[1:nz] */
    double aelt[],	/* aelt[1:nz] */
    double v[],		/* v[1:n+1] */
    int64_t iv[],		/* iv[1:2*n+1] */
    double shift,
	double* tran,
	double amult )
{
    int64_t i, nnza, iouter, ivelt, ivelt1, irow, nzv;

/*--------------------------------------------------------------------
c      nonzer is approximately  (int(sqrt(nnza /n)));
c-------------------------------------------------------------------*/
    double size, ratio, scale;
    int64_t jcol;

    size = 1.0;
    ratio = pow(rcond, (1.0 / (double)n));
    nnza = 0;

/*---------------------------------------------------------------------
c  Initialize colidx(n+1 .. 2n) to zero.
c  Used by sprnvc to mark nonzero positions
c---------------------------------------------------------------------*/
#pragma omp parallel for default(shared) private(i)
    for (i = 1; i <= n; i++) {
	colidx[n+i] = 0;
    }
    for (iouter = 1; iouter <= n; iouter++) {
	nzv = nonzer;
	sprnvc(n, nzv, v, iv, &(colidx[0]), &(colidx[n]), tran, amult);
	vecset(n, v, iv, &nzv, iouter, 0.5);
	for (ivelt = 1; ivelt <= nzv; ivelt++) {
	    jcol = iv[ivelt];
	    if (jcol >= firstcol && jcol <= lastcol) {
		scale = size * v[ivelt];
		for (ivelt1 = 1; ivelt1 <= nzv; ivelt1++) {
	            irow = iv[ivelt1];
                    if (irow >= firstrow && irow <= lastrow) {
			nnza = nnza + 1;
			if (nnza > nz) {
			    printf("Space for matrix elements exceeded in"
				   " makea\n");
			    printf("nnza, nzmax = %d, %d\n", nnza, nz);
			    printf("iouter = %d\n", iouter);
			    exit(1);
			}
			acol[nnza] = jcol;
			arow[nnza] = irow;
			aelt[nnza] = v[ivelt1] * scale;
		    }
		}
	    }
	}
	size = size * ratio;
    }

/*---------------------------------------------------------------------
c       ... add the identity * rcond to the generated matrix to bound
c           the smallest eigenvalue from below by rcond
c---------------------------------------------------------------------*/
    for (i = firstrow; i <= lastrow; i++) {
	if (i >= firstcol && i <= lastcol) {
	    iouter = n + i;
	    nnza = nnza + 1;
	    if (nnza > nz) {
		printf("Space for matrix elements exceeded in makea\n");
		printf("nnza, nzmax = %d, %d\n", nnza, nz);
		printf("iouter = %d\n", iouter);
		exit(1);
	    }
	    acol[nnza] = i;
	    arow[nnza] = i;
	    aelt[nnza] = rcond - shift;
	}
    }

/*---------------------------------------------------------------------
c       ... make the sparse matrix from list of elements with duplicates
c           (v and iv are used as  workspace)
c---------------------------------------------------------------------*/
    sparse(a, colidx, rowstr, n, arow, acol, aelt,
	   firstrow, lastrow, v, &(iv[0]), &(iv[n]), nnza);


/*---------------------------------------------------------------------
c  Note: as a result of this call to makea:
c        values of j used in indexing rowstr go from 1 --> lastrow-firstrow+1
c        values of colidx which are col indexes go from firstcol --> lastcol
c        So:
c        Shift the col index vals from actual (firstcol --> lastcol ) 
c        to local, i.e., (1 --> lastcol-firstcol+1)
c---------------------------------------------------------------------*/
	for (int64_t j = 1; j <= lastrow - firstrow + 1; j++) {
		for (int64_t k = rowstr[j]; k < rowstr[j+1]; k++) {
			colidx[k] = colidx[k] - firstcol + 1;
		}
    }
}

/*---------------------------------------------------
c       generate a sparse matrix from a list of
c       [col, row, element] tri
c---------------------------------------------------*/
static void sparse(
    double a[],		/* a[1:*] */
    int64_t colidx[],	/* colidx[1:*] */
    int64_t rowstr[],	/* rowstr[1:*] */
    int64_t n,
    int64_t arow[],		/* arow[1:*] */
    int64_t acol[],		/* acol[1:*] */
    double aelt[],	/* aelt[1:*] */
    int64_t firstrow,
    int64_t lastrow,
    double x[],		/* x[1:n] */
    boolean mark[],	/* mark[1:n] */
    int64_t nzloc[],	/* nzloc[1:n] */
    int64_t nnza)
/*---------------------------------------------------------------------
c       rows range from firstrow to lastrow
c       the rowstr pointers are defined for nrows = lastrow-firstrow+1 values
c---------------------------------------------------------------------*/
{
    int64_t nrows;
    int64_t i, j, jajp1, nza, k, nzrow;
    double xi;

/*--------------------------------------------------------------------
c    how many rows of result
c-------------------------------------------------------------------*/
    nrows = lastrow - firstrow + 1;

/*--------------------------------------------------------------------
c     ...count the number of triples in each row
c-------------------------------------------------------------------*/
#pragma omp parallel for default(shared) private(j)
    for (j = 1; j <= n; j++) {
	rowstr[j] = 0;
	mark[j] = FALSE;
    }
    rowstr[n+1] = 0;
    
    for (nza = 1; nza <= nnza; nza++) {
	j = (arow[nza] - firstrow + 1) + 1;
	rowstr[j] = rowstr[j] + 1;
    }

    rowstr[1] = 1;
    for (j = 2; j <= nrows+1; j++) {
	rowstr[j] = rowstr[j] + rowstr[j-1];
    }

/*---------------------------------------------------------------------
c     ... rowstr(j) now is the location of the first nonzero
c           of row j of a
c---------------------------------------------------------------------*/
    
/*---------------------------------------------------------------------
c     ... preload data pages
c---------------------------------------------------------------------*/
#pragma omp parallel for default(shared) private(k,j)
      for(j = 0;j <= nrows-1;j++) {
         for(k = rowstr[j];k <= rowstr[j+1]-1;k++)
	       a[k] = 0.0;
      }
/*--------------------------------------------------------------------
c     ... do a bucket sort of the triples on the row index
c-------------------------------------------------------------------*/
    for (nza = 1; nza <= nnza; nza++) {
	j = arow[nza] - firstrow + 1;
	k = rowstr[j];
	a[k] = aelt[nza];
	colidx[k] = acol[nza];
	rowstr[j] = rowstr[j] + 1;
    }

/*--------------------------------------------------------------------
c       ... rowstr(j) now points to the first element of row j+1
c-------------------------------------------------------------------*/
    for (j = nrows; j >= 1; j--) {
	rowstr[j+1] = rowstr[j];
    }
    rowstr[1] = 1;

/*--------------------------------------------------------------------
c       ... generate the actual output rows by adding elements
c-------------------------------------------------------------------*/
    nza = 0;
#pragma omp parallel for default(shared) private(i)    
    for (i = 1; i <= n; i++) {
	x[i] = 0.0;
	mark[i] = FALSE;
    }

    jajp1 = rowstr[1];
    for (j = 1; j <= nrows; j++) {
	nzrow = 0;
	
/*--------------------------------------------------------------------
c          ...loop over the jth row of a
c-------------------------------------------------------------------*/
	for (k = jajp1; k < rowstr[j+1]; k++) {
            i = colidx[k];
            x[i] = x[i] + a[k];
            if ( mark[i] == FALSE && x[i] != 0.0) {
		mark[i] = TRUE;
		nzrow = nzrow + 1;
		nzloc[nzrow] = i;
	    }
	}

/*--------------------------------------------------------------------
c          ... extract the nonzeros of this row
c-------------------------------------------------------------------*/
	for (k = 1; k <= nzrow; k++) {
            i = nzloc[k];
            mark[i] = FALSE;
            xi = x[i];
            x[i] = 0.0;
            if (xi != 0.0) {
		nza = nza + 1;
		a[nza] = xi;
		colidx[nza] = i;
	    }
	}
	jajp1 = rowstr[j+1];
	rowstr[j+1] = nza + rowstr[1];
    }
}

/*---------------------------------------------------------------------
c       generate a sparse n-vector (v, iv)
c       having nzv nonzeros
c
c       mark(i) is set to 1 if position i is nonzero.
c       mark is all zero on entry and is reset to all zero before exit
c       this corrects a performance bug found by John G. Lewis, caused by
c       reinitialization of mark on every one of the n calls to sprnvc
---------------------------------------------------------------------*/
static void sprnvc(
    int64_t n,
    int64_t nz,
    double v[],		/* v[1:*] */
    int64_t iv[],		/* iv[1:*] */
    int64_t nzloc[],	/* nzloc[1:n] */
    int64_t mark[], 
	double* tran,
	double amult ) 	/* mark[1:n] */
{
    int64_t nn1;
    int64_t nzrow, nzv, ii, i;
    double vecelt, vecloc;

    nzv = 0;
    nzrow = 0;
    nn1 = 1;
    do {
	nn1 = 2 * nn1;
    } while (nn1 < n);

/*--------------------------------------------------------------------
c    nn1 is the smallest power of two not less than n
c-------------------------------------------------------------------*/

    while (nzv < nz) {
	vecelt = randlc(tran, amult);

/*--------------------------------------------------------------------
c   generate an integer between 1 and n in a portable manner
c-------------------------------------------------------------------*/
	vecloc = randlc(tran, amult);
	i = icnvrt(vecloc, nn1) + 1;
	if (i > n) continue;

/*--------------------------------------------------------------------
c  was this integer generated already?
c-------------------------------------------------------------------*/
	if (mark[i] == 0) {
	    mark[i] = 1;
	    nzrow = nzrow + 1;
	    nzloc[nzrow] = i;
	    nzv = nzv + 1;
	    v[nzv] = vecelt;
	    iv[nzv] = i;
	}
    }

    for (ii = 1; ii <= nzrow; ii++) {
	i = nzloc[ii];
	mark[i] = 0;
    }
}

/*---------------------------------------------------------------------
* scale a double precision number x in (0,1) by a power of 2 and chop it
*---------------------------------------------------------------------*/
static int64_t icnvrt(double x, int64_t ipwr2) {
    return ((int64_t)(ipwr2 * x));
}

/*--------------------------------------------------------------------
c       set ith element of sparse vector (v, iv) with
c       nzv nonzeros to val
c-------------------------------------------------------------------*/
static void vecset(
    int64_t n,
    double v[],	/* v[1:*] */
    int64_t iv[],	/* iv[1:*] */
    int64_t *nzv,
    int64_t i,
    double val)
{
    int64_t k;
    boolean set;

    set = FALSE;
    for (k = 1; k <= *nzv; k++) {
	if (iv[k] == i) {
            v[k] = val;
            set  = TRUE;
	}
    }
    if (set == FALSE) {
	*nzv = *nzv + 1;
	v[*nzv] = val;
	iv[*nzv] = i;
    }
}



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
	c   48 mantissa bits in double precision floating point64_t data.  On 64 bit
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
	a1 = (int64_t)t1;
	a2 = a - t23 * a1;

	/*c---------------------------------------------------------------------
	c   Break X into two parts such that X = 2^23 * X1 + X2, compute
	c   Z = A1 * X2 + A2 * X1  (mod 2^23), and then
	c   X = 2^23 * Z + A2 * X2  (mod 2^46).
	c---------------------------------------------------------------------*/
	t1 = r23 * (*x);
	x1 = (int64_t)t1;
	x2 = (*x) - t23 * x1;
	t1 = a1 * x2 + a2 * x1;
	t2 = (int64_t)(r23 * t1);
	z = t1 - t23 * t2;
	t3 = t23 * z + a2 * x2;
	t4 = (int64_t)(r46 * t3);
	(*x) = t3 - t46 * t4;

	return (r46 * (*x));
}

/*c---------------------------------------------------------------------
c---------------------------------------------------------------------*/

void vranlc(int64_t n, double *x_seed, double a, double y[])
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

	int64_t i;
	double x, t1, t2, t3, t4, a1, a2, x1, x2, z;

	/*c---------------------------------------------------------------------
	c   Break A into two parts such that A = 2^23 * A1 + A2.
	c---------------------------------------------------------------------*/
	t1 = r23 * a;
	a1 = (int64_t)t1;
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
		x1 = (int64_t)t1;
		x2 = x - t23 * x1;
		t1 = a1 * x2 + a2 * x1;
		t2 = (int64_t)(r23 * t1);
		z = t1 - t23 * t2;
		t3 = t23 * z + a2 * x2;
		t4 = (int64_t)(r46 * t3);
		x = t3 - t46 * t4;
		y[i] = r46 * x;
	}
	*x_seed = x;
}

// static void ipow46(double a, int64_t exponent, double *result)
// {

// 	/*--------------------------------------------------------------------
// 	c-------------------------------------------------------------------*/

// 	/*--------------------------------------------------------------------
// 	c compute a^exponent mod 2^46
// 	c-------------------------------------------------------------------*/

// 	double dummy, q, r;
// 	int64_t n, n2;

// 	/*--------------------------------------------------------------------
// 	c Use
// 	c   a^n = a^(n/2)*a^(n/2) if n even else
// 	c   a^n = a*a^(n-1)       if n odd
// 	c-------------------------------------------------------------------*/
// 	*result = 1;
// 	if (exponent == 0)
// 		return;
// 	q = a;
// 	r = 1;
// 	n = exponent;

// 	while (n > 1)
// 	{
// 		n2 = n / 2;
// 		if (n2 * 2 == n)
// 		{
// 			dummy = randlc(&q, q);
// 			n = n2;
// 		}
// 		else
// 		{
// 			dummy = randlc(&r, q);
// 			n = n - 1;
// 		}
// 	}
// 	dummy = randlc(&r, q);
// 	*result = r;
// }
