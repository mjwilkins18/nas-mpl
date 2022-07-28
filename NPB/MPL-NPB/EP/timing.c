#include <time.h>
#include <stdio.h>

#if defined(IBM)
#define wtime wtime
#elif defined(CRAY)
#define wtime WTIME
#else
#define wtime wtime_
#endif
#include <sys/time.h>

#include "ffi-help.h"

// int main(int argc, char **argv)
// {
// 	double begin_time = clock();
// 	for (int i = 0; i < 1000; i+= 2) {
// 		// printf(".");
// 		// if (i+1 % 100 == 0) {
// 		// 	// printf("\n");
// 		// }
// 		i -= 1;
// 	}
// 	double end_time = clock();

// 	double time_time = end_time-begin_time;
// 	printf("\ntime time: %lf\n", time_time);

// }

void wtime(double *t)
{
	static int sec = -1;
	struct timeval tv;
	gettimeofday(&tv, (void *)0);
	if (sec < 0)
		sec = tv.tv_sec;
	*t = (tv.tv_sec - sec) + 1.0e-6 * tv.tv_usec;
}

// double get_time() {
// 	return clock();
// }

double get_wtime() {
	static int sec = -1;
	struct timeval tv;
	gettimeofday(&tv, (void *)0);
	if (sec < 0)
		sec = tv.tv_sec;
	return (tv.tv_sec - sec) + 1.0e-6 * tv.tv_usec;
}

double get_cps() {
	return CLOCKS_PER_SEC;
}