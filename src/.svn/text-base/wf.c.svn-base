/*
 *  Copyright (C) 2011-2013 J. Schiffner
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 or 3 of the License
 *  (at your option).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 *
 */

#include "wf.h"

/* window functions
 *
 * *weights		pointer to weights vector, which is changed in these functions
 * *dist		pointer to dist
 * *N			length of weights and dist
 * *bw			pointer to bandwidth parameter
 * *k			number of nearest neighbors
 *
 * Note that not all arguments passed to the window functions are always used.
 *
 */
/*
 * FIXME: fabs()? check for positive dist?
 */


/***********************************************************************************************/
/* sample 
 * taken from the R-help mailing list: 
 * https://stat.ethz.ch/pipermail/r-help/2009-April/194193.html
 * and slightly adapted
 */
void sample(int *k, int *n, int *y, int *x) {
	GetRNGstate();
	
	int i, j;
	for (i = 0; i < *n; i++)
		x[i] = i;
	for (i = 0; i < *k; i++) {
		j = *n * unif_rand();
		y[i] = x[j] + 1;
		x[j] = x[--*n];
	}
	
	PutRNGstate();
	
}


/***********************************************************************************************/
/* biweight 
 * variant 1: fixed bandwidth
 */
void biweight1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = dist[i] < *bw ? 15/(double) 16 * pow(1 - pow(dist[i]/ *bw, 2), 2)/ *bw : 0;		
	}
}


/* cauchy 
 * variant1: fixed bandwidth
 */
void cauchy1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = 1/(M_PI * (1 + pow(dist[i]/ *bw, 2)) * *bw);
	}
}


/* cosine 
 * variant1: fixed bandwidth 
 */
void cosine1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = dist[i] < *bw ? (1 + cos(M_PI * dist[i]/ *bw))/(2 * *bw) : 0;
	}
}


/* epanechnikov 
 * variant1: fixed bandwidth 
 */
void epanechnikov1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = dist[i] < *bw ? 0.75 * (1 - pow(dist[i]/ *bw, 2))/ *bw : 0;		
	}
}


/* exponential 
 * variant1: fixed bandwidth 
 */
void exponential1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = 0.5 * exp(-dist[i]/ *bw)/ *bw;
	}
}


/* gaussian 
 * variant1: fixed bandwidth 
 */
void gaussian1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = dnorm(dist[i], 0, *bw, 0);
	}
}


/* optcosine 
 * variant1: fixed bandwidth 
 */
void optcosine1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = fabs(dist[i]) < *bw ? M_PI_4 * cos(M_PI * dist[i]/(2 * *bw))/ *bw : 0;		
	}
}


/* rectangular 
 * variant1: fixed bandwidth 
 */
void rectangular1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = fabs(dist[i]) < *bw ? 0.5/ *bw : 0;		
	}
}


/* triangular 
 * variant1: fixed bandwidth 
 */
void triangular1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0)
			error("'dist' must be positive");
		weights[i] = dist[i] < *bw ? (1 - dist[i]/ *bw)/ *bw : 0;
	}
}


/***********************************************************************************************/
/* biweight 
 * variant 2: fixed bandwidth, k nearest neighbors only
 */
void biweight2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}

	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dist[i] < *bw ? 15/(double) 16 * pow(1 - pow(dist[i]/ *bw, 2), 2)/ *bw : 0;		
		} else {
			weights[i] = 0;
		}
	}
}


/* cauchy 
 * variant 2: fixed bandwidth, k nearest neighbors only
 */
void cauchy2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 1/(M_PI * (1 + pow(dist[i]/ *bw, 2)) * *bw);
		} else {
			weights[i] = 0;
		}
	}
}


/* cosine 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void cosine2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dist[i] < *bw ? (1 + cos(M_PI * dist[i]/ *bw))/(2 * *bw) : 0;
		} else {
			weights[i] = 0;
		}
	}
}


/* epanechnikov 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void epanechnikov2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dist[i] < *bw ? 0.75 * (1 - pow(dist[i]/ *bw, 2))/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
}


/* exponential 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void exponential2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 0.5 * exp(-dist[i]/ *bw)/ *bw;
		} else {
			weights[i] = 0;
		}
	}
}


/* gaussian 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void gaussian2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dnorm(dist[i], 0, *bw, 0);
		} else {
			weights[i] = 0;
		}
	}
}


/* optcosine 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void optcosine2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dist[i] < *bw ? M_PI_4 * cos(M_PI * dist[i]/(2 * *bw))/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
}


/* rectangular 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void rectangular2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dist[i] < *bw ? 0.5/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
}


/* triangular 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void triangular2 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dist[i] < *bw ? (1 - dist[i]/ *bw)/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
}


/***********************************************************************************************/
/* biweight 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void biweight3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 15/(double) 16 * pow(1 - pow(dist[i]/a, 2), 2)/a;
		} else {
			weights[i] = 0;
		}
	}
}


/* cauchy 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void cauchy3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 1/(M_PI * (1 + pow(dist[i]/a, 2)) * a);
		} else {
			weights[i] = 0;
		}
	}
}


/* cosine 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void cosine3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = (1 + cos(M_PI * dist[i]/a))/(2 * a);
		} else {
			weights[i] = 0;
		}
	}
}


/* epanechnikov 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void epanechnikov3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 0.75 * (1 - pow(dist[i]/a, 2))/a;
		} else {
			weights[i] = 0;
		}
	}
}


/* exponential 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void exponential3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 0.5 * exp(-dist[i]/a)/a;
		} else {
			weights[i] = 0;
		}
	}
}


/* gaussian 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void gaussian3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = dnorm(dist[i], 0, a, 0);
		} else {
			weights[i] = 0;
		}
	}
}


/* optcosine 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void optcosine3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = M_PI_4 * cos(M_PI * dist[i]/(2 * a))/a;
		} else {
			weights[i] = 0;
		}
	}
}


/* rectangular 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void rectangular3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = 0.5/a;
		} else {
			weights[i] = 0;
		}
	}
}


/* triangular 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void triangular3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	
	/* calculate the k nearest neighbor bandwidth */
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	
	/* determine the nearest neighbors */
	int nn = 0;					// number of neighbors
	int neighbors[*N];			// 0 1 neighbor indicator
	for (i = 0; i < *N; i++) {
		if (dist[i] < a) {
			neighbors[i] = 1;
			nn += 1;
		} else {
			neighbors[i] = 0;
		}
	}
	
	/* number of neighbors may be larger than k due to ties in dist */
	if (nn > *k) {				// number of neighbors too large
		int size;				// number of neighbors to remove
		size = nn - *k;
		int y[size];			// index of neighbors to remove
		int tied[nn];			// index of tied distances (only first nt entries are relevant)
		int nt = 0;				// number of tied distances
		for (i = 0; i < *N; i++) {
			if (dist[i] == distcopy[*k-1]) {
				nt += 1;
				tied[nt] = i;
			}
		}
		int x[nt];						// index vector 1,2, ..., nt
		for (i = 0; i < nt; i++)
			x[i] = i;
		sample(&size, &nt, y, x);		// sample *size indices from nt indices
		for (i = 0; i < size; i++) {
			neighbors[tied[y[i]]] = 0;	// remove selected neighbors
		}
	}
	
	/* calculate weights */
	for (i = 0; i < *N; i++) {
		if (neighbors[i]) {
			weights[i] = (1 - dist[i]/a)/a;
		} else {
			weights[i] = 0;
		}
	}
}


/***********************************************************************************************/
/* cauchy 
 * variant 4: adaptive bandwidth, all observations 
 */
void cauchy4 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	for (i = 0; i < *N; i++) {
		weights[i] = 1/(M_PI * (1 + pow(dist[i]/a, 2)) * a);
	}
	
}


/* exponential 
 * variant 4: adaptive bandwidth, all observations 
 */
void exponential4 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	for (i = 0; i < *N; i++) {
		weights[i] = 0.5 * exp(-dist[i]/a)/a;
	}
}


/* gaussian 
 * variant 4: adaptive bandwidth, all observations 
 */
void gaussian4 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		if (dist[i] < 0) {
			error("'dist' must be positive");
		}
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = distcopy[*k-1]/(1 - DOUBLE_EPS/2);
	for (i = 0; i < *N; i++) {
		weights[i] = dnorm(dist[i], 0, a, 0);
	}
}
