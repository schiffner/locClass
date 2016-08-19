/*  copyright (C) 2011 J. Schiffner
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


# include "wf.h"

/* predkda:
 *
 *  s_test:		test data, N_test * p matrix
 *  s_learn:	training data, N_learn * p matrix
 *  s_grouping:	class labels, factor of length N_learn
 *  s_wf:		weight function, either R function or integer mapping to the name of the function
 *  s_bw:		bandwidth parameter of window function
 *  s_k:		number of nearest neighbors
 *  s_env:		environment for evaluation of R functions
 *
 */


SEXP predkda(SEXP s_test, SEXP s_learn, SEXP s_grouping, SEXP s_wf, SEXP s_bw, SEXP s_k, SEXP s_env)
{
	const R_len_t p = ncols(s_test);			// dimensionality
	R_len_t N_learn = nrows(s_learn);		// # training observations
	const R_len_t N_test = nrows(s_test);		// # test observations
	const R_len_t K = nlevels(s_grouping);		// # classes
	double *test = REAL(s_test);				// pointer to test data set
	double *learn = REAL(s_learn);				// pointer to training data set
	int *g = INTEGER(s_grouping);				// pointer to class labels
	int *k = INTEGER(s_k);						// pointer to number of nearest neighbors
	double *bw = REAL(s_bw);					// bandwidth
	/*Rprintf("k %u\n", *k);
	Rprintf("bw %f\n", *bw);
	 */
	
	
	SEXP s_posterior;							// initialize posteriors
	PROTECT(s_posterior = allocMatrix(REALSXP, N_test, K));
	double *posterior = REAL(s_posterior);
	
	SEXP s_dist;								// initialize distances to test observation
	PROTECT(s_dist = allocVector(REALSXP, N_learn));
	double *dist = REAL(s_dist);
	
	SEXP s_weights;								// initialize weight vector
	PROTECT(s_weights = allocVector(REALSXP, N_learn));
	double *weights = REAL(s_weights);
	
	int nas = 0;
	
	int i, j, l, n;								// indices
		
	// select weight function
	typedef void (*wf_ptr_t) (double*, double*, int*, double*, int*);// *weights, *dist, *N, *bw, *k
	wf_ptr_t wf = NULL;
	if (isInteger(s_wf)) {
		const int wf_nr = INTEGER(s_wf)[0];
		//Rprintf("wf_nr %u\n", wf_nr);
		wf_ptr_t wfs[] = {biweight1, cauchy1, cosine1, epanechnikov1, exponential1, gaussian1,
			optcosine1, rectangular1, triangular1, biweight2, cauchy2, cosine2, epanechnikov2,
			exponential2, gaussian2, optcosine2, rectangular2, triangular2, biweight3, cauchy3,
			cosine3, epanechnikov3, exponential3, gaussian3, optcosine3, rectangular3, 
			triangular3, cauchy4, exponential4, gaussian4};
		wf = wfs[wf_nr - 1];
	}
	
	// loop over all test observations
	for(n = 0; n < N_test; n++) {
			
		// 0. check for NAs in test
		nas = 0;
		for (j = 0; j < p; j++) {
			nas += ISNA(test[n + N_test * j]);
		}
		if (nas > 0) { // NAs in n-th test observation
			warning("NAs in test observation %u", n+1);
			// set posterior to NA
			for (l = 0; l < K; l++) {
				posterior[n + N_test * l] = NA_REAL;
			}			
		} else {
			// 1. calculate distances to n-th test observation
			for (i = 0; i < N_learn; i++) {
				dist[i] = 0;
				for (j = 0; j < p; j++) {
					dist[i] += pow(learn[i + N_learn * j] - test[n + N_test * j], 2);
				}
				dist[i] = sqrt(dist[i]);
				weights[i] = 0;				// important because some weights are 0
				//Rprintf("dist %f\n", dist[i]);
			}
				
			// 2. calculate observation weights
			if (isInteger(s_wf)) {
				// case 1: wf is integer
				// calculate weights by reading number and calling corresponding C function
				wf(weights, dist, &N_learn, bw, k);
			} else if (isFunction(s_wf)) {
				// case 2: wf is R function
				// calculate weights by calling R function
				SEXP R_fcall;
				PROTECT(R_fcall = lang2(s_wf, R_NilValue)); //R_NilValue = NULL??? NILSXP = NULL
				SETCADR(R_fcall, s_dist); // SETCADR: cadr list = (car (cdr list))
				weights = REAL(eval(R_fcall, s_env));
				UNPROTECT(1);	// R_fcall
			}
			/*for(i = 0; i < N_learn; i++) {
				Rprintf("weights %f\n", weights[i]);
			}*/
				
			// 3. calculate posterior probabilities as class wise sum of weights
			for (l = 0; l < K; l++) {
				posterior[n + N_test * l] = 0;
				for (i = 0; i < N_learn; i++) {
					if (g[i] == l + 1) {
						posterior[n + N_test * l] += weights[i];
					}
				}
			}
		}
			
	}
	// end loop over test observations
		
	// 4. set dimnames of s_posterior
	SEXP dimnames;
	PROTECT(dimnames = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(getAttrib(s_test, R_DimNamesSymbol), 0));
	SET_VECTOR_ELT(dimnames, 1, getAttrib(s_grouping, R_LevelsSymbol));
	setAttrib(s_posterior, R_DimNamesSymbol, dimnames);
	
	//void R_max_col (double* matrix, int* nr, int* nc, int* maxes)
	// maxes initialisieren
	//R_max_col (posterior, &N_test, &K, int* maxes)
	
	UNPROTECT(4);	// dimnames, s_dist, s_weights, s_posterior
	return(s_posterior);
	
}
