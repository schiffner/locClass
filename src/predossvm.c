/*
 *  Copyright (C) 2011 Julia Schiffner
 *  Copyright (C) Evgenia Dimitriadou, Kurt Hornik, Friedrich Leisch, 
 *   David Meyer, and Andreas Weingessel
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


#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "svm.h"
#include "sparse.h"
#include "wf.h"
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))


/* predossvm
 *
 * training data
 * s_y:				y, class labels
 *
 * s_tr:			training data
 * s_trr:			nrow(tr)
 * s_trc:			ncol(tr)
 * s_trrowindex:	sparse index info
 * s_trcolindex:	sparse index info
 * s_sparsetr:		sparse training matrix
 *
 * weighting
 * s_wf:			weight function, either R function or integer mapping to the name of the function
 * s_bw:			bandwidth parameter of window function
 * s_k:				number of nearest neighbors
 *
 * parameters
 * s_svm_type:		svm type
 * s_kernel_type:	kernel type
 * s_degree:
 * s_gamma:
 * s_coef0:			
 * s_cache
 * s_tolerance
 * s_cost:
 * s_nu:				
 * s_weightlabels:	
 * s_weights:		class weights
 * s_nweights:		number of classes / class weights
 * s_epsilon
 * s_shrinking
 *
 * s_decisionvalues:	predict decision values?
 * s_probability:		predict probabilities?
 *
 * s_seed:
 *
 * s_nclasses:
 *
 * test data
 * s_te:			newdata
 * s_ter:			nrow(newdata)
 * s_terowindex:	sparse matrix info
 * s_tecolindex:	sparse matrix info
 * s_sparsete:		sparse newdata matrix
 *
 */


SEXP predossvm (SEXP s_y,
				 
				 SEXP s_tr, SEXP s_trr, SEXP s_trc,
				 SEXP s_trrowindex, SEXP s_trcolindex,
				 SEXP s_sparsetr,

				 SEXP   s_wf,
				 SEXP	s_bw,
				 SEXP   s_k,
				 SEXP   s_env,
				 
				 SEXP   s_svm_type,
				 SEXP   s_kernel_type,
				 SEXP   s_degree,
				 SEXP   s_gamma,
				 SEXP   s_coef0,
				 SEXP   s_cache,
				 SEXP   s_tolerance,
				 SEXP   s_cost,
				 SEXP   s_nu,
				 SEXP   s_weightlabels,
				 SEXP   s_weights,
				 SEXP   s_nweights,
				 SEXP   s_epsilon,
				 SEXP   s_shrinking,

				 SEXP   s_decisionvalues,
				 SEXP   s_probability,
				 
				 SEXP   s_seed,
				 
				 SEXP   s_nclasses,
				 
				 SEXP   s_te, SEXP s_ter,
				 SEXP   s_terowindex,
				 SEXP   s_tecolindex,
				 SEXP   s_sparsete)
{
	
	/* unpack some arguments */
	double *tr = REAL(s_tr);
	int trr = INTEGER(s_trr)[0];
	int trc  = INTEGER(s_trc)[0];
	double *te = REAL(s_te);
	int ter = INTEGER(s_ter)[0];
	int *nclasses = INTEGER(s_nclasses);
	int svm_type = INTEGER(s_svm_type)[0];
	
	/* initialization */
    struct svm_parameter par;
    struct svm_problem problem;
    struct svm_node	** train;
	
	SEXP s_dist;								// distances to test observation
	PROTECT(s_dist = allocVector(REALSXP, trr));
	double *dist = REAL(s_dist);

	SEXP s_caseweights;							// case weight vector
	PROTECT(s_caseweights = allocVector(REALSXP, trr));
	double *caseweights = REAL(s_caseweights);

	double sum_weights = 0.0;					// sum of case weights for normalization of weights
	
	SEXP s_res;									// list for results
	PROTECT(s_res = allocVector(VECSXP, 4)); 
	
	SEXP s_ret;									// predictions
	PROTECT(s_ret = allocVector(REALSXP, ter));
	double *ret = REAL(s_ret);
	
	SEXP s_dec;									// decision values
	PROTECT(s_dec = allocVector(REALSXP, ter * *nclasses * (*nclasses - 1) / 2));
	double *dec = REAL(s_dec);
	double dec_vector[*nclasses * (*nclasses - 1) / 2];
	
	SEXP s_prob;								// posterior probabilities
	PROTECT(s_prob = allocVector(REALSXP, ter * *nclasses));
	double *prob = REAL(s_prob);
	double prob_vector[*nclasses];
	
	int nr_class = 0;
	int labels[*nclasses];
	
    const char* s;
	int i, j, n, p;
	
	/* initialize decision values */
	for (j = 0; j < ter * *nclasses * (*nclasses - 1) / 2; j++) {
		dec[j] = NA_REAL;
	}

	/* initialize posteriors */
	for (j = 0; j < ter * *nclasses; j++) {
		prob[j] = 0.0;
	}
	
	/*for (i=0; i<10; i++) {
		Rprintf("prob init %f\n", prob[i]);		
		Rprintf("dec init %f\n", dec[i]);		
	}*/
	
	/* set up weight function */
	typedef void (*wf_ptr_t) (double*, double*, int*, double*, int*);
	// signature: *weights, *dist, N, *bw, k
	wf_ptr_t wf = NULL;
	if (isInteger(s_wf)) {
		const int wf_nr = INTEGER(s_wf)[0];
		wf_ptr_t wfs[] = {biweight1, cauchy1, cosine1, epanechnikov1, exponential1, gaussian1,
			optcosine1, rectangular1, triangular1, biweight2, cauchy2, cosine2, epanechnikov2,
			exponential2, gaussian2, optcosine2, rectangular2, triangular2, biweight3, cauchy3,
			cosine3, epanechnikov3, exponential3, gaussian3, optcosine3, rectangular3, 
			triangular3, cauchy4, exponential4, gaussian4};
		wf = wfs[wf_nr - 1];
	}

    /* set parameters */
    par.svm_type    = svm_type;
    par.kernel_type = INTEGER(s_kernel_type)[0];
    par.degree      = INTEGER(s_degree)[0];
    par.gamma       = REAL(s_gamma)[0];
    par.coef0       = REAL(s_coef0)[0];
    par.cache_size  = REAL(s_cache)[0];
    par.eps         = REAL(s_tolerance)[0];
    par.C           = REAL(s_cost)[0];
    par.nu          = REAL(s_nu)[0];
    par.nr_weight   = INTEGER(s_nweights)[0];
    if (par.nr_weight > 0) {
		par.weight      = (double *) malloc (sizeof(double) * par.nr_weight);
		memcpy(par.weight, REAL(s_weights), par.nr_weight * sizeof(double));
		par.weight_label = (int *) malloc (sizeof(int) * par.nr_weight);
		memcpy(par.weight_label, INTEGER(s_weightlabels), par.nr_weight * sizeof(int));
    }
    par.p           = REAL(s_epsilon)[0];
    par.shrinking   = INTEGER(s_shrinking)[0];
    par.probability = INTEGER(s_probability)[0];

    /* set problem */
    problem.l = trr;
    problem.y = REAL(s_y);

    /* create sparse training matrix */
    if (INTEGER(s_sparsetr)[0] > 0)
		problem.x = transsparse(tr, trr, INTEGER(s_trrowindex), INTEGER(s_trcolindex));
    else
		problem.x = sparsify(tr, trr, trc);
	
    /* create sparse test matrix */
	if (INTEGER(s_sparsete)[0] > 0)
		train = transsparse(te, ter, INTEGER(s_terowindex), INTEGER(s_tecolindex));
	else
		train = sparsify(te, ter, trc);

	/* check parameters & copy error message */
	s = svm_check_parameter(&problem, &par);
	if (s) {
		//error(s);
		//strcpy(*error, s);
		SEXP s_error;
		PROTECT(s_error = mkChar(s));
		SET_VECTOR_ELT(s_res, 0, s_error);
		UNPROTECT(1); // s_error
	} else {
		
		/* set seed */
		//srand(INTEGER(s_seed)[0]);
		
		/* loop over all test observations */
		for (n = 0; n < ter; n++) {
			
			struct svm_model * model = NULL;
			
			/* initialization */
			sum_weights = 0.0;
			for (j = 0; j < *nclasses * (*nclasses - 1) / 2; j++) {
				dec_vector[j] = 0.0;
			}
			for (j = 0; j < *nclasses; j++) {
				prob_vector[j] = 0.0;
			}
			
			/*for (i=0; i<10; i++) {
				Rprintf("%f \n", problem.x[i][0].value);
				Rprintf("%f \n", tr[i * *trc + 0]); // stimmt überein
			}*/
			
			/* calculate distances */
			for (i = 0; i < trr; i++) {
				dist[i] = 0.0;
				for (j = 0; j < trc; j++) {
					dist[i] += pow(problem.x[i][j].value - train[n][j].value, 2);
				}
				dist[i] = sqrt(dist[i]);
				caseweights[i] = 0;
				//Rprintf("dist %f\n", dist[i]);
			}
			
			/* calculate weights */
			if (isInteger(s_wf)) {
				// case 1: wf is integer
				// calculate weights by reading number and calling corresponding C function
				wf (caseweights, dist, &trr, REAL(s_bw), INTEGER(s_k));
			} else if (isFunction(s_wf)) {
				// case 2: wf is R function
				// calculate case weights by calling R function
				SEXP R_fcall;
				PROTECT(R_fcall = lang2(s_wf, R_NilValue));
				SETCADR(R_fcall, s_dist);
				caseweights = REAL(eval(R_fcall, s_env));
				UNPROTECT(1); // R_fcall
			}
			//typedef void (*wf_ptr_t) (double*, double*, int, double*, int);// *weights, *dist, N, *bw, k
			
			/* rescale weights such that they sum up to trr */
			for(i = 0; i < trr; i++) {
				sum_weights += caseweights[i];
				//Rprintf("caseweights %f\n", caseweights[i]);
			}
			//Rprintf("sum_weights %f\n", sum_weights);
			for(i = 0; i < trr; i++) {
				caseweights[i] = (double)trr / sum_weights * caseweights[i];
				//Rprintf("caseweights2 %f\n", caseweights[i]);
				//Rprintf("caseweights3 %u\n", caseweights[i] == 0.0);
			}
			
			/* update problem */
			problem.W = caseweights;
			
			/* set seed */
			srand(INTEGER(s_seed)[0]);
			//Rprintf("seed %u\n", INTEGER(s_seed)[0]);
			//Rprintf ("random number: %u\n", rand());			
			
			/* call svm_train */
			model = svm_train(&problem, &par);
						
			nr_class = svm_get_nr_class(model);
			//Rprintf("nr_class %u\n", nr_class);
			svm_get_labels(model, labels);
			/*for (i = 0; i < nr_class; i++) {
				Rprintf("labels %u\n", labels[i]);
			}*/
			//Rprintf("model->l %u\n", model->l);
			/*for(i = 0;i < nr_class * (nr_class - 1) / 2; i++) {
				Rprintf("model->rho %f\n", model->rho[i]);
			}*/
			/*for(i = 0;i < nr_class; i++) {
				Rprintf("model->nSV %u\n", model->nSV[i]);
			}*/
			
			/* call svm_predict */
			if (INTEGER(s_probability) && svm_check_probability_model(model)) {
				/*for(i = 0;i < nr_class * (nr_class - 1) / 2; i++) {
					Rprintf("model->probA %f\n", model->probA[i]);
					Rprintf("model->probB %f\n", model->probB[i]);
				}*/
				//Rprintf("prob calculated, test obs = %u\n", n+1);
				//if (nr_class < *nclasses) {
					ret[n] = svm_predict_probability(model, train[n], prob_vector);
					for (j = 0; j < nr_class; j++) {
						//Rprintf("index prob %u\n", n * *nclasses + labels[j] - 1);
						prob[n * *nclasses + labels[j] - 1] = prob_vector[j];
					}
				//} else {
				//	ret[n] = svm_predict_probability(model, train[n], prob + n * *nclasses);
				//}
			} else {
				ret[n] = svm_predict(model, train[n]);
			}			

			/*Rprintf("ret %f\n", ret[n]);
			for (i = 0; i < *nclasses; i++) {
				Rprintf("prob %f\n", prob[n * *nclasses + i]);
			}*/
			
			/* optionally, compute decision values */
			if (INTEGER(s_decisionvalues)[0]) {
				/* test: dec_vector stays zero of nr_class == 1
				if (nr_class == 1) {
					svm_predict_values(model, train[n], dec_vector);
					Rprintf("dec_vector[1] %f\n", dec_vector[1]);
					Rprintf("dec_vector[2] %f\n", dec_vector[2]);
					Rprintf("dec_vector[3] %f\n", dec_vector[3]);					
				}*/
				//if (nr_class > 1 && nr_class < *nclasses) {
					p = 0;
					svm_predict_values(model, train[n], dec_vector);
					for (i = 0; i < nr_class; i++) {
						for (j = i + 1; j < nr_class; j++) {
							if (labels[i] < labels[j]) {
								//Rprintf("index dec %u\n", n * *nclasses * (*nclasses - 1) / 2 + (labels[i]-1) * *nclasses - labels[i]*(labels[i]-1)/2 + labels[j] - labels[i] - 1);
								dec[n * *nclasses * (*nclasses - 1) / 2 + (labels[i]-1) * *nclasses - labels[i]*(labels[i]-1)/2 + labels[j] - labels[i] - 1] = dec_vector[p];
							} else {
								//Rprintf("index dec %u\n", n * *nclasses * (*nclasses - 1) / 2 + (labels[j]-1) * *nclasses - labels[j]*(labels[j]-1)/2 + labels[i] - labels[j] - 1);
								dec[n * *nclasses * (*nclasses - 1) / 2 + (labels[j]-1) * *nclasses - labels[j]*(labels[j]-1)/2 + labels[i] - labels[j] - 1] = -dec_vector[p];							
							}
							p++;
						}
					}
				//} else {
				//	svm_predict_values(model, train[n], dec + n * *nclasses * (*nclasses - 1) / 2);
				//}
			}

			
			/*for (i = 0; i < *nclasses; i++) {
				Rprintf("dec %f\n", dec[i + n * *nclasses * (*nclasses - 1) / 2]);
			}*/
			/* clean up memory */
			svm_free_and_destroy_model(&model);

		}
		
		SET_VECTOR_ELT(s_res, 1, s_ret);
		SET_VECTOR_ELT(s_res, 2, s_prob);
		SET_VECTOR_ELT(s_res, 3, s_dec);
		
		//SET_VECTOR_ELT(s_res, 4, s_dist);
		//SET_VECTOR_ELT(s_res, 5, s_caseweights);
		
	}

	/* clean up memory */
	if (par.nr_weight > 0) {
		free(par.weight);
		free(par.weight_label);
	}

	for (i = 0; i < trr; i++) 
		free (problem.x[i]);
	free (problem.x);
	
	for (i = 0; i < ter; i++)
		free (train[i]);
	free (train);
	
	UNPROTECT(6); //s_dist, s_caseweights, s_dec, s_ret, s_prob, s_res
	
	return(s_res);
}
