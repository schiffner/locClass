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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "svm.h"
#include "sparse.h"
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))

/*
 * results from cross-validation
 */

struct crossresults
{
    double* results;
    double  total1;
    double  total2;
};


/* Cross-Validation-routine from svm-train */
void do_cross_validation(struct svm_problem *prob,
			 struct svm_parameter *param,
			 int nr_fold,
			 double* cresults,
			 double* ctotal1,
			 double* ctotal2)
{
	int i;
	int total_correct = 0;
	double total_error = 0;
	double sumv = 0, sumy = 0, sumvv = 0, sumyy = 0, sumvy = 0;

	/* random shuffle */
	for(i=0; i<prob->l; i++)
	{
		int j = i+rand()%(prob->l-i);
		struct svm_node *tx;
		double ty;
		double tW;
			
		tx = prob->x[i];
		prob->x[i] = prob->x[j];
		prob->x[j] = tx;

		ty = prob->y[i];
		prob->y[i] = prob->y[j];
		prob->y[j] = ty;
		
		tW = prob->W[i];
		prob->W[i] = prob->W[j];
		prob->W[j] = tW;
		
	}

	for(i=0; i<nr_fold; i++)
	{
		int begin = i*prob->l/nr_fold;
		int end = (i+1)*prob->l/nr_fold;
		int j,k;
		struct svm_problem subprob;

		subprob.l = prob->l-(end-begin);
		subprob.x = Malloc(struct svm_node*,subprob.l);
		subprob.y = Malloc(double,subprob.l);
		subprob.W = Malloc(double,subprob.l);
					
		k=0;
		for(j = 0; j < begin; j++)
		{
			subprob.x[k] = prob->x[j];
			subprob.y[k] = prob->y[j];
			subprob.W[k] = prob->W[j];
			++k;
		}
		for(j = end; j<prob->l; j++)
		{
			subprob.x[k] = prob->x[j];
			subprob.y[k] = prob->y[j];
			subprob.W[k] = prob->W[j];
			++k;
		}

		if(param->svm_type == EPSILON_SVR ||
		   param->svm_type == NU_SVR)
		{
			struct svm_model *submodel = svm_train(&subprob,param);
			double error = 0;
			for(j=begin;j<end;j++)
			{
				double v = svm_predict(submodel,prob->x[j]);
				double y = prob->y[j];
				error += (v-y)*(v-y);
				sumv += v;
				sumy += y;
				sumvv += v*v;
				sumyy += y*y;
				sumvy += v*y;
			}
			svm_free_and_destroy_model(&submodel);
			/* printf("Mean squared error = %g\n",
			   error/(end-begin)); */
			cresults[i] = error/(end-begin);
			total_error += error;			
		}
		else
		{
			struct svm_model *submodel = svm_train(&subprob,param);
			int correct = 0;
			for(j=begin;j<end;j++)
			{
				double v = svm_predict(submodel,prob->x[j]);
				if(v == prob->y[j])
					++correct;
			}
			svm_free_and_destroy_model(&submodel);
			/* printf("Accuracy = %g%% (%d/%d)\n", */
			/* 100.0*correct/(end-begin),correct,(end-begin)); */
			cresults[i] = 100.0*correct/(end-begin);
			total_correct += correct;
		}

		free(subprob.x);
		free(subprob.y);
		free(subprob.W);
	}
	
	if(param->svm_type == EPSILON_SVR || param->svm_type == NU_SVR)
	{
	    /* printf("Cross Validation Mean squared error = %g\n",total_error/prob.l);
	        printf("Cross Validation Squared correlation coefficient = %g\n",
	    	((prob.l*sumvy-sumv*sumy)*(prob.l*sumvy-sumv*sumy))/
	    	((prob.l*sumvv-sumv*sumv)*(prob.l*sumyy-sumy*sumy))
	    	); */
	    *ctotal1 = total_error/prob->l;
	    *ctotal2 = ((prob->l * sumvy - sumv * sumy) *
			(prob->l * sumvy - sumv*sumy))  /
		       ((prob->l * sumvv - sumv * sumv) *
		        (prob->l * sumyy - sumy * sumy));
	}
	else
	    /* printf("Cross Validation Accuracy =
	       %g%%\n",100.0*total_correct/prob.l); */
	    *ctotal1 = 100.0 * total_correct / prob->l;
}


void svmtrain (double *x, int *r, int *c, 
	       double *y,
	       int    *rowindex, int *colindex,
	       int    *svm_type,
	       int    *kernel_type,
	       int    *degree,
	       double *gamma,
	       double *coef0,
	       double *cost,
	       double *nu,
	       int    *weightlabels,
	       double *weights,
	       int    *nweights,
		   double *W,
	       double *cache,
	       double *tolerance,
	       double *epsilon,
	       int    *shrinking,
	       int    *cross,
	       int    *sparse,
	       int    *probability,
	       int    *seed,
	       
	       int    *nclasses,
	       int    *nr,
	       int    *index,
	       int    *labels,
	       int    *nSV,
	       double *rho,
		   double *obj,
	       double *coefs,
	       double *sigma,
	       double *probA,
	       double *probB,

	       double *cresults,
	       double *ctotal1,
	       double *ctotal2,
	       char   **error)
{
    struct svm_parameter par;
    struct svm_problem   prob;
    struct svm_model    *model = NULL;
    int i, ii;
    const char* s;
    
    /* set parameters */
    par.svm_type    = *svm_type;
    par.kernel_type = *kernel_type;
    par.degree      = *degree;
    par.gamma       = *gamma;
    par.coef0       = *coef0;
    par.cache_size  = *cache;
    par.eps         = *tolerance;
    par.C           = *cost;
    par.nu          = *nu;
    par.nr_weight   = *nweights;
    if (par.nr_weight > 0) {
	par.weight      = (double *) malloc (sizeof(double) * par.nr_weight);
	memcpy(par.weight, weights, par.nr_weight * sizeof(double));
	par.weight_label = (int *) malloc (sizeof(int) * par.nr_weight);
	memcpy(par.weight_label, weightlabels, par.nr_weight * sizeof(int));
    }
    par.p           = *epsilon;
    par.shrinking   = *shrinking;
    par.probability = *probability;

    /* set problem */
    prob.l = *r;
    prob.y = y;
	prob.W = W;
    
    if (*sparse > 0)
	prob.x = transsparse(x, *r, rowindex, colindex);
    else
	prob.x = sparsify(x, *r, *c);
    
    /* check parameters & copy error message */
    s = svm_check_parameter(&prob, &par);
    if (s) {
	strcpy(*error, s);
    } else {
	/* set seed */
	srand(*seed);

	/* call svm_train */
	model = svm_train(&prob, &par);
    
	/* set up return values */
	for (ii = 0; ii < model->l; ii++)
	    for (i = 0; i < *r;	i++)
		if (prob.x[i] == model->SV[ii]) index[ii] = i+1;
	
	*nr  = model->l;
	*nclasses = model->nr_class;
	memcpy (rho, model->rho, *nclasses * (*nclasses - 1)/2 * sizeof(double));
	memcpy (obj, model->obj, *nclasses * (*nclasses - 1)/2 * sizeof(double));

	if (*probability && par.svm_type != ONE_CLASS) {
	  if (par.svm_type == EPSILON_SVR || par.svm_type == NU_SVR)
	    *sigma = svm_get_svr_probability(model);
	  else {
	    memcpy(probA, model->probA, 
		    *nclasses * (*nclasses - 1)/2 * sizeof(double));
	    memcpy(probB, model->probB, 
		    *nclasses * (*nclasses - 1)/2 * sizeof(double));
	  }
	}

	for (i = 0; i < *nclasses-1; i++)
	    memcpy (coefs + i * *nr, model->sv_coef[i],  *nr * sizeof (double));
	
	if (*svm_type < 2) {
	    memcpy (labels, model->label, *nclasses * sizeof(int));
	    memcpy (nSV, model->nSV, *nclasses * sizeof(int));
	}
	
	/* Perform cross-validation, if requested */
	if (*cross > 0)
	    do_cross_validation (&prob, &par, *cross, cresults,
				 ctotal1, ctotal2);

	/* clean up memory */
	svm_free_and_destroy_model(&model);
    }
    
    /* clean up memory */
    if (par.nr_weight > 0) {
	free(par.weight);
	free(par.weight_label);
    }
    
    for (i = 0; i < *r; i++) free (prob.x[i]);
    free (prob.x);
}
	     
void svmpredict  (int    *decisionvalues,
		  int    *probability,

		  double *v, int *r, int *c,
		  int    *rowindex,
		  int    *colindex,
		  double *coefs,
		  double *rho,
          double *obj,
		  int    *compprob,
		  double *probA,
		  double *probB,
		  int    *nclasses,
		  int    *totnSV,
		  int    *labels,
		  int    *nSV,
		  int    *sparsemodel,

		  int    *svm_type,
		  int    *kernel_type,
		  int    *degree,
		  double *gamma,
		  double *coef0,

		  double *x, int *xr,
		  int    *xrowindex,
		  int    *xcolindex,
		  int    *sparsex,
		  
		  double *ret,
		  double *dec,
		  double *prob)
{
    struct svm_model m;
    struct svm_node ** train;
    int i;
    
    /* set up model */
    m.l        = *totnSV;
    m.nr_class = *nclasses;
    m.sv_coef  = (double **) malloc (m.nr_class * sizeof(double));
    for (i = 0; i < m.nr_class - 1; i++) {
      m.sv_coef[i] = (double *) malloc (m.l * sizeof (double));
      memcpy (m.sv_coef[i], coefs + i*m.l, m.l * sizeof (double));
    }
    
    if (*sparsemodel > 0)
	m.SV   = transsparse(v, *r, rowindex, colindex);
    else
	m.SV   = sparsify(v, *r, *c);
    
    m.rho      = rho;
    m.obj      = obj;
    m.probA    = probA;
    m.probB    = probB;
    m.label    = labels;
    m.nSV      = nSV;

    /* set up parameter */
    m.param.svm_type    = *svm_type;
    m.param.kernel_type = *kernel_type;
    m.param.degree      = *degree;
    m.param.gamma       = *gamma;
    m.param.coef0       = *coef0;
    m.param.probability = *compprob;      

    m.free_sv           = 1;

    /* create sparse training matrix */
    if (*sparsex > 0)
	train = transsparse(x, *xr, xrowindex, xcolindex);
    else
	train = sparsify(x, *xr, *c);

    /* call svm-predict-function for each x-row, possibly using probability 
       estimator, if requested */
    if (*probability && svm_check_probability_model(&m)) {
      for (i = 0; i < *xr; i++)
	ret[i] = svm_predict_probability(&m, train[i], prob + i * *nclasses);
    } else {
      for (i = 0; i < *xr; i++)
	ret[i] = svm_predict(&m, train[i]);
    }

    /* optionally, compute decision values */
    if (*decisionvalues)
      for (i = 0; i < *xr; i++)
	svm_predict_values(&m, train[i], dec + i * *nclasses * (*nclasses - 1) / 2);

    /* clean up memory */
    for (i = 0; i < *xr; i++)
	free (train[i]);
    free (train);

    for (i = 0; i < *r; i++)
	free (m.SV[i]);
    free (m.SV);
    
    for (i = 0; i < m.nr_class - 1; i++)
      free(m.sv_coef[i]);
    free(m.sv_coef);
}	     
		
void svmwrite (double *v, int *r, int *c,
		  int    *rowindex,
		  int    *colindex,
		  double *coefs,
		  double *rho,
		  double *obj,
		  double *probA,
	      double *probB,
		  int    *nclasses,
		  int    *totnSV,
		  int    *labels,
		  int    *nSV,
		  int    *sparsemodel,

		  int    *svm_type,
		  int    *kernel_type,
		  int    *degree,
		  double *gamma,
		  double *coef0,

		  char **filename) 

{
    struct svm_model m;
    int i;
	char *fname = *filename;    

    /* set up model */
    m.l        = *totnSV;
    m.nr_class = *nclasses;
    m.sv_coef  = (double **) malloc (m.nr_class * sizeof(double));
    for (i = 0; i < m.nr_class - 1; i++) {
      m.sv_coef[i] = (double *) malloc (m.l * sizeof (double));
      memcpy (m.sv_coef[i], coefs + i*m.l, m.l * sizeof (double));
    }
    
    if (*sparsemodel > 0)
	m.SV   = transsparse(v, *r, rowindex, colindex);
    else
	m.SV   = sparsify(v, *r, *c);
    
    m.rho      = rho;
    m.obj      = obj;
    m.label    = labels;
    m.nSV      = nSV;
    m.probA    = probA;
    m.probB    = probB;

    /* set up parameter */
    m.param.svm_type    = *svm_type;
    m.param.kernel_type = *kernel_type;
    m.param.degree      = *degree;
    m.param.gamma       = *gamma;
    m.param.coef0       = *coef0;

    m.free_sv           = 1;

	/* write svm model */
	svm_save_model(fname, &m);

    for (i = 0; i < m.nr_class - 1; i++)
      free(m.sv_coef[i]);
    free(m.sv_coef);


}




/*
// x enthält margin points
// dec_values hat länge 
double svm_predict_values2(const svm_model *model, const svm_node *x, double* dec_values)
{
	if(model->param.svm_type == ONE_CLASS ||
	   model->param.svm_type == EPSILON_SVR ||
	   model->param.svm_type == NU_SVR)
	{
		double *sv_coef = model->sv_coef[0];
		double sum = 0;
		for(int i=0;i<model->l;i++)
			sum += sv_coef[i] * Kernel::k_function(x,model->SV[i],model->param);
		sum -= model->rho[0];
		*dec_values = sum;
		
		if(model->param.svm_type == ONE_CLASS)
			return (sum>0)?1:-1;
		else
			return sum;
	}
	else
	{
		int i;
		int nr_class = model->nr_class;
		int l = model->l;
		
		double *kvalue = Malloc(double,l);
		for(i=0;i<l;i++)
			kvalue[i] = Kernel::k_function(x,model->SV[i],model->param);
		
		int *start = Malloc(int,nr_class);
		start[0] = 0;
		for(i=1;i<nr_class;i++)
			start[i] = start[i-1]+model->nSV[i-1];
		
		int *vote = Malloc(int,nr_class);
		for(i=0;i<nr_class;i++)
			vote[i] = 0;
		
		int p=0;
		for(i=0;i<nr_class;i++)
			for(int j=i+1;j<nr_class;j++)
			{
				double sum = 0;
				int si = start[i];
				int sj = start[j];
				int ci = model->nSV[i];
				int cj = model->nSV[j];
				
				int k;
				double *coef1 = model->sv_coef[j-1];
				double *coef2 = model->sv_coef[i];
				for(k=0;k<ci;k++)
					sum += coef1[si+k] * kvalue[si+k];
				for(k=0;k<cj;k++)
					sum += coef2[sj+k] * kvalue[sj+k];
				sum -= model->rho[p];
				dec_values[p] = sum;
				
				if(dec_values[p] > 0)
					++vote[i];
				else
					++vote[j];
				p++;
			}
		
		int vote_max_idx = 0;
		for(i=1;i<nr_class;i++)
			if(vote[i] > vote[vote_max_idx])
				vote_max_idx = i;
		
		free(kvalue);
		free(start);
		free(vote);
		return model->label[vote_max_idx];
	}
}*/
