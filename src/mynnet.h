/*  nnet/src/nnet.c by W. N. Venables and B. D. Ripley  Copyright (C) 1992-2002
 *
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
 */

/* weights are stored in order of their destination unit.
 * the array Conn gives the source unit for the weight (0 = bias unit)
 * the array Nconn gives the number of first weight connecting to each unit,
 * so the weights connecting to unit i are Nconn[i] ... Nconn[i+1] - 1.
 *
 */

#include <R.h>
#include <R_ext/Applic.h>
#include <math.h>

typedef double Sdata;

static double *vect(int);
static void free_vect(double *);
static double **matrix(int, int);
static void free_matrix(double**, int, int);
static void fpass(Sdata*, Sdata*, Sdata, int);
static void Build_Net(int, int, int);
#define EPS 1.0E-80
#define REPORT		10
#define max9(a,b) a>b?a:b
#define min9(a,b) a<b?a:b





/*

void
VR_set_net(Sint*, Sint*, Sint*,
		   double*, Sint*, Sint*,
		   Sint*, Sint*);

void 
VR_unset_net();

void
VR_nntest(Sint *, Sdata *, Sdata *, double *, int);


static double
sigmoid(double);


static double
E(double, double);


static void
fpass(Sdata *, Sdata *, Sdata, int);

static double
sigmoid_prime(double);

static double
sigmoid_prime_prime(double);

static void
bpass(Sdata *, Sdata);

void
VR_dfunc(double *, double *, double *);

static double
fminfn(int, double *, void *);

static void
fmingr(int, double *, double *, void *);

static double *
vect(int);

static double **
matrix(int, int);

static void 
free_matrix(double **, int, int);

static double **
Lmatrix(int);

static void 
free_Lmatrix(double **, int);


void
VR_dovm(Sint *, Sdata *, Sdata *,
		Sint *, double *, double *,
		Sint *, Sint *, Sint *,
		double *, double *, int *);

static void
pHessian(Sdata *, Sdata *, Sdata, int);

void
VR_nnHessian(Sint *, Sdata *, Sdata *,
			 double *, Sdata *);

static int 
Zcompar(const Sdata *, const Sdata *);

void
VR_summ2(Sint *, Sint *, Sint *, Sdata *, Sint *);*/

//#include "R_ext/Rdynload.h"

/*static const R_CMethodDef CEntries[] = {
    {"VR_dfunc", (DL_FUNC) &VR_dfunc, 3},
    {"VR_dovm", (DL_FUNC) &VR_dovm, 12},
    {"VR_nnHessian", (DL_FUNC) &VR_nnHessian, 5},
    {"VR_nntest", (DL_FUNC) &VR_nntest, 4},
    {"VR_set_net", (DL_FUNC) &VR_set_net, 8},
    {"VR_summ2", (DL_FUNC) &VR_summ2, 5},
    {"VR_unset_net", (DL_FUNC) &VR_unset_net, 0},
    {NULL, NULL, 0}
};

void R_init_nnet(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
}*/

