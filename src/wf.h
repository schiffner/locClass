/*  
 *	Copyright (C) 2011-2013 J. Schiffner
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


#ifndef WF_H

#define WF_H

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Lapack.h>
#include <R_ext/BLAS.h>
#include <math.h>


/* sample */

void sample (int *, int *, int *, int *);


/* weight functions */

void biweight1 (double *, double *, int *, double *, int *);
void cauchy1 (double *, double *, int *, double *, int *);
void cosine1 (double *, double *, int *, double *, int *);
void epanechnikov1 (double *, double *, int *, double *, int *);
void exponential1 (double *, double *, int *, double *, int *);
void gaussian1 (double *, double *, int *, double *, int *);
void optcosine1 (double *, double *, int *, double *, int *);
void rectangular1 (double *, double *, int *, double *, int *);
void triangular1 (double *, double *, int *, double *, int *);

void biweight2 (double *, double *, int *, double *, int *);
void cauchy2 (double *, double *, int *, double *, int *);
void cosine2 (double *, double *, int *, double *, int *);
void epanechnikov2 (double *, double *, int *, double *, int *);
void exponential2 (double *, double *, int *, double *, int *);
void gaussian2 (double *, double *, int *, double *, int *);
void optcosine2 (double *, double *, int *, double *, int *);
void rectangular2 (double *, double *, int *, double *, int *);
void triangular2 (double *, double *, int *, double *, int *);

void biweight3 (double *, double *, int *, double *, int *);
void cauchy3 (double *, double *, int *, double *, int *);
void cosine3 (double *, double *, int *, double *, int *);
void epanechnikov3 (double *, double *, int *, double *, int *);
void exponential3 (double *, double *, int *, double *, int *);
void gaussian3 (double *, double *, int *, double *, int *);
void optcosine3 (double *, double *, int *, double *, int *);
void rectangular3 (double *, double *, int *, double *, int *);
void triangular3 (double *, double *, int *, double *, int *);

void cauchy4 (double *, double *, int *, double *, int *);
void exponential4 (double *, double *, int *, double *, int *);
void gaussian4 (double *, double *, int *, double *, int *);

#endif /* WF_H */
