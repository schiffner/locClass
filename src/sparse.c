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


#include "sparse.h"
#include "svm.h"

struct svm_node ** sparsify (double *x, int r, int c)
{
    struct svm_node** sparse;
    int         i, ii, count;
    
    sparse = (struct svm_node **) malloc (r * sizeof(struct svm_node *));
    for (i = 0; i < r; i++) {
		/* determine nr. of non-zero elements */
		for (count = ii = 0; ii < c; ii++)
			if (x[i * c + ii] != 0) count++;
		
		/* allocate memory for column elements */
		sparse[i] = (struct svm_node *) malloc ((count + 1) * sizeof(struct svm_node));
		
		/* set column elements */
		for (count = ii = 0; ii < c; ii++)
			if (x[i * c + ii] != 0) {
				sparse[i][count].index = ii + 1;
				sparse[i][count].value = x[i * c + ii];
				count++;
			}
		
		/* set termination element */
		sparse[i][count].index = -1;
    }
	
    return sparse;
}


struct svm_node ** transsparse (double *x, int r, int *rowindex, int *colindex)
{
    struct svm_node** sparse;
    int i, ii, count = 0, nnz = 0;
	
    sparse = (struct svm_node **) malloc (r * sizeof(struct svm_node*));
    for (i = 0; i < r; i++) {
		/* allocate memory for column elements */
		nnz = rowindex[i+1] - rowindex[i];
		sparse[i] = (struct svm_node *) malloc ((nnz + 1) * sizeof(struct svm_node));
		
		/* set column elements */
		for (ii = 0; ii < nnz; ii++) {
			sparse[i][ii].index = colindex[count];
			sparse[i][ii].value = x[count];
			count++;
		}
		
		/* set termination element */
		sparse[i][ii].index = -1;
    }    
	
    return sparse;
    
}