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


#ifndef SPARSE_H
#define SPARSE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "svm.h"
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))

struct svm_node ** sparsify (double*, int, int);

struct svm_node ** transsparse (double*, int, int*, int*);

#endif /* SPARSE_H */