#  Copyright (C) 2012 J. Schiffner
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

#' Fit a neural network repeatedly for different starting values.
#'
#' @title Fit a Neural Network Repeatedly for Different Starting Values
#'
#' @param reps Number of repetitions. Defaults to 1. \code{reps} larger one does not make sense if \code{Wts} is specified.
#' @param \dots Further arguments to \code{\link[nnet]{nnet}}.
#'
#' @return An object of class \code{"nnet"}.
#'
#' @seealso \code{\link[nnet]{nnet}}.
#'
#' @examples
#' fit <- nnetRep(5, Species ~ ., data = iris, size = 2, decay = 0.2, trace = FALSE)
#' predict(fit)
#'
#' @export

nnetRep <- function(reps = 1, ...) {
	fit <- lapply(1:reps, function(z) nnet(...))
	m <- which.min(sapply(fit, function(x) x$value))
	return(fit[[m]])
}
