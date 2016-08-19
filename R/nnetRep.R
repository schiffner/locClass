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
#' @family nnet
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
