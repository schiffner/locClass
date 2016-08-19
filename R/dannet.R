#' A local version of a single-hidden-layer neural network for classification that puts increased emphasis on a good model fit near the decision boundary.
#'
#' The idea of Hand and Vinciotti (2003) to put increased weight on observations near the decision boundary is generalized to the multiclass case and applied to 
#' neural networks.
#' Since the decision boundary is not known in advance an iterative procedure is required.
#' First, an unweighted neural network is fitted to the data. 
#' Based on the differences between the two largest estimated posterior probabilities observation weights are calculated.
#' Then a weighted neural network (see \code{\link[nnet]{nnet}}) from package \pkg{nnet} is fitted using these weights. 
#' Calculation of weights and model fitting is done several times in turn. 
#' The number of iterations is determined by the \code{itr}-argument that defaults to 3.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{dalda}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{dalda}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' If the predictor variables include factors, the formula interface must be used in order 
#' to get a correct model matrix.
#'
#' In contrast to \code{\link[nnet]{nnet}} this function is only appropriate for classification
#' problems. As response in \code{formula} only factors are allowed. If the
#' response is not a factor, it is coerced to a factor with a warning.
#' An appropriate classification network is constructed; this has one output and entropy fit if the
#' number of levels is two, and a number of outputs equal to the number
#' of classes and a softmax output stage for more levels. 
#' If you use the default method, you get only meaningful results if \code{y} is a 0-1 class indicator
#' matrix.
#' 
#' Optimization is done via the BFGS method of \code{\link{optim}}.
#'
#' @title Discriminant Adaptive Neural Network
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + \dots}, that is, the response
#' is the grouping \code{factor} and the right hand side specifies the (normally non-\code{factor})
#' discriminators.  
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} containing the explanatory variables.
#' @param y (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying
#' the class membership for each observation.
#' @param weights Initial observation weights (defaults to a vector of 1s).
#' @param wf A window function which is used to calculate weights that are introduced into 
#'   the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#'   For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param itr Number of iterations for model fitting, defaults to 3. See also the Details section.
#' @param reps Neural networks are fitted repeatedly (\code{reps} times) for different initial values and the solution with largest likelihood
#'  value is kept. Defaults to 1. (\code{reps} larger one does not make sense if \code{Wts} is specified.)
#' @param \dots Further arguments to \code{\link[nnet]{nnet}}.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if NAs are found. The default action is first
#'   the \code{na.action} setting of \code{\link{options}} and second \code{\link{na.fail}} if that is unset. 
#'   An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#'   variable. (NOTE: If given, this argument must be named.)
#' @param contrasts A list of contrasts to be used for some or all of the factors appearing as variables in the model formula.
#
# @param size Number of units in the hidden layer. Can be zero if there are skip-layer units.
# @param contrasts A list of contrasts to be used for some or all of the factors appearing as variables in the model formula.
# @param Wts Initial parameter vector. If missing chosen at random.
# @param mask Logical vector indicating which parameters should be optimized (default all).
# @param linout Switch for linear output units. Default logistic output units.
# @param entropy Switch for entropy (= maximum conditional likelihood) fitting. Default by least-squares.
# @param softmax Switch for softmax (log-linear model) and maximum conditional
#   likelihood fitting. \code{linout}, \code{entropy}, \code{softmax} and \code{censored} are mutually
#   exclusive.
# @param censored A variant on \code{softmax}, in which non-zero targets mean possible
#   classes. Thus for \code{softmax} a row of \code{(0, 1, 1)} means one example
#   each of classes 2 and 3, but for \code{censored} it means one example whose
#   class is only known to be 2 or 3.
# @param skip Switch to add skip-layer connections from input to output.
# @param rang Initial random weights on [-\code{rang}, \code{rang}].  Value about 0.5 unless the
#   inputs are large, in which case it should be chosen so that
#   \code{rang} * max(\code{|x|}) is about 1.
# @param decay Parameter for weight decay. Default 0.
# @param maxit Maximum number of iterations. Default 100.
# @param trace Switch for tracing optimization. Default \code{TRUE}.
# @param MaxNWts The maximum allowable number of weights.  There is no intrinsic limit
#   in the code, but increasing \code{MaxNWts} will probably allow fits that
#   are very slow and time-consuming.
# @param abstol Stop if the fit criterion falls below \code{abstol}, indicating an
#   essentially perfect fit.
# @param reltol  Stop if the optimizer is unable to reduce the fit criterion by a
#   factor of at least \code{1 - reltol}.
#'
#' @return
#' An object of class \code{"dannet"} or \code{"dannet.formula"} inheriting from \code{"nnet"}. A \code{list} mostly containing internal structure,
#' but with the following components: 
#'  \item{wts}{The best set of weights found.}
#'  \item{value}{Value of fitting criterion plus weight decay term.}
#'  \item{fitted.values}{The fitted values for the training data.}
#'  \item{residuals}{The residuals for the training data.}
#'  \item{convergence}{1 if the maximum number of iterations was reached, otherwise 0.}
#'  \item{weights}{A list of length \code{itr + 1}. The initial observation weights (a vector of 1s if none were given) and the observation
#'	  weights calculated in the individual iterations.}
#'  \item{itr}{The number of iterations used.}
#'  \item{wf}{The window function used. Always a function, even if the input was a string.}
#'  \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#'	\item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#'  \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	 specified.) \code{TRUE} if only the \code{k} nearest neighbors recieve a positive weight, \code{FALSE} otherwise.}
#'  \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the local density of data points, \code{FALSE} if the bandwidth
#'	  is fixed.}
#'  \item{call}{The (matched) function call.}
#'
#' @references Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}. Cambridge.
#'
#' Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
#'
#' @seealso \code{\link{predict.dannet}}, \code{\link[nnet]{nnet}} and 
#' \code{\link{dalr}} for discriminant adaptive logistic regression.
#'
#' @family nnet border_based
#'
#' @examples
#'  fit <- dannet(Species ~ Sepal.Length + Sepal.Width, data = iris, size = 2, rang = 0.1, maxit = 200, bw = 2)
#'  pred <- predict(fit)
#'  mean(pred$class != iris$Species)
#
#' @keywords classif multivariate
#'
#' @aliases dannet dannet.default dannet.formula dannet.matrix dannet.data.frame
#'
#' @export
#'
#' @import nnet

dannet <- function(x, ...)
	UseMethod("dannet")	



#' @rdname dannet
#' @export

dannet.formula <- function(formula, data, weights, ..., subset, na.action, contrasts = NULL) {
    class.ind <- function(cl) {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval.parent(m$data)))
        m$data <- as.data.frame(data)
    m$... <- m$contrasts <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    cons <- attr(x, "contrast")
    xint <- match("(Intercept)", colnames(x), nomatch=0L)
    if (xint > 0L) x <- x[, -xint, drop=FALSE] # Bias term is used for intercepts
    w <- model.weights(m)
    if (length(w) == 0L)
    	w <- rep(1, nrow(x))
    y <- model.response(m)
    if (!is.factor(y)) {
    	y <- as.factor(y)
    	warning("response was coerced to a factor")
    }
    lev <- lev1 <- levels(y)
    counts <- table(y)
    if (any(counts == 0L)) {
        empty <- lev[counts == 0L]
        warning(sprintf(ngettext(length(empty),
                                 "group %s is empty",
                                 "groups %s are empty"),
                        paste(empty, collapse=" ")), domain = NA)
        lev1 <- lev[counts > 0L]
        y <- factor(y, levels=lev1)
    }
    if (length(lev1) == 1L)
    	stop("training data from only one class given")
    if (length(lev) == 2L) {
        y <- as.vector(unclass(y)) - 1
        res <- dannet.default(x = x, y = y, weights = w, entropy = TRUE, ...)
    } else {
        y <- class.ind(y)
        res <- dannet.default(x = x, y = y, weights = w, softmax = TRUE, ...)
    }
    res$lev <- lev
    res$lev1 <- lev1
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$call <- match.call()
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("dannet.formula", "nnet.formula", "dannet", "nnet")
    res
}



#' @rdname dannet
#' @export

dannet.data.frame <- function (x, ...) {
    res <- dannet(structure(data.matrix(x, rownames.force = TRUE), class = "matrix"), ...)
    cl <- match.call()
    cl[[1L]] <- as.name("dannet")
    res$call <- cl
    res
}



#' @rdname dannet
#' @export

dannet.matrix <- function (x, y, weights = rep(1, nrow(x)), ..., subset, na.action = na.fail) {
    class.ind <- function(cl) {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    if (is.matrix(y))
    	stop("only factors are allowed as reponse")
	else { 
		if (!is.factor(y)) {
			y <- as.factor(y)
			warning("'y' was coerced to a factor")
		}	
    	if (!missing(subset)) {
        	weights <- weights[subset]
        	x <- x[subset, , drop = FALSE]
        	y <- y[subset]
    	}
    	if (missing(na.action)) {
        	if (!is.null(naa <- getOption("na.action")))    # if options(na.action = NULL) the default of na.action comes into play
            	if(!is.function(naa))
            		na.action <- get(naa, mode = "function")
            	else
                	na.action <- naa
    	} 
    	dfr <- na.action(structure(list(y = y, w = weights, x = x), 
        	    class = "data.frame", row.names = rownames(x)))
    	y <- dfr$y
    	x <- dfr$x
    	w <- dfr$w
		lev <- lev1 <- levels(y)
    	counts <- table(y)
   	 	if (any(counts == 0L)) {
       		empty <- lev[counts == 0L]
       		warning(sprintf(ngettext(length(empty),
                    "group %s is empty",
                    "groups %s are empty"),
        			paste(empty, collapse=" ")), domain = NA)
        	lev1 <- lev[counts > 0L]
	    	if (length(lev1) == 1L)
	    		stop("training data from only one class given")
        	y <- factor(y, levels=lev1)
    	}
    	if (length(lev) == 1L)
    		stop("training data from only one class given")
	    if (length(lev) == 2L) {
    	   	y <- as.vector(unclass(y)) - 1
       		res <- dannet.default(x = x, y = y, weights = w, entropy = TRUE, ...)
   	 	} else {
       		y <- class.ind(y)
       		res <- dannet.default(x = x, y = y, weights = w, softmax = TRUE, ...)
    	}
    	res$lev <- lev
    	res$lev1 <- lev1
    	res$coefnames <- colnames(x)
    	cl <- match.call()
    	cl[[1]] <- as.name("dannet")
    	res$call <- cl
    	res$na.action <- na.action
    	class(res) <- c("dannet", "nnet")
    	res
    }
}



#' @rdname dannet
#' @export

##...: size, Wts, mask, linout, entropy, softmax, censored, skip, rang, decay, maxit, Hess, trace, MaxNWts, abstol, reltol, ...
dannet.default <- function(x, y, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	"exponential", "gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only, itr = 3, weights = rep(1, nrow(x)), reps = 1, ...) {
	dannet.fit <- function(x, y, wf, itr, weights = rep(1, nrow(x)), ...) {
		w <- list()
		ntr <- nrow(x)
		weights <- weights/sum(weights) * ntr     		# rescale weights such that they sum up to ntr
		w[[1]] <- weights
		names(w[[1]]) <- rownames(x)
		# res <- nnet.default(x = x, y = y, weights = weights, ...)
		res <- nnetRep(reps, x = x, y = y, weights = weights, ...)
		for (i in seq_len(itr)) {
			# 1. prediction
			post <- predict(res, type = "raw")
			if (any(!is.finite(post)))
				stop("inifinite, NA or NaN values in 'post', may indiciate numerical problems due to small observation weights, please check your settings of 'bw', 'k' and 'wf'")
			# 2. calculate weights and fit model
# print(post)
# print(abs(2*as.vector(post) - 1))
			if (ncol(y) == 1L) {	             		# in this case predict.nnet returns posteriors for only one class
				weights <- wf(abs(2*as.vector(post) - 1))	# largest if both probabilities are equal
			} else {
				spost <- apply(post, 1, sort, decreasing = TRUE)
				weights <- wf(spost[1,] - spost[2,])    # largest if both probabilities are equal
			}
			if (all(weights == 0)) {
				stop("all observation weights are zero")
			}
# print(weights)
			weights <- weights/sum(weights) * ntr     	# rescale weights such that they sum up to ntr
			names(weights) <- rownames(x)
# print(weights)			
# print(y)
# print(ncol(y))
			# 3. check if break
			if (ncol(y) == 1L) 
				freqs <- tapply(weights, y, sum)  
			else 
				freqs <- weights %*% y 
# print(freqs)
			if (any(freqs == 0L))               	# class where all weights are zero
				warning("for at least one class all weights are zero")
			if (sum(freqs > 0) <= 1L) { 			# additionally look if only one single class left
				warning("training data from only one class, breaking out of iterative procedure")
				itr <- i - 1
				break
			} else {
				w[[i+1]] <- weights
				# res <- nnet.default(x = x, y = y, weights = weights, ...)
				res <- nnetRep(reps, x = x, y = y, weights = weights, ...)
			}
		}
		names(w) <- seq_along(w) - 1
		res$weights <- w
		res$itr <- itr
		return(res)
	}
    x <- as.matrix(x)
    y <- as.matrix(y)
    if (any(is.na(x)))
    	stop("missing values in 'x'")
    if (any(is.na(y)))
    	stop("missing values in 'y'")
    ntr <- nrow(x)

    if (!missing(itr)) {
    	if (itr < 1)
			stop("'itr' must be >= 1")
    	if (abs(itr - round(itr)) > .Machine$double.eps^0.5)
       		warning("'itr' is not a natural number and is rounded off")
    }

    if (any(weights < 0))
        stop("weights have to be larger or equal to zero")
    if (all(weights == 0))
        stop("all weights are zero")

    if (is.character(wf)) {
    	m <- match.call(expand.dots = FALSE)
    	m$n <- ntr
    	m[[1L]] <- as.name("generatewf")
    	wf <- eval.parent(m)
    } else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    	if(!is.null(attr(wf, "adaptive"))) {
    		if(attr(wf, "adaptive")) {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") + 1 > ntr)
    				stop("'k + 1' is larger than 'nrow(x)'")
    		} else {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") > ntr)
    				stop("'k' is larger than 'nrow(x)'")
    		}
    	}
    } else
    	stop("argument 'wf' has to be either a character or a function")
	res <- dannet.fit(x = x, y = y, wf = wf, itr = itr, weights = weights, ...)
    res <- c(res, list(wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive"), reps = reps))
    cl <- match.call()
    cl[[1]] <- as.name("dannet")
    res$call <- cl
    class(res) <- c("dannet", "nnet")
    return(res)
}



# @param x A \code{dannet} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @noRd
#'
#' @export

print.dannet <- function (x, ...) {
	if (inherits(x, "dannet.formula")) {
    	NextMethod(x, ...)
    } else {
    	cat("a ", x$n[1L], "-", x$n[2L], "-", x$n[3L], " network", 
        	sep = "")
    	cat(" with", length(x$wts), "weights\n")
    	if (length(x$coefnames)) 
        	cat("inputs:", x$coefnames, "\noutput(s):", deparse(x$call$y, 
            	backtick = TRUE), "\n")
    	cat("options were -")
    	tconn <- diff(x$nconn)
    	if (tconn[length(tconn)] > x$n[2L] + 1L) 
        	cat(" skip-layer connections ")
    	if (x$nunits > x$nsunits && !x$softmax) 
        	cat(" linear output units ")
    	if (x$entropy) 
        	cat(" entropy fitting ")
    	if (x$softmax) 
        	cat(" softmax modelling ")
    	if (x$decay[1L] > 0) 
        	cat(" decay=", x$decay[1L], sep = "")
    	cat("\n")
    }    	
    if(!is.null(attr(x$wf, "name"))) {
        cat("\nWindow function: ")
        cat(deparse(attr(x$wf, "name")), sep = "\n")  
    } else {
        cat("\nWindow function:\n")
        cat(deparse(x$wf), sep = "\n")  
    }
    if(!is.null(x$bw))
        cat("Bandwidth: ", x$bw, "\n")
    if(!is.null(x$k))
    	cat("k: ", x$k, "\n")
    if(!is.null(x$nn.only))
    	cat("Nearest neighbors only: ", x$nn.only, "\n")
    if(!is.null(x$adaptive))
    	cat("Adaptive bandwidth: ", x$adaptive, "\n")
    cat("Iterations: ", x$itr, "\n")
	invisible(x)
}



#  Copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#' Predict new examples by a trained discriminant adaptive neural net.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"dannet"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.dannet(x)} regardless of 
#' the class of the object. 
#'
#' In contrast to \code{\link[nnet]{predict.nnet}} \code{predict.dannet} does not have
#' a \code{type} argument. Since \code{dannet} is only suitabe for classification, both, the 
#' predicted posterior probabilities and the class labels, are returned.
#'
#' @title Predict New Examples by a Trained Discriminant Adaptive Neural Net
#'
#' @param object Object of class \code{"dannet"}.
#' @param newdata A \code{matrix} or \code{data.frame} of test examples. A vector is considered to be
#' a row vector comprising a single case.
#' @param \dots Arguments passed to or from other methods.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}. Cambridge.
#'
#' Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
#'
#' @seealso \code{\link{dannet}}, \code{\link[nnet]{nnet}}.
#'
#' @examples
#' fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 0.5, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
#' pred <- predict(fit)
#' mean(pred$class != iris$Species)

# ## comparison with nnet:
# library(MASS)
# fit1 <- nnet(Species ~ Sepal.Length + Sepal.Width, data = iris)
# pred <- predict(fit1)
# mean(pred$class != iris$Species)
# 
# fit2 <- dannet(Species ~ Sepal.Length + Sepal.Width, data = iris,
#     wf = "gaussian", bw = 0.5)
# pred <- predict(fit2)
# mean(pred$class != iris$Species)
# 
# ## plot of decision boundary (maximum posterior probabilities):
# grid <- expand.grid(Sepal.Length = seq(4,8,0.1), Sepal.Width = seq(2,5,0.1)) 
# 
# predgrid1 <- predict(fit1, newdata = grid)$posterior
# predgrid2 <- predict(fit2, newdata = grid)$posterior
# 
# par(mfrow = c(1,2))
# contour(seq(4,8,0.1), seq(2,5,0.1), 
#     matrix(as.numeric(apply(predgrid1, 1, max)), nrow = length(seq(4,8,0.1))))
# contour(seq(4,8,0.1), seq(2,5,0.1), 
#     matrix(as.numeric(apply(predgrid2, 1, max)), nrow = length(seq(4,8,0.1))))
# points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#     cex = fit2$weights[[3]]*2, col = as.numeric(iris$Species))
# 
#' @keywords classif neural
#' 
#' @rdname predict.dannet
#'
#' @import nnet
#'
#' @export

predict.dannet <- function(object, newdata, ...) {
    if (!inherits(object, "dannet")) 
        stop("object not of class \"dannet\"")
    posterior <- NextMethod(object, newdata, type = "raw", ...)
    if (length(object$lev) == 2) {
    	posterior <- cbind(1 - posterior, posterior) # posterior is probability for 2nd factor level
    	colnames(posterior) <- object$lev 
    }
	gr <- factor(object$lev1[max.col(posterior)], levels = object$lev)
	names(gr) <- rn <- rownames(posterior)
	if (is.null(rn)) {
		rn <- seq_along(gr)
		rownames(posterior) <- rn
		names(gr) <- rn
	}
	return(list(class = gr, posterior = posterior))
}



#' @noRd
#'
#' @importFrom stats weights
#' @export

weights.dannet <- function (object, ...) {
	if (!inherits(object, "dannet"))
        stop("object not of class \"dannet\"")
	object$weights
}



#' Calculate value of objective function and gradient for a fitted neural network.
#'
#' @param object Object inheriting from \code{"nnet"}.
#'
#' @return Value of objective function of a fitted neural network. The attribute "gradient" contains the gradient vector.
#'
#' @export

nnetGradient <- function(object) { #, x, y, weights, ...) {
	#x <-
	#y <-	
#    if(dim(x)[1L] != dim(y)[1L]) stop("dims of 'x' and 'y' must match")
    nw <- length(object$wts)
    decay <- object$decay
    if(!inherits(object, "nnet")) 
    	stop("object not of class \"nnet\"")
    if(length(decay) == 1) decay <- rep(decay, nw)
    .C("VR_set_net",
		as.integer(object$n), 
		as.integer(object$nconn),
       	as.integer(object$conn), 
       	as.double(decay),
       	as.integer(object$nsunits), 
       	as.integer(object$entropy),
       	as.integer(object$softmax), 
       	as.integer(object$censored)
       	)
	z <- .C("VR_dfunc", as.double(object$wts), df = double(length(object$wts)), 
    	fp = as.double(1))
    fp <- z$fp
    attr(fp, "gradient") <- z$df
    .C("VR_unset_net")
    fp
}



# @noRd
#
# @export

# nnetHess <- function(net, x, y, weights)
# {
    # x <- as.matrix(x)
    # y <- as.matrix(y)
    # if(dim(x)[1L] != dim(y)[1L]) stop("dims of 'x' and 'y' must match")
    # nw <- length(net$wts)
    # decay <- net$decay
    # if(length(decay) == 1) decay <- rep(decay, nw)
    # .C(VR_set_net,
       # as.integer(net$n),
       # as.integer(net$nconn),
       # as.integer(net$conn),
       # as.double(decay),
       # as.integer(net$nsunits),
       # as.integer(net$entropy),
       # as.integer(net$softmax),
       # as.integer(net$censored)
       # )
    # ntr <- dim(x)[1L]
    # if (missing(weights)) 
    	# weights <- rep(1, ntr)
    # if (length(weights) != ntr || any(weights < 0))
        # stop("invalid weights vector")
    # Z <- as.double(cbind(x,y))
    # storage.mode(weights) <- "double"
    # z <- matrix(.C(VR_nnHessian, as.integer(ntr), Z, weights,
                   # as.double(net$wts), H = double(nw*nw))$H,
                # nw, nw)
    # .C(VR_unset_net)
    # z
# }