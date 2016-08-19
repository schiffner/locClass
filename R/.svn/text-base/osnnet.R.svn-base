#  Copyright (C) 2011 J. Schiffner
#  Copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
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

#' A localized version of single-hidden-layer neural networks, possibly with skip-layer connections.
#'
#' This is a localized version of neural networks where a neural network is fitted for each test observation
#' based on the training data near the trial point. It
#' is based on the function \code{\link[nnet]{nnet}} from package \pkg{nnet}.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{\link{predict.osnnet}}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{osnnet}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' If the predictor variables include factors, the formula interface must be used in order 
#' to get a correct model matrix.
#'
#' @title Observation Specific Neural Networks
#'
#' @param formula A \code{formula} of the form \code{response ~ x1 + x2 + \dots}.  
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param y (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} of target values for examples.
#' @param size Number of units in the hidden layer. Can be zero if there are skip-layer units.
#' @param contrasts A list of contrasts to be used for some or all of the factors appearing as variables in the model formula.
#' @param Wts Initial parameter vector. If missing chosen at random.
#' @param mask Logical vector indicating which parameters should be optimized (default all).
#' @param linout Switch for linear output units. Default logistic output units.
#' @param entropy Switch for entropy (= maximum conditional likelihood) fitting. Default by least-squares.
#' @param softmax Switch for softmax (log-linear model) and maximum conditional
#'   likelihood fitting. \code{linout}, \code{entropy}, \code{softmax} and \code{censored} are mutually
#'   exclusive.
#' @param censored A variant on \code{softmax}, in which non-zero targets mean possible
#'   classes. Thus for \code{softmax} a row of \code{(0, 1, 1)} means one example
#'   each of classes 2 and 3, but for \code{censored} it means one example whose
#'   class is only known to be 2 or 3.
#' @param skip Switch to add skip-layer connections from input to output.
#' @param rang Initial random weights on [-\code{rang}, \code{rang}].  Value about 0.5 unless the
#'   inputs are large, in which case it should be chosen so that
#'   \code{rang} * max(\code{|x|}) is about 1.
#' @param decay Parameter for weight decay. Default 0.
#' @param maxit Maximum number of iterations. Default 100.
#' @param trace Switch for tracing optimization. Default \code{TRUE}.
#' @param MaxNWts The maximum allowable number of weights.  There is no intrinsic limit
#'   in the code, but increasing \code{MaxNWts} will probably allow fits that
#'   are very slow and time-consuming.
#' @param abstol Stop if the fit criterion falls below \code{abstol}, indicating an
#'   essentially perfect fit.
#' @param reltol  Stop if the optimizer is unable to reduce the fit criterion by a
#'   factor of at least \code{1 - reltol}.
#' @param reps Neural networks are fitted repeatedly (\code{reps} times) for different initial values and the solution with largest likelihood
#'  value is kept. Defaults to 1. (\code{reps} larger one does not make sense if \code{Wts} is specified.)
#' @param wf A window function which is used to calculate weights that are introduced into 
#'   the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#'   For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param \dots Arguments passed to or from other methods.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if \code{NA}s are found. The default action is for the
#'   procedure to fail. 
#'   An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#'   variable. (NOTE: If given, this argument must be named.)
#'
#' @return An object of class \code{"osnnet.formula"} or \code{"osnnet"}, a \code{list} containing the following components:
#'   \item{x}{A \code{matrix} containing the explanatory variables.}
#'   \item{y}{If argument \code{y} was a factor a class inicator matrix, otherwise a \code{matrix} that contains \code{y} unchanged.}
#'   \item{...}{}
#'   \item{mask}{The \code{mask} vector used.}
#'   \item{maxit}{The \code{maxit} argument used.}
#'   \item{trace}{The \code{trace} argument used.}
#'   \item{abstol}{The \code{abstol} argument used.}
#'   \item{reltol}{The \code{reltol} argument used.}
#'   \item{lev}{If \code{y} is a factor the class labels (levels of \code{y}).}  
#'   \item{wf}{The window function used. Always a function, even if the input was a string.}
#'   \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#' 	 \item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#'   \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	 specified.) \code{TRUE} if only the \code{k} nearest neighbors recieve a positive weight, \code{FALSE} otherwise.}
#'   \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the local density of data points, \code{FALSE} if the bandwidth
#'	  is fixed.}
#'   \item{variant}{(Only if \code{wf} is a string or one of the window functions documented in \code{\link[=biweight]{wfs}} is used, for internal use only). 
#'	  An integer indicating which weighting scheme is implied by \code{bw}, \code{k} and \code{nn.only}.}
#'   \item{call}{The (matched) function call.}
#'
#' @references 
#' Czogiel, I., Luebke, K., Zentgraf, M. and Weihs, C. (2007), Localized linear discriminant analysis.
#' In Decker, R. and Lenz, H.-J., editors, Advances in Data Analysis, volume 33 of Studies in Classification,
#' Data Analysis, and Knowledge Organization, pages 133--140, Springer, Berlin Heidelberg.
#'
#' Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}. Cambridge.
#'
#' Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
#'
#' @seealso \code{\link{predict.osnnet}}, \code{\link[nnet]{nnet}}.
#'
#' @examples
#' samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
#' fit <- osnnet(Species ~ ., data = iris, subset = samp, size = 2, 
#'                rang = 0.1, maxit = 200, bw = 0.5, reps = 2)
#' pred <- predict(fit)
#' 
#' @keywords neural
#'
#' @aliases osnnet osnnet.default osnnet.formula
#'
#' @export
#'
#' @import nnet

osnnet <- function(x, ...)
	UseMethod("osnnet")

	
	
#' @rdname osnnet
#' @method osnnet formula
#'
#' @S3method osnnet formula

osnnet.formula <- function(formula, data, ..., subset, na.action, contrasts = NULL) {
    class.ind <- function(cl)			# class indictor matrix
    {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval.parent(m$data)))
        m$data <- as.data.frame(data)
    m$... <- m$contrasts <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    cons <- attr(x, "contrast")
    xint <- match("(Intercept)", colnames(x), nomatch=0L)
    if (xint > 0L) 
    	x <- x[, -xint, drop=FALSE] # Bias term is used for intercepts
    y <- model.response(m)
    if (is.factor(y)) {							# y factor
        lev <- levels(y)
        counts <- table(y)
        if (any(counts == 0L)) {
            empty <- lev[counts == 0L]
            warning(sprintf(ngettext(length(empty),
                                     "group %s is empty",
                                     "groups %s are empty"),
                            paste(empty, collapse=" ")), domain = NA)
            y <- factor(y, levels=lev[counts > 0L])
        }
        if (length(lev) == 2L) {
            y <- as.vector(unclass(y)) - 1		# 0-1-vector
            res <- osnnet.default(x, y, entropy=TRUE, ...)
            res$lev <- lev
        } else {
            y <- class.ind(y)					# indicator matrix
            res <- osnnet.default(x, y, softmax=TRUE, ...)
            res$lev <- lev
        }
    } else res <- osnnet.default(x, y, ...)		# y not a factor
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$call <- match.call()
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("osnnet.formula", "osnnet")
    res
}



#' @rdname osnnet
#' @method osnnet default
#'
#' @S3method osnnet default

osnnet.default <- function (x, y, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	"exponential", "gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only = TRUE, 
	size, Wts, mask = rep(TRUE, length(wts)), linout = FALSE, entropy = FALSE, softmax = FALSE, 
	censored = FALSE, skip = FALSE, rang = 0.7, decay = 0, maxit = 100, 
    trace = TRUE, MaxNWts = 1000, abstol = 1e-04, reltol = 1e-08, reps = 1, ...) {
	net <- NULL
    x <- as.matrix(x, rownames.force = TRUE)
    y <- as.matrix(y)
    if (any(is.na(x))) 
        stop("missing values in 'x'")
    if (any(is.na(y))) 
        stop("missing values in 'y'")
    ntr <- dim(x)[1L]
    if (ntr != dim(y)[1L]) 
        stop("nrows of 'x' and 'y' must match")
    if (linout && entropy) 
        stop("entropy fit only for logistic units")
    if (softmax) {
        linout <- TRUE
        entropy <- FALSE
    }
    if (censored) {
        linout <- TRUE
        entropy <- FALSE
        softmax <- TRUE
    }
    nout <- dim(y)[2L]
    net$n <- c(dim(x)[2L], size, nout)
    net$nunits <- as.integer(1L + sum(net$n))
    net$nconn <- rep(0, net$nunits + 1L)
    net$conn <- numeric(0L)
    net <- nnet:::norm.net(net)
    if (skip) 
        net <- nnet:::add.net(net, seq(1L, net$n[1L]), seq(1L + net$n[1L] + 
            net$n[2L], net$nunits - 1L))
    if ((nwts <- length(net$conn)) == 0) 
        stop("no weights to fit")
    if (nwts > MaxNWts) 
        stop(gettextf("too many (%d) weights", nwts), domain = NA)
    nsunits <- net$nunits
    if (linout) 
        nsunits <- net$nunits - net$n[3L]
    net$nsunits <- nsunits
    net$decay <- decay
    net$entropy <- entropy
    if (softmax && NCOL(y) < 2L) 
        stop("'softmax = TRUE' requires at least two response categories")
    net$softmax <- softmax
    net$censored <- censored
    ## if fixed initial weights are given these are passed to predict.osnnet, together with random = FALSE
    ## otherwise 0-vector of correct length is passed to predict.osnnet
    ## 	if rang > 0 random = TRUE and random initial weights are produced in predict.ossnnet
    ## 	otherwise random = FALSE and the 0-vector is used as initial weights in predict.osnnet
    net$rang <- rang
    if (!missing(Wts)) {
    	if (length(Wts) != nwts) 
        	stop("weights vector of incorrect length")
        net$wts <- wts <- Wts
        net$random <- FALSE
    } else {
    	net$wts <- wts <- rep(0, nwts)
    	if (rang > 0)
    		net$random <- TRUE
    	else
    		net$random <- FALSE 
    }
	if (length(mask) != nwts) 
    	stop("incorrect length of 'mask'")
    if (length(decay) == 1L) 
        decay <- rep(decay, nwts)
	net$decay <- decay
#    if (missing(Wts))
#        if (rang > 0) 
#            wts <- runif(nwts, -rang, rang)
#        else wts <- rep(0, nwts)
#    else wts <- Wts	# lägencheck nötig
#    if (length(wts) != nwts) 
#        stop("weights vector of incorrect length")
#    if (length(mask) != length(wts)) 
#        stop("incorrect length of 'mask'")
#    if (trace) {###?
#        cat("# weights: ", length(wts))
#        nw <- sum(mask != 0)
#        if (nw < length(wts)) 
#            cat(" (", nw, " variable)\n", sep = "")
#        else cat("\n")
#        flush.console()
#    }
#    if (length(decay) == 1L) 
#        decay <- rep(decay, length(wts))
	lev <- NULL
    if (entropy) 
        lev <- c("0", "1")
    if (softmax) 
        lev <- colnames(y)
	## checks on k and bw
    if (is.character(wf)) {
		cl <- sys.call(-1)
#print(cl)
#print(match.call(call = cl))
#print(cl$formula)
#stop("bla")
#print(sys.call())
#print(sys.call(-1))
#print(sys.call(-2))
#print(sys.call(-3))
#print(sys.calls())
		formula <- grepl("formula", cl[[1L]]) || grepl("multinom", cl[[1L]]) #????
#print(formula)
    	m <- match.call(expand.dots = FALSE)
    	m$n <- ntr
    	m[[1L]] <- as.name("checkwf")
    	if (formula)
    		check <- eval.parent(m)
    	else
    		check <- eval.parent(m, n = 0)    	
    	cl <- match.call()
    	cl[[1]] <- as.name("osnnet")
    	return(structure(c(list(x = x, y = y), net, list(mask = mask, maxit = maxit, trace = trace, 
    		abstol = abstol, reltol = reltol, reps = reps, lev = lev, wf = check$wf, bw = check$bw, k = check$k, nn.only = check$nn.only, 
    		adaptive = check$adaptive, variant = check$variant, call = cl)), class = "osnnet"))
    } else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    	if (!is.null(attr(wf, "adaptive"))) {
    		if (attr(wf, "adaptive")) {			# adaptive bandwidth
     				if (attr(wf, "k") + 1 > ntr)
    					stop("'k + 1' is larger than 'nrow(x)'")
     				if (attr(wf, "nn.only")) 	# only knn
    					variant <- 3
    				else						# all observations
    					variant <- 4
    		} else {							# fixed bandwidth
    			if (!is.null(attr(wf, "k"))) {
    				if (attr(wf, "k") > ntr)
    					stop("'k' is larger than 'nrow(x)'")
    				variant <- 2
    			} else 
    				variant <- 1
    		}
    	} else
    		variant <- NULL
    	cl <- match.call()
    	cl[[1]] <- as.name("osnnet")
    	return(structure(c(list(x = x, y = y), net, list(mask = mask, maxit = maxit, trace = trace, 
    		abstol = abstol, reltol = reltol, reps = reps, lev = lev, wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), 
    		nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive"), variant = variant, call = cl)), class = "osnnet"))
    } else
		stop("argument 'wf' has to be either a character or a function")
}	



# @param x An \code{osnnet} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @method print osnnet
#' @noRd
#'
#' @S3method print osnnet

print.osnnet <- function(x, ...) {
    if (!inherits(x, "osnnet")) 
        stop("not a legitimate \"osnnet\" object")
    if (!is.null(cl <- x$call)) {
        names(cl)[2L] <- ""
        cat("Call:\n")
        dput(cl, control = NULL)
    }   
    # taken from print.nnet
    cat("a ", x$n[1L], "-", x$n[2L], "-", x$n[3L], " network", 
        sep = "")
    cat(" with", length(x$Wts), "weights\n")
    if (length(x$coefnames)) 
        cat("inputs:", x$coefnames, "\noutput(s):", deparse(formula(x)[[2L]], 
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
    # weights
    if(is.character(x$wf)) {
	    cat("\nWindow function: ")
        cat(x$wf, sep="\n")  
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
	invisible(x)
}



#' Predict new examples in conjunction with \code{\link{osnnet}}.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"osnnet"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.osnnet(x)} regardless of 
#' the class of the object. 
#' 
#' If the response in \code{formula} is a factor, appropriate classification
#' networks are constructed; these have one output and entropy fit if the
#' number of levels is two, and a number of outputs equal to the number
#' of classes and a softmax output stage for more levels.  If the
#' response is not a factor, it is passed on unchanged to \code{predict.osnnet}.
#'
#' Optimization is done via the BFGS method of \code{\link{optim}}.
#'
#' @title Predict New Examples Based on Observation Specific Neural Netwoks
#'
#' @param object Object of class \code{"osnnet"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if \code{object} has a
#' \code{formula}, a \code{data.frame} with columns of the same names as the
#' variables used. A vector will be interpreted as a row
#' vector. If \code{newdata} is missing, an attempt will be made to
#' retrieve the data used in \code{\link{osnnet}}.
#' @param type Type of output.
#' @param \dots Arguments passed to or from other methods.
#'
#' @return If \code{type = "raw"}, the matrix of values returned by the trained network;
#'   if \code{type = "class"}, the corresponding class (which is probably only
#'   useful if the net was generated by \code{\link{osnnet.formula}}).
#'
#' @seealso \code{\link{osnnet}}, \code{\link[nnet]{predict.nnet}}, \code{\link[nnet]{nnet}}.
#'
#' @examples
#' samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
#' fit <- osnnet(Species ~ ., data = iris, subset = samp, size = 2, 
#'                rang = 0.1, maxit = 200, bw = 0.5, reps = 2)
#' pred <- predict(fit)
#' 
#' @keywords neural
#' 
#' @method predict osnnet
#' @rdname predict.osnnet
#'
#' @S3method predict osnnet
#'
#' @useDynLib locClass

predict.osnnet <- function(object, newdata, type = c("raw", "class"), ...) {    
	if (!inherits(object, "osnnet")) 
        stop("object not of class \"osnnet\"")
    type <- match.arg(type)
    if (missing(newdata)) {
    	x <- object$x
    	rn <- rownames(x)
    } else { ## keep
        if (inherits(object, "osnnet.formula")) {
            newdata <- as.data.frame(newdata)
            #rn <- row.names(newdata)
            Terms <- delete.response(object$terms)
            m <- model.frame(Terms, newdata, na.action = na.omit, 
                xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses"))) 
                .checkMFClasses(cl, m)
            x <- model.matrix(Terms, m, contrasts = object$contrasts)
      		rn <- rownames(x)
            xint <- match("(Intercept)", colnames(x), nomatch = 0L)
            if (xint > 0L) 
                x <- x[, -xint, drop = FALSE]
        }
        else {
            if (is.null(dim(newdata))) 
                dim(newdata) <- c(1L, length(newdata))
            x <- as.matrix(newdata)
            if (any(is.na(x))) 
                stop("missing values in 'x'")
            rn <- rownames(newdata)
        }
	}
	if (object$trace) {
        cat("# weights: ", length(object$wts))
        nw <- sum(object$mask != 0)
        if (nw < length(object$wts)) 
            cat(" (", nw, " variable)\n", sep = "")
        else cat("\n")
        flush.console()
    }
    ntest <- nrow(x)
    nout <- object$n[3L]
	Z <- as.double(cbind(object$x, object$y))
	wfs <- c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian",
		"optcosine", "rectangular", "triangular")
	if (is.function(object$wf) && !is.null(attr(object$wf,"name")) && attr(object$wf, "name") %in% wfs)
		object$wf <- attr(object$wf, "name")	
    if (is.character(object$wf)) {
		wfs <- paste(wfs, rep(1:3, each = length(wfs)), sep = "")
		wfs <- c(wfs, "cauchy4", "exponential4", "gaussian4")
		object$wf <- paste(object$wf, object$variant, sep = "")
		object$wf <- match(object$wf, wfs)
	}
	
	zList <- list()
	for (i in 1:object$reps) {
		zList[[i]] <- .Call("predosnnet", 
			as.integer(object$n),
			as.integer(object$nconn),
			as.integer(object$conn),
			as.double(object$decay),
			as.integer(object$nsunits),
			as.integer(object$entropy),
			as.integer(object$softmax),
			as.integer(object$censored),
		
			as.integer(nrow(object$x)),
			Z,
			as.integer(length(object$conn)),
			as.double(object$wts),
			object$random,
			as.double(object$rang),
			double(1),
			as.integer(object$maxit),
			as.logical(object$trace), 
			as.integer(object$mask), 
			as.double(object$abstol),
			as.double(object$reltol),
			integer(1L),

			as.integer(ntest),
			as.integer(nout),
			x,
		
			object$wf,
			ifelse(is.integer(object$wf) && !is.null(object$bw), object$bw, 0),
			ifelse(is.integer(object$wf) && !is.null(object$k), as.integer(object$k), 0L),
			new.env())
	}
	if (object$reps > 1) {
		obj <- matrix(sapply(zList, function(x) x[[2]]), ncol = object$reps)
# print(obj)
		sol <- max.col(-obj)
# print(sol)
		z <- matrix(0, ntest, nout)
		for (i in 1:object$reps) {
			z[sol == i, ] <- zList[[i]][[1]][sol == i,, drop = FALSE]
		}
	} else 
		z <- zList[[1]][[1]]
# print(zList)
# print(z)
	dimnames(z) <- list(rn, colnames(object$y))
    switch(type, raw = z,
    	class = {
        	if (is.null(object$lev)) 
            	stop("inappropriate fit for class")
          	if (ncol(z) > 1L) 
          		cl <- factor(object$lev[max.col(z)], levels = object$lev)
            else 
            	cl <- factor(object$lev[1L + (z > 0.5)], levels = object$lev)
            names(cl) <- rn
            cl
           })
}
