#  Copyright (C) 2011 J. Schiffner
#  Copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
#  Copyright (C) R Development Core Team
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

#' A local version of logistic regression for classification that puts increased emphasis 
#' on a good model fit near the decision boundary.
#'
#' Local logistic regression (Hand and Vinciotti, 2003) is a modification of the standard logistic regression approach 
#' to discrimination. For discrimination a good fit of the model is required especially near the true decision 
#' boundary. Therefore weights are introduced into the fitting process that reflect the proximity of training points 
#' to the decision boundary. 
#' Let the class levels be 0 and 1.
#' The distance of a training observation \eqn{x} to the decision boundary is measured by means of 
#' the difference \eqn{P(1 | x) - thr} where \code{thr} is a threshold in \eqn{[0,1]}. 
#' Since \eqn{P(1 | x)} is not known in advance an iterative 
#' procedure is required. We start by fitting an unweighted logistic regression model to the data in order to obtain 
#' initial estimates of \eqn{P(1 | x)}. These are used to calculate the observation weights. 
#' Model fitting and calculation of weights is done several times in turn. 
#' By default, the number of iterations is limited to 3.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{dalr}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{dalr}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#' 
#' Internally, \code{\link{glm.fit}} with \code{family = binomial()} is used and the weights produced using 
#' \code{wf} are passed to \code{\link{glm.fit}} via its \code{weights} argument.
#' 
#' If the predictor variables include factors, the formula interface must be used in order 
#' to get a correct model matrix.
#'
#' Warnings about non-integer #successes in a binomial glm are expected.
#' 
#' @title Discriminant Adaptive Logistic Regression
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + \dots}, that is, the response
#' is the grouping \code{factor} and the right hand side specifies the (non-\code{factor})
#' discriminators. Details concerning model specification are given in the documentation of \code{\link{glm}}.
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param X (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param Y (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying the class membership for each observation.
#' @param thr The threshold value used to predict class membership, defaults to 0.5. See Details.
#' @param wf A window function which is used to calculate weights that are introduced into 
#' the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#' For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param itr Number of iterations for model fitting, defaults to 3. See also the Details section.
#' @param intercept Should the model contain an intercept? Passed to \code{\link{glm.fit}}, null.model.
#' @param weights Initial observation weights (defaults to a vector of 1s).
#' @param \dots Further arguments to \code{\link{glm}}. Currently \code{offset},
#'  \code{control}, \code{model}, \code{x}, \code{y}, \code{contrasts}, \code{start}, \code{etastart}, 
#'  \code{mustart} are supported.
#  family is "binomial", method?. Note that some of theses arguments only make sense when using the formula method, namely: ...?
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.)
#' @param na.action The default is first, any \code{na.action} attribute of data, second a \code{na.action} setting of options, and third \code{na.fail} if that is unset.
#'   The default is first, a \code{na.action} setting of options, and second \code{na.fail} if that is unset.
#'
#' @return An object of class \code{"dalr"} inheriting from class \code{"glm"}, a list containing at least the following components:
#' 
#' Values of \code{glm}:
#' \item{coefficients}{A named vector of coefficients.}
#' \item{residuals}{The working residuals, that is the residuals in the final iteration of the IWLS fit. 
#'	Since cases with zero weights are omitted, their working residuals are \code{NA}.}
#' \item{fitted.values}{The fitted mean values, obtained by transforming the linear predictors by the inverse of the link function.}
#' \item{rank}{The numeric rank of the fitted linear model.}
#' \item{family}{The \code{\link{family}} object used.}
#' \item{linear.predictor}{The linear fit on link scale.}
#' \item{deviance}{Up to a constant, minus twice the maximized log-likelihood. Where sensible, the constant is chosen so that a saturated model has deviance zero.}
#' \item{aic}{A version of Akaike's An Information Criterion, minus twice the maximized log-likelihood plus twice the number of parameters, 
#'	computed by the aic component of the family. For binomial and poisson families the dispersion is fixed at one and the number of parameters 
#' is the number of coefficients. For gaussian, Gamma and inverse gaussian families the dispersion is estimated from the residual deviance, 
#' and the number of parameters is the number of coefficients plus one. For a gaussian family the MLE of the dispersion is used so this is a 
#' valid value of AIC, but for Gamma and inverse gaussian families it is not. For families fitted by quasi-likelihood the value is NA.}
#' \item{null.deviance}{The deviance for the null model, comparable with deviance. The null model will include the offset, and an intercept if there is one in the model. 
#'	Note that this will be incorrect if the link function depends on the data other than through the fitted mean: specify a zero offset to force a correct calculation.}
#' \item{iter}{The number of iterations of IWLS used.}
#' \item{weights}{A list of length \code{itr + 1}. The working weights, that is the observation weights in the final iteration of the IWLS fit.}
#' \item{prior.weights}{A list of length \code{itr + 1}. The observation weights initially supplied, the first list element is a vector of 1s if none were specified.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom for the null model.}
#' \item{y}{If requested (the default) the y vector used. (It is a vector even for a binomial model.)}
#' \item{x}{If requested, the model matrix.}
#' \item{model}{If requested (the default), the model frame.}
#' \item{converged}{Logical. Was the IWLS algorithm judged to have converged?}
#' \item{boundary}{Logical. Is the fitted value on the boundary of the attainable values?}
#' \item{call}{The (matched) function call.}
#' \item{formula}{The formula supplied.}
#' \item{terms}{The \code{\link{terms}} object used.}
#' \item{data}{The data argument.}
#' \item{offset}{The offset vector used.}
#' \item{control}{The value of the control argument used.}
#' \item{method}{The name of the fitter function used, currently always \code{"glm.fit"}.}
#' \item{contrasts}{(Where relevant) the contrasts used.}
#' \item{xlevels}{(Where relevant) a record of the levels of the factors used in fitting.}
#' \item{na.action}{(Where relevant) information returned by \code{\link{model.frame}} on the special handling of NAs.}
#' Additionally, \code{dalr} returns
#' \item{lev}{The class labels (the levels of \code{grouping}).}
#' \item{thr}{The threshold used.}
#' \item{itr}{The number of iterations used.}
#' \item{wf}{The window function used. Always a function, even if the input was a string.}
#' \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	 The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#' \item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	 The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#' \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	specified.) \code{TRUE} if only the \code{k} nearest neighbors recieve a positive weight, \code{FALSE} otherwise.}
#' \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the local density of data points, \code{FALSE} if the bandwidth
#'	 is fixed.}
#'
#' @references 
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#' 
#' @seealso \code{\link{predict.dalr}}, \code{\link{glm}}, \code{\link{predict.glm}}.
#'
#' @examples
#' # generate toy data set of Hand und Vinciotti (2003):
#' x1 <- x2 <- seq(0.1,1,0.05)
#' train <- expand.grid(x1 = x1, x2 = x2)
#' posterior <- train$x2/(train$x1 + train$x2)
#' y <- as.factor(sapply(posterior, function(x) sample(0:1, size = 1, 
#'     prob = c(1-x,x))))
#' train <- data.frame(train, y = y)
#' 
#' par(mfrow = c(1,3))
#' 
#' # contours of true class posterior probabilities:
#' plot(train$x1, train$x2, col = y, pch = 19, main = "true posteriors")
#' contour(x1, x2, matrix(posterior, length(x1)), add = TRUE)
#' 
#' # 0.3-contour line fit of logistic regression:
#' glob.fit <- glm(y ~ ., data = train, family = "binomial")
#' plot(train$x1, train$x2, col = y, pch = 19, main = "global fit")
#' contour(x1, x2, matrix(glob.fit$fitted.values, length(x1)), 
#'     levels = 0.3, add = TRUE)
#' 
#' # 0.3-contour line fit of local logistic regression:
#' loc.fit <- dalr(y ~ ., data = train, thr = 0.3, wf = "gaussian", bw = 0.2)
#' plot(train$x1, train$x2, col = y, pch = 19, main = "local fit")
#' contour(x1, x2, matrix(loc.fit$fitted.values, length(x1)), 
#'     levels = 0.3, add = TRUE)
#' 
#' 
#' # specify wf as a character string:
#' dalr(y ~ ., data = train , thr = 0.3, wf = "rectangular", k = 50)
#' 
#' # use window function generating function:
#' rect <- rectangular(100)
#' dalr(y ~ ., data = train, thr = 0.3, wf = rect)
#' 
#' # specify own window function:
#' dalr(y ~ ., data = train, thr = 0.3, wf = function(x) exp(-10*x^2)) 
#' 
#' 
#' # generate test data set:
#' x1 <- runif(200, min = 0, max = 1)              
#' x2 <- runif(200, min = 0, max = 1)              
#' test <- data.frame(x1 = x1, x2 = x2)
#' 
#' pred <- predict(loc.fit, test)
#' 
#' prob <- test$x2/(test$x1 + test$x2)
#' y <- as.factor(sapply(prob, function(x) sample(0:1, size = 1, 
#'     prob = c(1-x,x))))
#' 
#' mean(y != pred$class)
#'
#' @keywords classif model multivariate
#'
#' @aliases dalr dalr.data.frame dalr.default dalr.formula dalr.matrix
#'
#' @export

dalr <- function(X, ...)
    UseMethod("dalr")
    
    

#' @rdname dalr
#' @method dalr formula
#'
#' @S3method dalr formula

dalr.formula <- function(formula, data, weights, ..., subset, na.action) {
    if (missing(data))
        data <- environment(formula)
    mf <- cl <- match.call()
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "etastart", "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    #mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    Y <- model.response(mf, "any")
    if (length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if (!is.null(nm))
            names(Y) <- nm
    }
    if ("contrasts" %in% names(mf)) contrasts <- mf$contrasts else contrasts <- NULL
    X <- if (!is.empty.model(mt))
        model.matrix(mt, mf, contrasts)
    else matrix(, NROW(Y), 0L)
    weights <- model.weights(mf)
    if (is.null(weights))
    	weights <- rep(1, nrow(X))
    res <- dalr.default(X, Y, weights = weights, ...)
    if ("model" %in% names(res))
        res$model <- mf
    res$na.action <- attr(mf, "na.action")
    cl[[1L]] <- as.name("dalr")
    res$call <- cl
    res$formula <- formula
    res$terms <- mt
    res$data <- data
    res$xlevels <- .getXlevels(mt, mf)
    res
}    



#' @rdname dalr
#' @method dalr data.frame
#'
#' @S3method dalr data.frame

dalr.data.frame <- function (X, ...) {
    res <- dalr(structure(data.matrix(X, rownames.force = TRUE), class = "matrix"), ...)
    cl <- match.call()
    cl[[1L]] <- as.name("dalr")
    res$call <- cl
    res
}
    
    

#' @rdname dalr
#' @method dalr matrix
#'
#' @S3method dalr matrix

dalr.matrix <- function (X, Y, weights = rep(1, nrow(X)), intercept = TRUE, ..., subset, na.action) {
    data <- data.frame(X, y = Y)
    if (!missing(subset)) {
    	weights <- weights[subset]
        X <- X[subset, , drop = FALSE]
        Y <- Y[subset]
    }
    if (missing(na.action)) {
        if (!is.null(naa <- getOption("na.action"))) {
            if(!is.function(naa))
            	na.action <- get(naa, mode = "function")
            else
                na.action <- naa
        } else {
            na.action <- na.fail
        }
    } 
    dfr <- na.action(structure(list(g = Y, w = weights, x = X), 
            class = "data.frame", row.names = rownames(X)))
    Y <- dfr$g
    X <- dfr$x
    weights <- dfr$w
    if (intercept) {
        X <- cbind(1, X)
        colnames(X)[1] <- "(Intercept)"
    }
    res <- dalr.default(X, Y, weights = weights, intercept = intercept, ...)
    if ("model" %in% names(res))
        res$model <- data[,c("y", names(data)[names(data) != "y"])]
    if (any(is.na(data))) res$na.action <- na.action else res$na.action <- NULL
    cl <- match.call()
    cl[[1L]] <- as.name("dalr")
    res$call <- cl
    res$data <- data
    res
}
# todo:
# default: subset = NULL???
# some parameters of glm do not make sense if a matrix is passed instead of formula/data
# model: Attribute for intercept???




#' @rdname dalr
#' @method dalr default
#'
#' @S3method dalr default

## todo: use method argument for different fit methods, e.g. glm.fit, ridge.fit

dalr.default <- function(X, Y, thr = 0.5, wf = c("biweight", "cauchy", "cosine", 
	"epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular"), 
    bw, k, nn.only = TRUE, itr = 3, intercept = TRUE, weights = rep(1, nrow(X)), ...) {
    
    dalr.fit <- function(X, Y, thr, wf, itr, intercept = TRUE, weights = rep(1, nrow(X)),
        offset = NULL, control = list(), model = TRUE, x = FALSE, 
        y = TRUE, contrasts = NULL, ...) {
        n <- nrow(X)
        pw <- ww <- list()
        control <- do.call("glm.control", control)
        if (any(weights < 0))
            stop("negative weights not allowed")
        if (!is.null(offset)) {
            if (length(offset) != NROW(Y))
                stop(gettextf("number of offsets is %d should equal %d (number of observations)",
                    length(offset), NROW(Y)), domain = NA)
        }
		if (all(weights == 0)) {
			stop("all observation weights are zero")
		}
        weights <- weights/sum(weights) * n     # rescale weights such that they sum up to n
        fit <- glm.fit(x = X, y = Y, weights = weights, family = binomial(), 
            offset = offset, control = control, intercept = intercept, ...)
        pw[[1]] <- fit$prior.weights
        ww[[1]] <- fit$weights
        names(pw[[1]]) <- names(ww[[1]]) <- rownames(X)
	    for (i in seq_len(itr)) {
            pw[[i+1]] <- wf(abs(fit$fitted.values - thr))
			pw[[i+1]] <- pw[[i+1]]/sum(pw[[i+1]]) * n     # rescale weights such that they sum up to n        
            if (all(pw[[i+1]] == 0))
            	stop("all 'weights' are zero")
        	fit <- glm.fit(x = X, y = Y, weights = pw[[i+1]], family = binomial(), 
                offset = offset, control = control, intercept = intercept, ...)
            ww[[i+1]] <- fit$weights
	        names(pw[[i+1]]) <- names(ww[[i+1]]) <- rownames(X)
       	}
    	#if (length(offset) && attr(mt, "intercept") > 0L) {
    	names(pw) <- names(ww) <- 0:itr
    	fit$prior.weights <- pw
    	fit$weights <- ww
    	if (length(offset) && intercept) {
        	fit$null.deviance <- glm.fit(x = X[, "(Intercept)", drop = FALSE], 
        	y = Y, weights = pw[[i+1]], offset = offset, family = binomial(), 
        	control = control, intercept = TRUE)$deviance
    	}
        if (model) 
            fit <- c(fit, list(model = NULL))
        fit <- c(fit, list(na.action = NULL))
        if (x) 
            fit$x <- X
        if (!y) 
            fit$y <- NULL    
        fit <- c(fit, list(call = NULL, formula = NULL, terms = NULL, 
            data = NULL, offset = offset, control = control, method = NULL, 
            contrasts = contrasts, xlevels = NULL)) # method erg‰nzen
        class(fit) <- c(fit$class, c("glm", "lm"))
        message("Warnings about non-integer #successes in a binomial glm are expected")
        return(fit)
    }
    if (is.null(dim(X)))
        stop("'X' is not a matrix")
    X <- as.matrix(X)
    if (any(!is.finite(X)))
        stop("infinite, NA or NaN values in 'X'")
    n <- nrow(X)
    if (missing(Y))
    	stop("'Y' is missing with no default")
    if (n != length(Y))
        stop("nrow(X) and length(Y) are different")
    if (!is.factor(Y))
    	warning("'Y' was coerced to a factor")
    g <- as.factor(Y)
    lev <- lev1 <- levels(g)
    counts <- as.vector(table(g))
	if (length(counts) > 2)
    	stop("number of classes larger than 2")
    if (any(counts == 0)) {
        empty <- lev[counts == 0]
        warning(sprintf(ngettext(length(empty), "group %s is empty",
            "groups %s are empty"), paste(empty, collapse = " ")),
            domain = NA)
        lev1 <- lev[counts > 0]
        g <- factor(g, levels = lev1)
        counts <- as.vector(table(g))
    }
    names(counts) <- lev1
    if (thr < 0 || thr > 1)
       stop("'thr' must be between 0 and 1")
	if (!missing(itr)) {
    	if (itr < 1)
       		stop("'itr' must be >= 1")
    	if (abs(itr - round(itr)) > .Machine$double.eps^0.5)
       		warning("'itr' is not a natural number and is rounded off")
	}
    if (is.character(wf)) {
    	m <- match.call(expand.dots = FALSE)
    	m$n <- n
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
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") + 1 > n)
    				stop("'k + 1' is larger than 'nrow(x)'")
    		} else {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") > n)
    				stop("'k' is larger than 'nrow(x)'")
    		}
    	}
    } else
    	stop("argument 'wf' has either to be a character or a function")
#    if (!is.null(attr(wf, "adaptive")) && attr(wf, "adaptive") && attr(wf, "name") == "rectangular" && attr(wf, "k") == n) { # todo
#    	itr <- 0
#    	warning("nonlocal solution")	
#    }   
	fit <- dalr.fit(X = X, Y = g, thr = thr, wf = wf, itr = itr, intercept = intercept, weights = weights, ...)
    fit.class <- class(fit)
    fit <- c(fit, list(counts = counts, N = n, lev = lev, thr = thr, itr = itr, wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive")))
    cl <- match.call()
    cl[[1]] <- as.name("dalr")
    fit$call <- cl
    class(fit) <- c("dalr", fit.class)
    return(fit)
}



# @param x A \code{dalr} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @method print dalr
#' @noRd
#'
#' @S3method print dalr

print.dalr <- function(x, ...) {
    NextMethod(x, ...)
    if(!is.null(attr(x$wf, "name"))) {
        cat("\nWindow function: ")
        cat(deparse(attr(x$wf, "name")), sep="\n")  
    } else {
        cat("\nWindow function:\n")
        cat(deparse(x$wf), sep="\n")  
    }
    if(!is.null(x$bw))
        cat("Bandwidth: ", x$bw, "\n")
    if(!is.null(x$k))
        cat("k: ", x$k, "\n")
    if(!is.null(x$nn.only))
        cat("Nearest neighbors only: ", x$nn.only, "\n")
    if(!is.null(x$adaptive))
        cat("Adaptive bandwidth: ", x$adaptive, "\n")
    cat("Threshold: ", x$thr, "\n")
    cat("Iterations: ", x$itr, "\n")
    invisible(x)
}



#' Obtains predicted class labels and posterior probabilities from a locally fitted logistic regression model.
#'
#'
#' @title Classify Multivariate Observations Based on Discriminant Adaptive Logistic Regression
#'
#' @param object An object of class \code{"dalr"} inheriting from \code{"glm"}.
#' @param newdata Optionally, a \code{data.frame} in which to look for variables with which to predict. 
#' If omitted, the fitted linear predictors are used.
#' @param ... Further arguments to be passed from or to other methods, especially to \code{\link{predict.glm}}???.
#' See also the Details section.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#' 
#' @seealso \code{\link{dalr}}, \code{\link{predict.glm}}, \code{\link{glm}}.
#'
#' @examples
#' # generate data set:
#' x1 <- runif(500, min = 0, max = 1)              
#' x2 <- runif(500, min = 0, max = 1)              
#' x <- data.frame(x1 = x1, x2 = x2)
#' prob <- x$x2/(x$x1 + x$x2)
#' y <- as.factor(sapply(prob, function(x) sample(0:1, size = 1, 
#'     prob = c(1-x,x))))
#' x <- data.frame(x, y = y)
#' 
#' # fit dalr on training set and predict on test set:
#' train <- sample(500, 300)
#' fit <- dalr(y ~ ., data = x, thr = 0.3, wf = "rectangular", bw = 100, 
#'     subset = train)
#' pred <- predict(fit, newdata = x[-train,])
#' mean(y[-train] != pred$class)
#'
#' @keywords classif model multivariate
#'
#' @method predict dalr
#' @rdname predict.dalr
#'
#' @S3method predict dalr

## todo: fix bug in predict.dalr if dalr.data.frame was used for fitting and newdata is given
## todo: NAs in newdata

predict.dalr <- function(object, newdata = NULL, ...) {
    post <- NextMethod(type = "response", ...)
    if (is.list(post)) {
        lev1 <- names(object$counts)
        if (missing(newdata))
        	l <- names(object$weights[[1]])
        else
        	l <- rownames(newdata)
        if (length(lev1) == 1) {
			group <- factor(rep(lev1, length(post$fit)), levels = object$lev)
        	post$fit <- as.matrix(post$fit)
        } else {
        	group <- ifelse(post$fit >= object$thr, object$lev[2], object$lev[1])
        	group <- factor(group, levels = object$lev)
        	post$fit <- matrix(c(1-post$fit, post$fit), ncol = 2)
        }
        colnames(post$fit) <- lev1
        rownames(post$fit) <- names(group) <- l
        post <- list(class = group, posterior = post$fit, se.fit = post$se.fit, residual.scale = post$residual.scale)
    } else {
        lev1 <- names(object$counts)
        if (missing(newdata))
        	l <- names(object$weights[[1]])
        else
        	l <- rownames(newdata)
        if (length(lev1) == 1) {
        	group <- factor(rep(lev1, length(post)), levels = object$lev)
        	post <- as.matrix(post)
        } else {
        	group <- ifelse(post >= object$thr, object$lev[2], object$lev[1])
        	group <- factor(group, levels = object$lev)  
        	post <- matrix(c(1-post, post), ncol = 2) 
        }
        colnames(post) <- lev1
        rownames(post) <- names(group) <- l
        post <- list(class = group, posterior = post)  
    }   
    return(post) 
}



#' @method weights dalr
#' @noRd
#'
#' @S3method weights dalr
#' @importFrom stats weights

weights.dalr <- function (object, ...) {
	object$prior.weights
}

