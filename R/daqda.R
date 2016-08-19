#' A local version of Quadratic Discriminant Analysis that puts increased emphasis on a good model fit near the decision boundary.
#'
#' The idea of Hand and Vinciotti (2003) to put increased weight on observations near the decision boundary is generalized to the multiclass case and applied to 
#' Quadratic Discriminant Analysis (QDA).
#' Since the decision boundary is not known in advance an iterative procedure is required.
#' First, an unweighted QDA is fitted to the data. 
#' Based on the differences between the two largest estimated posterior probabilities observation weights are calculated.
#' Then a weighted QDA (see \code{\link{wqda}}) is fitted using these weights. 
#' Calculation of weights and model fitting is done several times in turn. 
#' The number of iterations is determined by the \code{itr}-argument that defaults to 3.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{daqda}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{daqda}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#'
#' @title Discriminant Adaptive Quadratic Discriminant Analysis
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + \dots}, that is, the response
#' is the grouping \code{factor} and the right hand side specifies the (normally non-\code{factor})
#' discriminators.  
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param grouping (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying
#' the class membership for each observation.
#' @param wf A window function which is used to calculate weights that are introduced into 
#' the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#' For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param itr Number of iterations for model fitting, defaults to 3. See also the Details section.
#' @param weights Initial observation weights (defaults to a vector of 1s).
#' @param \dots Further arguments to be passed to \code{\link{wqda}}.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if NAs are found. The default action is first
#' the \code{na.action} setting of \code{\link{options}} and second \code{\link{na.fail}} if that is unset. 
#' An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#' variable. (NOTE: If given, this argument must be named.)
#'
#' If the predictor variables include factors, the formula interface must be used in order 
#' to get a correct model matrix.
#'
#' @return
#' An object of class \code{"daqda"} inheriting from \code{"wqda"}, a \code{list} containing the following components:
#'  \item{prior}{Weighted class prior probabilities.}
#'  \item{counts}{The number of observations per class.}
#'  \item{means}{Weighted estimates of class means.}
#'  \item{cov}{Weighted estimate of the pooled class covariance matrix.}
#'  \item{lev}{The class labels (the levels of \code{grouping}).}  
#'  \item{N}{The number of training observations.}
#'  \item{weights}{A list of length \code{itr + 1}. The initial observation weights (a vector of 1s if none were given) and the observation
#'	  weights calculated in the individual iterations. The weights are scaled such that they sum up to 1.}
#' 	\item{method}{The method used for scaling the pooled weighted covariance matrix.}
#'  \item{itr}{The number of iterations used.}
#'  \item{wf}{The window function used. Always a function, even if the input was a string.}
#'  \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#'	\item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#'  \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	 specified.) \code{TRUE} if only the \code{k} nearest neighbors receive a positive weight, \code{FALSE} otherwise.}
#'  \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the local density of data points, \code{FALSE} if the bandwidth
#'	  is fixed.}
#'  \item{call}{The (matched) function call.}
#'
#' @references Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' @seealso \code{\link{predict.daqda}}, \code{\link{wqda}} for a weighted version of Quadratic Discriminant Analysis and \code{\link{dalr}} for discriminant adaptive logistic regression.
#'
#' @family qda border_based
#'
#' @examples
#' fit <- daqda(Species ~ Sepal.Length + Sepal.Width, data = iris,
#'     wf = "gaussian", bw = 0.5)
#' pred <- predict(fit)
#' mean(pred$class != iris$Species)
#'
#' @keywords classif multivariate
#'
#' @aliases daqda daqda.data.frame daqda.default daqda.formula daqda.matrix
#'
#' @export

daqda <- function(x, ...)
	UseMethod("daqda")	



#' @rdname daqda
#' @export

daqda.formula <- function(formula, data, weights = rep(1, nrow(data)), ..., subset, na.action) {
    m <- match.call(expand.dots = FALSE)
    m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m$weights <- weights
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    weights <- m[,"(weights)"]
    grouping <- model.response(m)
    x <- model.matrix(Terms, m)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (xint > 0) 
        x <- x[, -xint, drop = FALSE]
    res <- daqda.default(x, grouping, weights = weights, ...)
    res$terms <- Terms
    cl <- match.call()
    cl[[1L]] <- as.name("daqda")
    res$call <- cl
    res$contrasts <- attr(x, "contrasts")
    res$xlevels <- .getXlevels(Terms, m)
    res$na.action <- attr(m, "na.action")
    res
}



#' @rdname daqda
#' @export

daqda.data.frame <- function (x, ...) {
    res <- daqda(structure(data.matrix(x, rownames.force = TRUE), class = "matrix"), ...)
    cl <- match.call()
    cl[[1L]] <- as.name("daqda")
    res$call <- cl
    res
}



#' @rdname daqda
#' @export

daqda.matrix <- function (x, grouping, weights = rep(1, nrow(x)), ..., subset, na.action = na.fail) {
    if (!missing(subset)) {
        weights <- weights[subset]
        x <- x[subset, , drop = FALSE]
        grouping <- grouping[subset]
    }
    if (missing(na.action)) {
        if (!is.null(naa <- getOption("na.action")))    # if options(na.action = NULL) the default of na.action comes into play
            if(!is.function(naa))
            	na.action <- get(naa, mode = "function")
            else
                na.action <- naa
    } 
    dfr <- na.action(structure(list(g = grouping, w = weights, x = x), 
            class = "data.frame", row.names = rownames(x)))
    grouping <- dfr$g
    x <- dfr$x
    weights <- dfr$w
    res <- daqda.default(x, grouping, weights = weights, ...)
    cl <- match.call()
    cl[[1L]] <- as.name("daqda")
    res$call <- cl
	res$na.action <- na.action
    res
}



#' @rdname daqda
#' @export

daqda.default <- function(x, grouping, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	"exponential", "gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only, itr = 3, weights, ...) {
	daqda.fit <- function(x, grouping, wf, itr, weights = rep(1, nrow(x)), ...) {
		w <- list()
		n <- nrow(x)
		w[[1]] <- weights/sum(weights) * n
		names(w[[1]]) <- rownames(x)
		res <- wqda.default(x = x, grouping = grouping, weights = weights, ...)
		for(i in seq_len(itr)) {
			# 1. prediction
			post <- predict(res)$posterior
			if (any(!is.finite(post)))
				stop("inifinite, NA or NaN values in 'post', may indiciate numerical problems due to small observation weights, please check your settings of 'bw', 'k' and 'wf'")
			# 2. calculate weights and fit model	
			spost <- apply(post, 1, sort, decreasing = TRUE)
			weights <- wf((spost[1,] - spost[2,]))    	# largest if both probabilities are equal
			if (all(weights == 0)) {
				stop("all observation weights are zero")
			}
			# 3. check if break
			freqs <- tapply(weights, grouping, sum)
# print(w[[i]])
# print(freqs)
			if (any(freqs == 0L, na.rm = TRUE))               	# classes where all weights are zero
				warning("for at least one class all weights are zero")
			if (sum(freqs > 0, na.rm = TRUE) <= 1L) {
				warning("training data from only one group, breaking out of iterative procedure")
				itr <- i - 1
				break
			} else {
				w[[i+1]] <- weights/sum(weights) * n
				names(w[[i+1]]) <- rownames(x)
				res <- wqda.default(x = x, grouping = grouping, weights = weights, ...)			
			}
		}
		names(w) <- seq_along(w) - 1
		res$weights <- w
		res$itr <- itr
		return(res)
	}
    if (is.null(dim(x))) 
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    #if (any(!is.finite(x))) 
    #    stop("infinite, NA or NaN values in 'x'")
    n <- nrow(x)
	#if (missing(grouping))
    #    stop('argument "grouping" is missing, with no default')
    #if (n != length(grouping)) 
    #    stop("nrow(x) and length(grouping) are different")
    #if (!is.factor(grouping))
    #    warning("'grouping' was coerced to a factor")
    #g <- as.factor(grouping)
    #lev <- lev1 <- levels(g)
    #counts <- as.vector(table(g))
    #if (any(counts == 0)) {
    #    empty <- lev[counts == 0]
    #    warning(sprintf(ngettext(length(empty), "group %s is empty", 
    #        "groups %s are empty"), paste(empty, collapse = ", ")), 
    #        domain = NA)
    #    lev1 <- lev[counts > 0]
    #    g <- factor(g, levels = lev1)
    #    counts <- as.vector(table(g))
    #}
    #ng <- nlevels(g)
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
    	stop("argument 'wf' has to be either a character or a function")
#	if (!is.null(attr(wf, "adaptive")) && attr(wf, "adaptive") && attr(wf, "name") == "rectangular" && attr(wf, "k") == n) { # todo
#    	itr <- 0
#    	warning("nonlocal solution")	
#    }   
	res <- daqda.fit(x = x, grouping = grouping, wf = wf, itr = itr, weights = weights, ...)
    res <- c(res, list(wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive")))
    cl <- match.call()
    cl[[1]] <- as.name("daqda")
    res$call <- cl
    class(res) <- c("daqda", "wqda")
    return(res)
}



# @param x A \code{daqda} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @noRd
#'
#' @export

print.daqda <- function (x, ...) {
    NextMethod(x, ...)
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



#' Classify multivariate observations in conjunction with \code{\link{daqda}}.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"daqda"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.daqda(x)} regardless of 
#' the class of the object. 
#'
#' @title Classify Multivariate Observations Based on Discriminant Adaptive Quadratic Discriminant Analysis
#'
#' @param object Object of class \code{"daqda"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if \code{object} has a
#' \code{formula}, a \code{data.frame} with columns of the same names as the
#' variables used. A vector will be interpreted as a row
#' vector. If \code{newdata} is missing, an attempt will be made to
#' retrieve the data used to fit the \code{daqda} object.
#' @param \dots Further arguments.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' @examples
#' ## comparison with qda:
#' library(MASS)
#' fit1 <- qda(Species ~ Sepal.Length + Sepal.Width, data = iris)
#' pred <- predict(fit1)
#' mean(pred$class != iris$Species)
#' 
#' fit2 <- daqda(Species ~ Sepal.Length + Sepal.Width, data = iris,
#'     wf = "gaussian", bw = 0.5)
#' pred <- predict(fit2)
#' mean(pred$class != iris$Species)
#' 
#' ## plot of decision boundary (maximum posterior probabilities):
#' grid <- expand.grid(Sepal.Length = seq(4,8,0.1), Sepal.Width = seq(2,5,0.1)) 
#' 
#' predgrid1 <- predict(fit1, newdata = grid)$posterior
#' predgrid2 <- predict(fit2, newdata = grid)$posterior
#' 
#' par(mfrow = c(1,2))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(apply(predgrid1, 1, max)), nrow = length(seq(4,8,0.1))))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(apply(predgrid2, 1, max)), nrow = length(seq(4,8,0.1))))
#' points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#'     cex = fit2$weights[[3]]*2, col = as.numeric(iris$Species))
#' 
#' @seealso \code{\link{daqda}}, \code{\link{wqda}}, \code{\link{dalr}}.
#'
#' @keywords classif multivariate
#' 
#' @rdname predict.daqda
#'
#' @export

predict.daqda <- function(object, newdata, ...) {
    if (!inherits(object, "daqda")) 
        stop("object not of class \"daqda\"")
    NextMethod(object, newdata, ...)
}



#' @noRd
#'
#' @importFrom stats weights
#' @export

weights.daqda <- function (object, ...) {
	if (!inherits(object, "daqda"))
		stop("object not of class \"daqda\"")
	NextMethod(object, ...)
}


