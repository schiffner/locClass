#' A local version of multinomial regression from package \pkg{nnet} that puts increased emphasis on a good model fit near 
#' the decision boundary.
#'
#' The idea of Hand and Vinciotti (2003) to put increased weight on observations near the decision boundary is generalized to the multiclass case and applied to 
#' multinomial regression.
#' Since the decision boundary is not known in advance an iterative procedure is required.
#' First, an unweighted multinomial regression model is fitted to the data. 
#' Based on the differences between the two largest estimated posterior probabilities observation weights are calculated.
#' Then a weighted multinomial regression model (see \code{\link[nnet]{multinom}}) is fitted using these weights.
#' Calculation of weights and model fitting is done several times in turn. 
#' The number of iterations is determined by the \code{itr}-argument that defaults to 3.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{damultinom}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{damultinom}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' \code{damultinom} calls \code{\link{dannet}}. The variables on the rhs of
#' the formula should be roughly scaled to [0,1] or the fit will be slow
#' or may not converge at all.
#'
#' Observation weights that reflect the importance of training observations for the fit
#' at a particular test observation are calculated internally in \code{damultinom}.
#' For this reason not all types of response in \code{formula} are allowed and \code{damultinom} does not take all arguments 
#' that can be passed to \code{\link[nnet]{multinom}}.
#' As response in \code{formula} factors and matrices are allowed. If \code{censored = FALSE} only zero-one class indicator matrices are 
#' allowed.
#' Argument \code{summ} that specifies a method to summarize rows of the model matrix is missing since this requires adjustment of the case weights.
#'
#'
#' @title Discriminant Adaptive Multinomial Log-linear Models
#'
#' @param formula A formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response should be a factor or a
#'  matrix with K columns. If \code{censored = FALSE} this is required to be a 
#' zero-one indicator matrix.
# , which will be interpreted as counts for each of K classes.
#' A log-linear model is fitted, with coefficients zero for the first
#' class. An offset can be included: it should be a numeric matrix with K columns
#' if the response is either a matrix with K columns or 
#' a factor with K > 2
#' classes, or a numeric vector for a response factor with 2 levels.
#' See the documentation of \code{\link{formula}()} for other details.
#' @param data An optional data frame in which to interpret the variables occurring
#'   in \code{formula}.
#' @param weights Initial case weights in fitting (defaults to a vector of 1s).
#' @param subset Expression saying which subset of the rows of the data should be used
#'   in the fit. All observations are included by default.
#' @param na.action A function to filter missing data.
#' @param contrasts A list of contrasts to be used for some or all of
#'   the factors appearing as variables in the model formula.
#' @param Hess Logical for whether the Hessian (the observed/expected information matrix)
#'   should be returned.
# @parm summ Integer; if non-zero summarize by deleting duplicate rows and adjust weights.
# Methods 1 and 2 differ in speed (2 uses \code{C}); method 3 also combines rows
# with the same X and different Y, which changes the baseline for the
# deviance.????
#' @param censored Logical. If the response is a matrix with \eqn{K > 2} classes, interpret the entries as one for possible classes, zero 
#'   for impossible classes. Defaults to \code{FALSE}.
#' @param model Logical. If \code{TRUE}, the model frame is saved as component \code{model}
#'  of the returned object.
#' @param \dots Additional arguments for \code{\link{dannet}}, including the window function and bandwidth
#'  parameters used to generate observation weights:
#' \describe{
#' 	\item{\code{wf}}{A window function which is used to calculate weights that are introduced into 
#' 		the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#' 		For details see the documentation for \code{\link[=biweight]{wfs}}.}
#' 	\item{\code{bw}}{(Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)}
#'  \item{\code{k}}{(Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. 
#'		(See \code{\link[=biweight]{wfs}}.)}
#' 	\item{\code{nn.only}}{(Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' 		only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)}
#' 	\item{\code{itr}}{Number of iterations for model fitting, defaults to 3. See also the Details section.}
#' }
#'
#' @return A \code{"damultinom"} object (that inherits from \code{"\link{dannet}"} and \code{"\link[nnet]{nnet}"}) with additional components:
#' \item{deviance}{The residual deviance, compared to the full saturated model (that
#' explains individual observations exactly).  Also, minus twice log-likelihood.}
#' \item{edf}{The (effective) number of degrees of freedom used by the model.}
#' \item{AIC}{The AIC for this fit.}
#' \item{Hessian}{(if \code{Hess} is true).}
#' \item{model}{(if \code{model} is true).}
#'
#' @references Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#'  Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#'  Venables, W. N. and Ripley, B. D. (2002)
#'  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
#'
#' @seealso \code{\link[nnet]{multinom}}, \code{\link{dannet}}, \code{\link[nnet]{nnet}}, \code{\link{predict.damultinom}}.
#'
#' @family multinom border_based
#'
#' @examples
#' fit <- damultinom(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5, Hess=TRUE)
#' pred <- predict(fit)
#' mean(pred$class != iris$Species)
#'
#' fit <- damultinom(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5, weights=1:nrow(iris), trace=FALSE)
#' pred <- predict(fit)
#' mean(pred$class != iris$Species)
#' 
#' @concept multiple logistic
#' @keywords classif neural models
#'
#' @aliases damultinom
#'
#' @import nnet
#'
#' @export


damultinom <- function(formula, data, weights, 
	subset, na.action, contrasts = NULL, Hess = FALSE, censored = FALSE, model = FALSE, ...) {
    class.ind <- function(cl) {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
#    summ2 <- function(X, Y)
#    {
#        X <- as.matrix(X)
#        Y <- as.matrix(Y)
#        n <- nrow(X)
#        p <- ncol(X)
#        q <- ncol(Y)
#        Z <- t(cbind(X, Y))
#        storage.mode(Z) <- "double"
#        z <- .C(VR_summ2,
#                as.integer(n),
#                as.integer(p),
#                as.integer(q),
#                Z = Z,
#                na = integer(1L))
#        Za <- t(z$Z[, 1L:z$na, drop = FALSE])
#        list(X = Za[, 1L:p, drop = FALSE], Y = Za[, p + 1L:q])
#    }
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$Hess <- m$contrasts <- m$censored <- m$model <- m$... <- NULL
#    m$summ <- m$Hess <- m$contrasts <- m$censored <- m$model <- m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
#print(m)
    Terms <- attr(m, "terms")
    X <- model.matrix(Terms, m, contrasts)
#print(X)
    cons <- attr(X, "contrasts")
    Xr <- qr(X)$rank
    Y <- model.response(m)
#print(Y)
    if (!is.matrix(Y)) {
    	if (!is.factor(Y))
	    	warning("response variable was coerced to a factor")	
    	Y <- as.factor(Y)
    }
    w <- model.weights(m)	## initial weights
    if (length(w) == 0L) {		## no weights given
    	if (is.matrix(Y)) 
       		w <- rep(1, dim(Y)[1L]) ## if no initial weights given, they default to 1, respective to adjustments 
       	else w <- rep(1, length(Y))
    } else {
	    if (any(w < 0))
	        stop("weights have to be larger or equal to zero")
    	if (all(w == 0))
        	stop("all weights are zero")
    }
    lev <- lev1 <- levels(Y)
    if (is.factor(Y)) {
    	counts <- table(Y)
    	if (any(counts == 0L)) {
        	empty <- lev[counts == 0L]
        	warning(sprintf(ngettext(length(empty),
                                 "group %s is empty",
                                 "groups %s are empty"),
                        paste(sQuote(empty), collapse=" ")), domain = NA)
        	Y <- factor(Y, levels=lev[counts > 0L])
        	lev <- lev[counts > 0L]
    	}
    	if (length(lev) < 2L)
        	stop("need two or more classes to fit a damultinom model")
    	if (length(lev) == 2L) 
    		Y <- as.vector(unclass(Y)) - 1	## vector
    	else Y <- class.ind(Y)  			## matrix
    }
#    if(summ == 1) {
#        Z <- cbind(X, Y)
#        z1 <- cumprod(apply(Z, 2L, max)+1)
#        Z1 <- apply(Z, 1L, function(x) sum(z1*x))
#        oZ <- order(Z1)
#        Z2 <- !duplicated(Z1[oZ])
#        oX <- (seq_along(Z1)[oZ])[Z2]
#        X <- X[oX, , drop=FALSE]
#        Y <- if(is.matrix(Y)) Y[oX, , drop=FALSE] else Y[oX]
#        w <- diff(c(0,cumsum(w))[c(Z2,TRUE)])
#        print(dim(X))
#    }
#    if(summ == 2) {
#        Z <- summ2(cbind(X, Y), w)
#        X <- Z$X[, 1L:ncol(X)]
#        Y <- Z$X[, ncol(X) + 1L:ncol(Y), drop = FALSE]
#        w <- Z$Y
#        print(dim(X))
#    }
#    if(summ == 3) {
#        Z <- summ2(X, Y*w)
#        X <- Z$X
#        Y <- Z$Y[, 1L:ncol(Y), drop = FALSE]
#        w <- rep(1, nrow(X))
#        print(dim(X))
#    }
    offset <- model.offset(m)
    r <- ncol(X)
    if (is.matrix(Y)) {
        # 3 or more response levels or direct matrix spec.
        p <- ncol(Y)
        sY <- Y %*% rep(1, p)   # row sums
        if (any(sY == 0)) 
        	stop("some case has no observations")
        if (!censored) {
        	if (any(Y != 0 & Y != 1))
        		stop("only 0-1 indicator response matrix allowed")
        	if (any(sY != 1))
        		stop("only 0-1 indicator response matrix allowed")
#            Y <- Y / matrix(sY, nrow(Y), p)  
#            w <- w*sY
        }
        if (length(offset) > 1L) {
            if(ncol(offset) !=  p) 
            	stop("ncol(offset) is wrong")
            mask <- c(rep(FALSE, r+1L+p),
                      rep(c(FALSE, rep(TRUE, r), rep(FALSE, p)), p-1L) )
            X <- cbind(X, offset)
            Wts <- as.vector(rbind(matrix(0, r+1L, p), diag(p)))
           	fit <- dannet.default(x = X, y = Y, weights = w, Wts=Wts, mask=mask, size=0, skip=TRUE,
                               softmax=TRUE, censored=censored, rang=0, ...)
        } else {
            mask <- c(rep(FALSE, r+1L), rep(c(FALSE, rep(TRUE, r)), p-1L) )
           	fit <- dannet.default(x = X, y = Y, weights = w, mask=mask, size=0, skip=TRUE,
                               softmax=TRUE, censored=censored, rang=0, ...)
        }
    } else { # Y 0-1-vector, 2 response levels
        if (length(offset) <= 1L) {
            mask <- c(FALSE, rep(TRUE, r))
            fit <- dannet.default(x = X, y = Y, weights = w, mask=mask, size=0, skip=TRUE,
                                entropy=TRUE, rang=0, ...)
        } else {
            mask <- c(FALSE, rep(TRUE, r), FALSE)
            Wts <- c(rep(0, r+1L), 1)
            X <- cbind(X, offset)
            fit <- dannet.default(x = X, y = Y, weights = w, Wts=Wts, mask=mask, size=0, skip=TRUE,
                                entropy=TRUE, rang=0, ...)
        }
    }
    fit$formula <- as.vector(attr(Terms, "formula"))
    fit$terms <- Terms
    fit$call <- call
    fit$lev1 <- lev1
    fit$lev <- lev
    fit$deviance <- 2 * fit$value
    fit$rank <- Xr
    edf <- ifelse(length(lev) == 2L, 1, length(lev)-1)*Xr
    if(is.matrix(Y)) {
        edf <- (ncol(Y)-1)*Xr
        if(length(dn <- colnames(Y)) > 0) fit$lab <- dn
        else fit$lab <- 1L:ncol(Y)
    }
    fit$coefnames <- colnames(X)
    fit$vcoefnames <- fit$coefnames[1L:r] # remove offset cols
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    fit$edf <- edf
    fit$AIC <- fit$deviance + 2 * edf
    if(model) fit$model <- m
    class(fit) <- c("damultinom", "multinom", "nnet")
    if(Hess) {
	    fit$w <- fit$weights
    	fit$weights <- fit$w[[length(fit$w)]]
    	fit$Hessian <- nnet:::multinomHess(fit, X)
    	fit$weights <- fit$w
    	fit$w <- NULL
    }
    fit
}



# @param x A \code{damultinom} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @noRd
#'
#' @export

print.damultinom <- function (x, ...) {
    if (!inherits(x, "damultinom")) 
        stop("not a legitimate \"damultinom\" object")
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



#' Predict new examples by a trained discriminant adaptive multinomial model.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"damultinom"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.damultinom(x)} regardless of 
#' the class of the object. 
#'
#' In contrast to \code{\link[nnet]{predict.multinom}} \code{predict.damultinom} does not have
#' a type argument. Both, the predicted posterior probabilities and the class labels, are returned.
#'
#' @title Predict New Examples by a Trained Discriminant Adaptive Multinomial Model
#'
#' @param object Object of class \code{"damultinom"}.
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
#' Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
#'
#' @seealso \code{\link{damultinom}}, \code{\link{dannet}}, \code{\link[nnet]{multinom}}, \code{\link[nnet]{nnet}}.
#'
#' @examples
#' fit <- damultinom(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5, Hess=TRUE)
#' pred <- predict(fit)
#' mean(pred$class != iris$Species)
#'
#' fit <- damultinom(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5, weights=1:nrow(iris), trace=FALSE)
#' pred <- predict(fit)
#' mean(pred$class != iris$Species)
#'
#' @keywords classif neural model
#' 
#' @rdname predict.damultinom
#'
#' @export

predict.damultinom <- function(object, newdata, ...) {
    if (!inherits(object, "damultinom")) 
        stop("object not of class \"damultinom\"")
    posterior <- NextMethod(object, newdata, type = "probs", ...)
    if (length(object$lev) == 2) {
    	posterior <- cbind(1 - posterior, posterior) # posterior is probability for 2nd factor level
    	colnames(posterior) <- object$lev 
    } else if (!is.matrix(posterior)) {
    	posterior <- matrix(posterior, ncol = length(posterior))
    	colnames(posterior) <- object$lev
    	if (missing(newdata))
    		rownames(posterior) <- names(fit$weights[[1]])
    	else
    		rownames(posterior) <- row.names(newdata)
    }
	gr <- factor(object$lev[max.col(posterior)], levels = object$lev1) ### y matrix zulassen? klappt das so?
	names(gr) <- rownames(posterior)
	return(list(class = gr, posterior = posterior))
}



#' @noRd
#'
#' @importFrom stats weights
#' @export

weights.damultinom <- function (object, ...) {
    if (!inherits(object, "damultinom")) 
        stop("object not of class \"damultinom\"")
	NextMethod(object, ...)
}

