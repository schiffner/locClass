#' A localized version of multinomial regression.
#'
#' This is a localized version of multinomial regression where a multinomial regression model is fitted for 
#' each test observation based on the training data near the trial point. It
#' is based on the function \code{\link[nnet]{multinom}} from package \pkg{nnet}.
#' \code{\link{osnnet}} is called internally.
#'
# The name of the window function (\code{wf}) can be specified as a character string.
# In this case the window function is generated internally in \code{\link{predict.osmultinom}}. Currently
# supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
# \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
# \code{"triangular"}.
#
# Moreover, it is possible to generate the window functions mentioned above in advance 
# (see \code{\link[=biweight]{wfs}}) and pass them to \code{osmultinom}. 
#
# Any other function implementing a window function can also be used as \code{wf} argument.
# This allows the user to try own window functions.
# See help on \code{\link[=biweight]{wfs}} for details.
#
# If the predictor variables include factors, the formula interface must be used in order 
# to get a correct model matrix.
#'
#' In \code{osmultinom} no fitting is done. In the prediction step an individual model for every test
#' observation is fitted.
#' Observation weights that reflect the importance of training observations for the fit
#' at a particular test observation are calculated internally in \code{\link{predict.osmultinom}}.
#' For this reason not all types of response in \code{formula} are allowed and \code{osmultinom} does not take all arguments 
#' that can be passed to \code{\link[nnet]{multinom}}.
#' As response in \code{formula} factors and matrices are allowed. If \code{censored = FALSE} only zero-one class indicator matrices are 
#' allowed.
#' Argument \code{weights} is missing since observation weights are calculated internally in \code{\link{predict.osmultinom}}.
#' \code{summ} that specifies a method to summarize rows of the model matrix is missing since this requires adjustment of the case weights.
#' Also \code{Hess} is not supported.
#'
#' @title Observation Specific Multinomial Log-linear Models
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
# @param wf A window function which is used to calculate weights that are introduced into 
#   the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#   For details see the documentation for \code{\link[=biweight]{wfs}}.
# @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
# @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. 
#   (See \code{\link[=biweight]{wfs}}.)
# @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
# only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if \code{NA}s are found. The default action is for the
#'   procedure to fail. 
#'   An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#'   variable. (NOTE: If given, this argument must be named.)
#' @param contrasts A list of contrasts to be used for some or all of the factors appearing as variables in the model formula.
#' @param censored Logical. If the response is a matrix with \eqn{K > 2} classes, interpret the entries as one for possible classes, zero 
#'   for impossible classes. Defaults to \code{FALSE}.
#' @param model Logical. If \code{TRUE}, the model frame is saved as component \code{model} of the returned object.
# is this necessary, since the model.matrix is returned as component X?
#' @param \dots Additional arguments for \code{\link{osnnet}}, including the window function and bandwidth
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
# @param Hess ...
# @param summ ...
#'
#' @return An object of class \code{osmultinom}, inheriting from \code{"osnnet"}, a \code{list} containing the following components:
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
#' @seealso \code{\link{predict.osmultinom}}, \code{\link{osnnet}}, \code{\link[nnet]{nnet}}.
#'
#' @family observation_specific multinom
#'
#' @examples
#' fit <- osmultinom(Species ~ Sepal.Length + Sepal.Width, data = iris,
#'     wf = "gaussian", bw = 0.5)
#' pred <- predict(fit)
#' mean(pred != iris$Species)
#'
#' @keywords neural
#'
#' @aliases osmultinom
#'
#' @export
#'
#' @import nnet

osmultinom <- function (formula, data, subset, na.action, contrasts = NULL, censored = FALSE, model = FALSE, 
    ...) 
{
    class.ind <- function(cl) {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    # summ2 <- function(X, Y) {
        # X <- as.matrix(X)
        # Y <- as.matrix(Y)
        # n <- nrow(X)
        # p <- ncol(X)
        # q <- ncol(Y)
        # Z <- t(cbind(X, Y))
        # storage.mode(Z) <- "double"
        # z <- .C(VR_summ2, as.integer(n), as.integer(p), as.integer(q), 
            # Z = Z, na = integer(1L))
        # Za <- t(z$Z[, 1L:z$na, drop = FALSE])
        # list(X = Za[, 1L:p, drop = FALSE], Y = Za[, p + 1L:q])
    # }
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    #m$summ <- m$Hess <- m$contrasts <- m$censored <- m$model <- m$... <- NULL
    m$contrasts <- m$censored <- m$model <- m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    X <- model.matrix(Terms, m, contrasts)
    cons <- attr(X, "contrasts")
  	Xr <- qr(X)$rank
    Y <- model.response(m)
    if (!is.matrix(Y)) 
        Y <- as.factor(Y)
    #w <- model.weights(m)
    #if (length(w) == 0L) 
    #    if (is.matrix(Y)) 
    #        w <- rep(1, dim(Y)[1L])
    #    else w <- rep(1, length(Y))
    lev <- lev1 <- levels(Y)
    if (is.factor(Y)) {
        counts <- table(Y)
        if (any(counts == 0L)) {
            empty <- lev[counts == 0L]
            warning(sprintf(ngettext(length(empty), "group %s is empty", 
                "groups %s are empty"), paste(empty, 
                collapse = " ")), domain = NA)
            lev1 <- lev[counts > 0L]
            Y <- factor(Y, levels = lev1)
        }
        if (length(lev1) < 2L) 
            stop("need two or more classes to fit a multinom model")
        if (length(lev1) == 2L) 
            Y <- as.vector(unclass(Y)) - 1
        else Y <- class.ind(Y)
    }
    # if (summ == 1) {
        # Z <- cbind(X, Y)
        # z1 <- cumprod(apply(Z, 2L, max) + 1)
        # Z1 <- apply(Z, 1L, function(x) sum(z1 * x))
        # oZ <- order(Z1)
        # Z2 <- !duplicated(Z1[oZ])
        # oX <- (seq_along(Z1)[oZ])[Z2]
        # X <- X[oX, , drop = FALSE]
        # Y <- if (is.matrix(Y)) 
            # Y[oX, , drop = FALSE]
        # else Y[oX]
        # w <- diff(c(0, cumsum(w))[c(Z2, TRUE)])
        # print(dim(X))
    # }
    # if (summ == 2) {
        # Z <- summ2(cbind(X, Y), w)
        # X <- Z$X[, 1L:ncol(X)]
        # Y <- Z$X[, ncol(X) + 1L:ncol(Y), drop = FALSE]
        # w <- Z$Y
        # print(dim(X))
    # }
    # if (summ == 3) {
        # Z <- summ2(X, Y * w)
        # X <- Z$X
        # Y <- Z$Y[, 1L:ncol(Y), drop = FALSE]
        # w <- rep(1, nrow(X))
        # print(dim(X))
    # }
    offset <- model.offset(m)
    r <- ncol(X)
    if (is.matrix(Y)) {
        p <- ncol(Y)
        sY <- Y %*% rep(1, p)
        if (any(sY == 0)) 
            stop("some case has no observations")
        if (!censored) {
        	if (any(Y != 0 & Y != 1))
        		stop("only 0-1 indicator response matrix allowed")
        	if (any(sY != 1))
        		stop("only 0-1 indicator response matrix allowed")
            #Y <- Y/matrix(sY, nrow(Y), p)
            #w <- w * sY
        }
        if (length(offset) > 1L) {
            if (ncol(offset) != p) 
                stop("ncol(offset) is wrong")
            net$mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
                r), rep(FALSE, p)), p - 1L))
            X <- cbind(X, offset)
            Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
            net <- osnnet.default(X, Y, Wts = Wts, mask = mask, 
                size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
                rang = 0, ...)
        }
        else {
            mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
                r)), p - 1L))
            net <- osnnet.default(X, Y, mask = mask, size = 0, 
                skip = TRUE, softmax = TRUE, censored = censored, 
                rang = 0, ...)
        }
    }
    else {
        if (length(offset) <= 1L) {
            mask <- c(FALSE, rep(TRUE, r))
            net <- osnnet.default(X, Y, mask = mask, size = 0, 
                skip = TRUE, entropy = TRUE, rang = 0, ...)
        }
        else {
            mask <- c(FALSE, rep(TRUE, r), FALSE)
            Wts <- c(rep(0, r + 1L), 1)
            X <- cbind(X, offset)
            net <- osnnet.default(X, Y, Wts = Wts, mask = mask, 
                size = 0, skip = TRUE, entropy = TRUE, rang = 0, 
                ...)
        }
    }
    # wird so wf, bw etc. vernünftig weitergegeben?
    # osnnet.default <- function (x, y, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	# "exponential", "gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only = TRUE, 
	# size, Wts, mask = rep(TRUE, length(wts)), linout = FALSE, entropy = FALSE, softmax = FALSE, 
	# censored = FALSE, skip = FALSE, rang = 0.7, decay = 0, maxit = 100, 
    # trace = TRUE, MaxNWts = 1000, abstol = 1e-04, reltol = 1e-08, ...) {
    net$formula <- as.vector(attr(Terms, "formula"))
    net$terms <- Terms
    net$call <- call
    #net$weights <- w
    net$lev <- lev
    net$lev1 <- lev1
    #net$deviance <- 2 * fit$value
    net$rank <- Xr
    edf <- ifelse(length(lev1) == 2L, 1, length(lev1) - 1) * Xr
    if (is.matrix(Y)) {
        edf <- (ncol(Y) - 1) * Xr
        if (length(dn <- colnames(Y)) > 0) 
            net$lab <- dn
        else net$lab <- 1L:ncol(Y)###????
    }
    net$coefnames <- colnames(X)
    net$vcoefnames <- net$coefnames[1L:r]
    net$na.action <- attr(m, "na.action")
    net$contrasts <- cons
    net$xlevels <- .getXlevels(Terms, m)
    net$edf <- edf
    #fit$AIC <- fit$deviance + 2 * edf
    if (model) 
        net$model <- m
    class(net) <- c("osmultinom", "osnnet")
    # if (Hess) 
        # fit$Hessian <- multinomHess(fit, X)
    net
}


# nnet:::print.multinom
# function (x, ...) 
# {
    # if (!is.null(cl <- x$call)) {
        # cat("Call:\n")
        # dput(cl, control = NULL)
    # }
    # cat("\nCoefficients:\n")
    # print(coef(x), ...)
    # cat("\nResidual Deviance:", format(x$deviance), "\n")
    # cat("AIC:", format(x$AIC), "\n")
    # invisible(x)
# }

# @param x An \code{osmultinom} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @noRd
#'
#' @export

print.osmultinom <- function(x, ...) {
    if (!inherits(x, "osmultinom")) 
        stop("not a legitimate \"osmultinom\" object")
	NextMethod(x, ...)
}



#' Predict new examples in conjunction with \code{\link{osmultinom}}.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"osmultinom"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.osmultinom(x)} regardless of 
#' the class of the object. 
#' 
# If the response in \code{formula} is a factor, appropriate classification
# networks are constructed; these have one output and entropy fit if the
# number of levels is two, and a number of outputs equal to the number
# of classes and a softmax output stage for more levels.  If the
# response is not a factor, it is passed on unchanged to \code{predict.osnnet}.
#
# Optimization is done via the BFGS method of \code{\link{optim}}.
#'
#' @title Predict New Examples Based on Observation Specific Multinomial Log-linear Models
#'
#' @param object Object of class \code{"osmultinom"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if \code{object} has a
#' \code{formula}, a \code{data.frame} with columns of the same names as the
#' variables used. A vector will be interpreted as a row
#' vector. If \code{newdata} is missing, an attempt will be made to
#' retrieve the data used in \code{\link{osmultinom}}.
#' @param type Type of output.
#' @param \dots Arguments passed to or from other methods.
#'
#' @return If \code{type = "class"}, a \code{factor} containing the predicted class labels;
#'   if \code{type = "probs"}, a \code{matrix} of class posterior probabilities. In case of a binary classification
#'	 problem the matrix contains only the posteriors for the second factor level.
#'
#' @seealso \code{\link{osmultinom}}, \code{\link{osnnet}}, \code{\link{predict.osnnet}}, \code{\link[nnet]{predict.nnet}}, \code{\link[nnet]{nnet}}.
#'
#' @family observation_specific multinom
#'
#' @examples
#'     fit <- osmultinom(Species ~ Sepal.Length + Sepal.Width, data = iris,
#'                       wf = "gaussian", bw = 0.5)
#'     pred <- predict(fit)
#'     mean(pred != iris$Species)
#' 
#' @keywords neural
#' 
#' @rdname predict.osmultinom
#'
#' @export
#'
#' @useDynLib locClass

# function (object, newdata, type = c("class", "probs"), ...) 
# {
    # if (!inherits(object, "multinom")) 
        # stop("not a \"multinom\" fit")
    # type <- match.arg(type)
    # if (missing(newdata)) 
        # Y <- fitted(object)
    # else {
        # newdata <- as.data.frame(newdata)
        # rn <- row.names(newdata)
        # Terms <- delete.response(object$terms)
        # m <- model.frame(Terms, newdata, na.action = na.omit, 
            # xlev = object$xlevels)
        # if (!is.null(cl <- attr(Terms, "dataClasses"))) 
            # .checkMFClasses(cl, m)
        # keep <- match(row.names(m), rn)
        # X <- model.matrix(Terms, m, contrasts = object$contrasts)
        # Y1 <- predict.nnet(object, X)
        # Y <- matrix(NA, nrow(newdata), ncol(Y1), dimnames = list(rn, 
            # colnames(Y1)))
        # Y[keep, ] <- Y1
    # }
    # switch(type, class = {
        # if (length(object$lev) > 2L) Y <- factor(max.col(Y), 
            # levels = seq_along(object$lev), labels = object$lev)
        # if (length(object$lev) == 2L) Y <- factor(1 + (Y > 0.5), 
            # levels = 1L:2L, labels = object$lev)
        # if (length(object$lev) == 0L) Y <- factor(max.col(Y), 
            # levels = seq_along(object$lab), labels = object$lab)
    # }, probs = {
    # })
    # drop(Y)
# }


predict.osmultinom <- function(object, newdata, type = c("class", "probs"), ...) {    
	if (!inherits(object, "osmultinom")) 
        stop("object not of class \"osmultinom\"")
    type <- match.arg(type)
    if (missing(newdata)) {
		x <- object$x
		rn <- rownames(x)
		keep <- seq_along(rn)
   		# if (!is.null(Terms <- object$terms)) { ## same as inherits(object, "osnnet.formula")?
   			# newdata <- model.frame(object)
        	# x <- model.matrix(delete.response(Terms), newdata, contrasts = object$contrasts)
       		# xint <- match("(Intercept)", colnames(x), nomatch = 0L)
        	# if (xint > 0L) 
            	# x <- x[, -xint, drop = FALSE]
			# rn <- rownames(x)
   		# } else {
            # newdata <- eval.parent(object$call$x)
        	# if (is.null(dim(newdata))) 
            	# dim(newdata) <- c(1L, length(newdata))
        	# x <- as.matrix(newdata)
            # if (any(is.na(x))) 
                # stop("missing values in 'x'")
            # rn <- rownames(x)
   		# }
    } else {
        newdata <- as.data.frame(newdata)
        Terms <- delete.response(object$terms)
        rn <- row.names(newdata)
        m <- model.frame(Terms, newdata, na.action = na.omit, 
            xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses"))) 
            .checkMFClasses(cl, m)
        keep <- match(row.names(m), rn)
        x <- model.matrix(Terms, m, contrasts = object$contrasts)
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
#	print(head(Z))
#    if (!is.null(Terms <- object$terms)) {
#        if (missing(newdata)) 
#            newdata <- model.frame(object)
#        else {
#            newdata <- model.frame(as.formula(delete.response(Terms)), 
#                newdata, na.action = function(x) x, xlev = object$xlevels)
#        }
#        x <- model.matrix(delete.response(Terms), newdata, contrasts = object$contrasts)
#        xint <- match("(Intercept)", colnames(x), nomatch = 0)
#        if (xint > 0) 
#            x <- x[, -xint, drop = FALSE]
#    }
#    else {
#        if (missing(newdata)) {
#            if (!is.null(sub <- object$call$subset)) 
#                newdataa <- eval.parent(parse(text = paste(deparse(object$call$x, 
#                  backtick = TRUE), "[", deparse(sub, backtick = TRUE), 
#                  ",]")))
#            else newdata <- eval.parent(object$call$x)
#            if (!is.null(nas <- object$call$na.action)) 
#                newdata <- eval(call(nas, newdata))
#        }
#        if (is.null(dim(newdata))) 
#            dim(newdata) <- c(1, length(newdata))
#        x <- as.matrix(newdata)
#    }
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
	res <- .Call("predosnnet", 
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
	#dimnames(res) <- list(rn, colnames(object$y))
   	
# print(keep)
    z <- matrix(NA, length(rn), ncol(res[[1]]), dimnames = list(rn, colnames(object$y)))
    z[keep, ] <- res[[1]]
    
    switch(type, class = {
        if (length(object$lev1) > 2L) z <- factor(object$lev1[max.col(z)], levels = object$lev)
        if (length(object$lev1) == 2L) z <- factor(object$lev1[1 + (z > 0.5)], levels = object$lev)###
        if (length(object$lev1) == 0L) z <- factor(max.col(z), 
            levels = seq_along(object$lab), labels = object$lab)
        names(z) <- rn
        z
    }, probs = {
    })
    #drop(z) ###?
    z
    # switch(type, raw = z,
           # class = {
               # if(is.null(object$lev)) stop("inappropriate fit for class")
               # if(ncol(z) > 1L) object$lev[max.col(z)]
               # else object$lev[1L + (z > 0.5)]
           # }) ## factor???
}