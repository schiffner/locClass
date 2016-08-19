#' Combine Model-based Recursive Partitioning with Multinomial Logistic Regression.
#'
#' This page lists all ingredients to combine Multinomial Regression with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{multinomModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link[nnet]{multinom}} model.
#'
#' Moreover, methods for \code{\link[nnet]{multinom}} and \code{multinomModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with Multinomial Logistic Regression
#'
#' @param object An object of class "multinomModel" and "multinom", respectively.
#' @param x An object of class "multinom".
#' @param weights A vector of observation weights.
#' @param out Should class labels or posterior probabilities be returned?
#' @param newdata A \code{data.frame} of cases to be classified.
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "multinomModel" object. \cr
#' \code{deviance}: The value of the deviance extracted from \code{object}. \cr
#' \code{estfun}: The empirical estimating (or score) function, i.e. the derivatives of the log-likelihood with respect
#'   to the parameters, evaluated at the training data. \cr
#' \code{predict}: Either a vector of predicted class labels or a matrix of class posterior probabilities.
#'
#' @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, \code{\link[stats]{predict}}.
#'
#' @references 
#' Zeileis, A., Hothorn, T. and Kornik, K. (2008), Model-based recursive partitioning. 
#' \emph{Journal of Computational and Graphical Statistics}, \bold{17(2)} 492--514.
#'
#' @examples
#' library(locClassData)
#'
#' data <- vData(500)
#' x <- seq(0,1,0.05)
#' grid <- expand.grid(x.1 = x, x.2 = x)
#' 
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = multinomModel, trace = FALSE,
#' control = mob_control(objfun = deviance, minsplit = 200))
#'
#' ## predict posterior probabilities
#' pred <- predict(fit, newdata = grid, out = "posterior")
#' post <- do.call("rbind", pred)
#' 
#' image(x, x, matrix(as.numeric(post[,1]), length(x)), xlab = "x.1", ylab = "x.2")
#' contour(x, x, matrix(as.numeric(post[,1]), length(x)), levels = 0.5, add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## predict node membership
#' splits <- predict(fit, newdata = grid, type = "node")
#' contour(x, x, matrix(splits, length(x)), levels = min(splits):max(splits), add = TRUE, lty = 2)
#'
#' ## training error
#' mean(predict(fit) != data$y)
#'
#' @rdname multinomModel
#' 
#' @import party
#' @export

multinomModel <- new("StatModel",
	name = "multinomial logistic regression",
	dpp = function(formula, data = list(), subset = NULL, na.action = NULL, 
			frame = NULL, enclos = sys.frame(sys.nframe()), other = list(), 
    		designMatrix = TRUE, responseMatrix = TRUE, setHook = NULL, ...) {
    		mf <- match.call(expand.dots = FALSE)
    		m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
		    mf <- mf[c(1, m)]
    		mf[[1]] <- as.name("model.frame")
    		mf$na.action <- stats::na.pass
#cat("mf\n")
#print(mf)
    		MEF <- new("ModelEnvFormula")
    		MEF@formula <- c(modeltools:::ParseFormula(formula, data = data)@formula, 
        		other)
    		MEF@hooks$set <- setHook
    		if (is.null(frame)) 
        		frame <- parent.frame()
    		mf$subset <- try(subset)
    		if (inherits(mf$subset, "try-error")) 
        		mf$subset <- NULL
    		MEF@get <- function(which, data = NULL, frame = parent.frame(), 
        		envir = MEF@env) {
        		if (is.null(data)) 
          	  		RET <- get(which, envir = envir, inherits = FALSE)
        		else {
            		oldData <- get(which, envir = envir, inherits = FALSE)
            		if (!use.subset) 
                		mf$subset <- NULL
            		mf$data <- data
            		mf$formula <- MEF@formula[[which]]
            		RET <- eval(mf, frame, enclos = enclos)
            		modeltools:::checkData(oldData, RET)
        		}
        		return(RET)
    		}
    		MEF@set <- function(which = NULL, data = NULL, frame = parent.frame(), 
        		envir = MEF@env) {
        		if (is.null(which)) 
            		which <- names(MEF@formula)
        		if (any(duplicated(which))) 
            		stop("Some model terms used more than once")
        		for (name in which) {
            		if (length(MEF@formula[[name]]) != 2) 
                		stop("Invalid formula for ", sQuote(name))
            		mf$data <- data
            		mf$formula <- MEF@formula[[name]]
            		if (!use.subset) 
                		mf$subset <- NULL
            		MF <- eval(mf, frame, enclos = enclos)
            		if (exists(name, envir = envir, inherits = FALSE)) 
                		modeltools:::checkData(get(name, envir = envir, inherits = FALSE), 
                  		MF)
            		assign(name, MF, envir = envir)
            		mt <- attr(MF, "terms")
            		if (name == "input" && designMatrix) {
                		#attr(mt, "intercept") <- 0 #???
                		assign("designMatrix", model.matrix(mt, data = MF, 
                  		...), envir = envir)
            		}
            		if (name == "response" && responseMatrix) {
						assign("responseMatrix", MF[,1:ncol(MF)], envir = envir)
#cat("MF[,1:ncol(MF)]")
#print(MF[,1:ncol(MF)])
            		}
        		}
        		MEapply(MEF, MEF@hooks$set, clone = FALSE)
    		}
    		use.subset <- TRUE
    		MEF@set(which = NULL, data = data, frame = frame)
    		use.subset <- FALSE
    		if (!is.null(na.action)) 
        		MEF <- na.action(MEF)
#cat("MEF\n")
#print(str(MEF))
       	MEF
		},
    ##arguments subset, na.action, contrasts, model discarded
	fit = function (object, weights = NULL, summ = 0, censored = FALSE, Hess = FALSE, ...) {
		    class.ind <- function(cl) {
		        n <- length(cl)
		        x <- matrix(0, n, length(levels(cl)))
		        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
		        dimnames(x) <- list(names(cl), levels(cl))
		        x
		    }
		    summ2 <- function(X, Y) {
		        X <- as.matrix(X)
		        Y <- as.matrix(Y)
		        n <- nrow(X)
		        p <- ncol(X)
		        q <- ncol(Y)
		        Z <- t(cbind(X, Y))
		        storage.mode(Z) <- "double"
		        z <- .C("VR_summ2", as.integer(n), as.integer(p), as.integer(q), 
		            Z = Z, na = integer(1L))
		        Za <- t(z$Z[, 1L:z$na, drop = FALSE])
		        list(X = Za[, 1L:p, drop = FALSE], Y = Za[, p + 1L:q])
		    }
	    	Terms <- attr(object@get("input"), "terms")
	    	X <- object@get("designMatrix")
	    	cons <- attr(object@get("designMatrix"), "contrasts")
	    	Xr <- qr(X)$rank
	    	Y <- object@get("responseMatrix")
	    	if (!is.matrix(Y)) 
	        	Y <- as.factor(Y)
	    	w <- weights
	    	if (length(w) == 0L) 
	        	if (is.matrix(Y)) 
	   	         	w <- rep(1, dim(Y)[1L])
		        else w <- rep(1, length(Y))
		    lev <- levels(Y)
		    if (is.factor(Y)) {
		        counts <- table(Y)
		        if (any(counts == 0L)) {
		            empty <- lev[counts == 0L]
		            warning(sprintf(ngettext(length(empty), "group %s is empty", 
		                "groups %s are empty"), paste(sQuote(empty), 
	    	            collapse = " ")), domain = NA)
	       	     	Y <- factor(Y, levels = lev[counts > 0L])
	       	     	lev <- lev[counts > 0L]
	      		}
	        	if (length(lev) < 2L) 
	            	stop("need two or more classes to fit a multinom model")
	        	if (length(lev) == 2L) 
	   	         	Y <- as.vector(unclass(Y)) - 1
	       	 	else Y <- class.ind(Y)
   		 	}
		    if (summ == 1) {
		        Z <- cbind(X, Y)
		        z1 <- cumprod(apply(Z, 2L, max) + 1)
		        Z1 <- apply(Z, 1L, function(x) sum(z1 * x))
		        oZ <- order(Z1)
		        Z2 <- !duplicated(Z1[oZ])
		        oX <- (seq_along(Z1)[oZ])[Z2]
		        X <- X[oX, , drop = FALSE]
		        Y <- if (is.matrix(Y)) 
 		           Y[oX, , drop = FALSE]
		        else Y[oX]
		        w <- diff(c(0, cumsum(w))[c(Z2, TRUE)])
				#print(dim(X))
		    }
		    if (summ == 2) {
		        Z <- summ2(cbind(X, Y), w)
		        X <- Z$X[, 1L:ncol(X)]
		        Y <- Z$X[, ncol(X) + 1L:ncol(Y), drop = FALSE]
		        w <- Z$Y
				#print(dim(X))
		    }
		    if (summ == 3) {
		        Z <- summ2(X, Y * w)
		        X <- Z$X
		        Y <- Z$Y[, 1L:ncol(Y), drop = FALSE]
		        w <- rep(1, nrow(X))
				#print(dim(X))
		    }
		    offset <- attr(object@get("designMatrix"), "offset") 
		    r <- ncol(X)
		    if (is.matrix(Y)) {
		        p <- ncol(Y)
		        sY <- Y %*% rep(1, p)
		        if (any(sY == 0)) 
		            stop("some case has no observations")
		        if (!censored) {
		            Y <- Y/matrix(sY, nrow(Y), p)
		            w <- w * sY
		        }
	 	    	if (length(offset) > 1L) {
       	    		if (ncol(offset) != p) 
                		stop("ncol(offset) is wrong")
            		mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
                		r), rep(FALSE, p)), p - 1L))
           			X <- cbind(X, offset)
            		Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
# cat("mask1\n")
# print(mask)
            		z <- mynnet.default(X, Y, w, Wts = Wts, mask = mask, 
                		size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
                		rang = 0, ...)
        		} else {
            		mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
               		 	r)), p - 1L))
# cat("mask2\n")
# print(mask)
            		z <- mynnet.default(X, Y, w, mask = mask, size = 0, 
                		skip = TRUE, softmax = TRUE, censored = censored, 
            	    	rang = 0, ...)
       	 		}
    		} else {
        		if (length(offset) <= 1L) {
            		mask <- c(FALSE, rep(TRUE, r))
# cat("mask3\n")
# print(mask)
            		z <- mynnet.default(X, Y, w, mask = mask, size = 0, 
            	    	skip = TRUE, entropy = TRUE, rang = 0, ...)
        		} else {
            		mask <- c(FALSE, rep(TRUE, r), FALSE)
           	 		Wts <- c(rep(0, r + 1L), 1)
           		 	X <- cbind(X, offset)
# cat("mask4\n")
# print(mask)
            		z <- mynnet.default(X, Y, w, Wts = Wts, mask = mask, 
            	    	size = 0, skip = TRUE, entropy = TRUE, rang = 0, 
        	        	...)
       		 	}
    		}
    		z$formula <- as.vector(attr(Terms, "formula"))
    		z$terms <- Terms
    		#fit$call <- call
    		z$weights <- w
    		z$lev <- lev
    		z$deviance <- 2 * z$value
    		z$rank <- Xr
    		edf <- ifelse(length(lev) == 2L, 1, length(lev) - 1) * Xr
    		if (is.matrix(Y)) {
        		edf <- (ncol(Y) - 1) * Xr
        		if (length(dn <- colnames(Y)) > 0) 
        	    	z$lab <- dn
    	    	else z$lab <- 1L:ncol(Y)
	    	}
    		z$coefnames <- colnames(X)
    		z$vcoefnames <- z$coefnames[1L:r]
    		z$contrasts <- cons
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$edf <- edf
    		z$AIC <- z$deviance + 2 * edf
	    	# if (model) 
    	    	# z$model <- m
    		class(z) <- c("multinomModel", "multinom", "nnet")
	    	if (Hess) 
	        	z$Hessian <- nnet:::multinomHess(z, X)
    		z$predict_response <- function(newdata = NULL) {#### prior als Argument für predict?
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
    		}
    		z$addargs <- list(summ = summ, censored = censored, Hess = Hess, ...)
    		z$ModelEnv <- object
    		z$statmodel <- multinomModel
   		 	z
		},
	predict = function (object, newdata = NULL, ...) {
			object$predict_response(newdata = newdata)
		},
	capabilities = new("StatModelCapabilities",
		weights = TRUE,
		subset = TRUE
	)
)

	

#' @rdname multinomModel
#'
#' @method reweight multinomModel
#' @S3method reweight multinomModel
#' @import party

reweight.multinomModel <- function (object, weights, ...) {
    fit <- multinomModel@fit
    try(do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs)))
}



#' @noRd
#'
#' @method model.matrix multinomModel
#' @S3method model.matrix multinomModel
#' @importFrom stats model.matrix

model.matrix.multinomModel <- function (object, ...) 
	object$ModelEnv@get("designMatrix")



#' @noRd

model.response.multinomModel <- function (object, ...)
	object$ModelEnv@get("responseMatrix")



#' @rdname multinomModel
#'
#' @method deviance multinom
#' @S3method deviance multinom
#' @importFrom stats deviance

## deviance is -2*log-likelihood and is minimized
deviance.multinom <- function (object, ...) {
	return(object$deviance)
}



#' @rdname multinomModel
#'
#' @method estfun multinom
#' @S3method estfun multinom
#' @importFrom sandwich estfun

estfun.multinom <- function(x, ...) {
#cat("gradient\n")
# print(x$wts)
# print(x$gr[x$wts != 0])
# print(colSums(x$gradient))
# print(head(x$gradient))
# print(x$convergence) ## 0 für konvergenz, 1 wenn maxit erreicht
# print(cor(x$gradient[x$weights>0,,drop = FALSE]))
# print(z <- crossprod(x$gradient[x$weights>0,,drop = FALSE]/sqrt(sum(x$weights))))
# print(eigen(z, symmetric = TRUE, only.values = TRUE)$values)
# print(z <- crossprod(10^10*x$gradient[x$weights>0,,drop = FALSE]/sqrt(n)))
# print(eigen(z, symmetric = TRUE, only.values = TRUE)$values)	
#print(crossprod(x$gradient[x$weights>0,,drop = FALSE])[1:5,1:5])
#print(solve(crossprod(x$gradient[x$weights>0,,drop = FALSE])[1:5,1:5]))
	return(x$gradient)
}



#' @import nnet

add.net <- nnet:::add.net



#' @rdname multinomModel
#'
#' @method predict multinomModel
#' @S3method predict multinomModel

predict.multinomModel <- function(object, out = c("class", "posterior"), newdata, ...) {
	out <- match.arg(out)
	pred <- switch(out,
		class = {
			cl <- NextMethod(object, type = "class", newdata, ...)
			factor(cl, levels = object$lev)	
		},
		posterior = {
			post <- NextMethod(object, type = "probs", newdata, ...)
			if (nrow(newdata) == 1) {
				post <- t(as.matrix(post))
				if (ncol(post) == 1)
					post <- cbind(1 - post, post)
				colnames(post) <- object$lev #?
			}
			if (!is.matrix(post)) {
				post = cbind(1 - post, post)
				colnames(post) <- object$lev
			}
			lapply(seq_len(nrow(post)), function(i) post[i,, drop = FALSE])
		})
	return(pred)
}
