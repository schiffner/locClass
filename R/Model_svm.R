#' Combine Model-based Recursive Partitioning with Support Vector Machines.
#'
#' This page lists all ingredients to combine Support Vector Machines with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{svmModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link{wsvm}} model.
#'
#' Moreover, methods for \code{\link{wsvm}} and \code{svmModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with Support Vector Machines
#'
#' @param object An object of class "svmModel" and "wsvm", respectively.
#' @param x An object of class "wsvm".
#' @param weights A vector of observation weights.
#' @param out Should class labels, posterior probabilities or decision values be returned?
#' @param newdata A \code{data.frame} of cases to be classified.
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "svmModel" object. \cr
#' \code{deviance}: The value of the objective function extracted from \code{object}. \cr
#' \code{estfun}: The empirical estimating (or score) function, i.e. the derivatives of the objective function with respect
#'   to the parameters, evaluated at the training data. \cr
#' \code{predict}: Either a vector of predicted class labels, a matrix of decision values or a matrix of class posterior probabilities.
#'
#' @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, \code{\link[stats]{predict}}.
#'
#' @family recursive_partitioning svm
#'
#' @references 
#' Zeileis, A., Hothorn, T. and Kornik, K. (2008), Model-based recursive partitioning. 
#' \emph{Journal of Computational and Graphical Statistics}, \bold{17(2)} 492--514.
#'
#' @examples
#' library(benchData)
#'
#' data <- vData(500)
#' x <- seq(0,1,0.05)
#' grid <- expand.grid(x.1 = x, x.2 = x)
#' 
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = svmModel, kernel = "linear", fitted = FALSE,
#' control = mob_control(objfun = deviance, minsplit = 200))
#'
#' ## predict decision values
#' dec <- predict(fit, newdata = grid, out = "decision")
#' 
#' image(x, x, matrix(dec, length(x)), xlab = "x.1", ylab = "x.2")
#' contour(x, x, matrix(dec, length(x)), levels = 0, add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## predict node membership
#' splits <- predict(fit, newdata = grid, type = "node")
#' contour(x, x, matrix(splits, length(x)), levels = min(splits):max(splits), add = TRUE, lty = 2)
#'
#' ## training error
#' mean(predict(fit) != data$y)
#'
#' @rdname svmModel 
#'
#' @import party
#' @export

svmModel <- new("StatModel",
	name = "support vector machine",
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
                		attr(mt, "intercept") <- 0
                		assign("designMatrix", model.matrix(mt, data = MF, 
                  		...), envir = envir)
            		}
            		if (name == "response" && responseMatrix) {
						y <- MF[,1]
						if (!is.factor(y)) {
							y <- as.factor(y)
							warning("response variable was coerced to a factor")
						}
                		assign("responseMatrix", y, envir = envir)
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
	fit = function (object, weights = NULL, ..., scale = TRUE) {
# function (formula, data = NULL, case.weights = rep(1, nrow(data)), 
    # ..., subset, na.action = na.omit, scale = TRUE) 		    
    		# call <- match.call()
    		# if (!inherits(formula, "formula")) 
        		# stop("method is only for formula objects")
    		# m <- match.call(expand.dots = FALSE)
    		# if (identical(class(eval.parent(m$data)), "matrix")) 
        		# m$data <- as.data.frame(eval.parent(m$data))
    		# m$... <- NULL
    		# m$scale <- NULL
    		# m$cw <- case.weights
    		# m[[1]] <- as.name("model.frame")
    		# m$na.action <- na.action
    		# m <- eval(m, parent.frame())
    		Terms <- attr(object@get("input"), "terms")
#print(1)
    		# cw <- m[, "(cw)"]
    		# attr(Terms, "intercept") <- 0
    		x <- object@get("designMatrix")
    		y <- object@get("responseMatrix")
#print(2)
    		# attr(x, "na.action") <- attr(y, "na.action") <- attr(weights, 
        		# "na.action") <- attr(m, "na.action")
    		if (length(scale) == 1) 
        		scale <- rep(scale, ncol(x))
#print(3)
    		if (any(scale)) {
        		remove <- unique(c(which(labels(Terms) %in% names(attr(x, 
            		"contrasts"))), which(!scale)))
        		scale <- !attr(x, "assign") %in% remove
    		}
#print(4)
			if (is.null(weights)) {
				z <- wsvm(x, y, scale = scale, ...)#, na.action = na.action)
			} else {
				z <- wsvm(x, y, scale = scale, case.weights = weights, 
        		...)#, na.action = na.action)
			}
    		# ret <- wsvm.default(x, y, scale = scale, case.weights = cw, 
        		# ..., na.action = na.action)
    		z$case.weights <- weights
    		# ret$call <- call
    		# ret$call[[1]] <- as.name("wsvm")
    		z$terms <- Terms
    		# if (!is.null(attr(m, "na.action"))) 
        		# ret$na.action <- attr(m, "na.action")
    		# class(ret) <- c("wsvm.formula", class(ret))
    		# return(ret)
    		class(z) <- c("svmModel", "wsvm", "svm")
    		# z$contrasts <- attr(x, "contrasts")
    		# z$xlevels <- attr(x, "xlevels")
    		z$predict_response <- function(newdata = NULL) {#### probability, decision.values als Argument für predict?
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
    			# lev1 <- names(z$prior)
    			# ng <- length(lev1)
    			# posterior <- matrix(0, ncol = ng, nrow = nrow(dm), dimnames = list(rownames(dm), lev1))
    			# posterior[, lev1] <- sapply(lev1, function(y) log(z$prior[y]) - 
        			# 0.5 * mahalanobis(dm, center = z$means[y, ], cov = z$cov))
    			# gr <- factor(lev1[max.col(posterior)], levels = z$lev)
			    # names(gr) <- rownames(dm)
# stop("predict_response aufgerufen")
        		# return(gr)
    		}
    		z$addargs <- list(scale = scale, ...)
    		z$ModelEnv <- object
    		z$statmodel <- svmModel
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
	

	
#' @rdname svmModel
#'
#' @import party
#' @export
	
reweight.svmModel <- function (object, weights, ...) {
    fit <- svmModel@fit
    try(do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs)))
}



#' @noRd
#'
#' @importFrom stats model.matrix
#' @export

model.matrix.svmModel <- function (object, ...) 
	object$ModelEnv@get("designMatrix")



#' @noRd

model.response.svmModel <- function (object, ...)
	object$ModelEnv@get("responseMatrix")



#' @rdname svmModel
#'
#' @importFrom stats deviance
#' @export

## object$obj: dual objective function for wsvm (to maximize)
deviance.wsvm <- function (object, ...) {
	return(sum(-object$obj))
}



#' @rdname svmModel
#'
#' @importFrom sandwich estfun
#' @export

estfun.wsvm <- function(x, ...) {
    wts <- weights(x)
    if (is.null(wts)) 
        wts <- 1
	xmat <- model.matrix.svmModel(x)
	n <- nrow(xmat)
	nl <- x$nclasses 			# number of present classes, nl = 2 for regression, one-class and binary classification problems
	if (nl == 1) {				# only one class with positive weights
		d1 <- d2 <- matrix(0, n, 1)
	} else {
	d2 <- attr(predict.wsvm(x, newdata = xmat, decision.values = TRUE), "decision.values")	# decision values f(x_n)
	d1 <- matrix(0, n, nl-1)
	d1[x$index,] <- -x$coefs	# -alpha_n y_n (correct rows, for nl>2 columns not correct)
	if (nl == 2) {				# binary classification problem
		d2 <- d2 - x$rho		# f(x_n) - beta_0
		d2 <- d1 * d2			# -alpha_n y_n * (f(x_n) - beta_0)
		m <- -colSums(d2) * wts/sum(wts)	# -colSums(d2) equals the regularization term ||beta||^2
# print("m")
# print(m)
		d2 <- d2 + m			# -alpha_n y_n * (f(x_n) - beta_0) + ||bbeta||^2 * wts/sum(wts)
	} else {					# multi-class problem
		y <- model.response.svmModel(x, ...)
		ng <- length(x$levels)						# total number of classes
		problems <- cbind(rep(x$labels, nl:1-1), unlist(sapply(2:nl, function(z) x$labels[z:nl])))
													# class labels involved in particular binary problems
		npr <- nl*(nl-1)/2							# number of binary classification problems
		m <- matrix(0, npr, ng)
		m[cbind(1:npr,problems[,1])] <- 1
		m[cbind(1:npr,problems[,2])] <- 1
		idx <- m[, as.numeric(y), drop = FALSE]		# 0/1 indicator matrix of obs involved in binary problems, npr x n matrix
		rownames(idx) <- paste(problems[,1], problems[,2], sep = "/")
		d1n <- idx
		d1n[idx != 0] <- t(d1)						# -alpha_n * y_n, npr x n matrix
		idx <- t(idx)								# 0/1 indicator matrix, n x npr matrix
		d2 <- t(d1n * (t(d2) - x$rho))				# -alpha_n y_n (f(x_n) - beta_0), n x npr matrix
		d1 <- t(d1n)								# -alpha_n * y_n, n x npr matrix
# print(cbind(wts, as.numeric(y), idx))
# print(cbind(wts, as.numeric(y), d1))
# print(cbind(wts, as.numeric(y), d2))
		m <- -colSums(d2)			# norm of beta vector
# print("m")
# print(m)
		idx <- wts * idx			# index matrix multiplied by weights, observations with zero weight are removed
									# wts is multiplied to account for wts > 1
		tab <- colSums(idx)			# sum of observation weights in the pairwise problems
		m <- t(t(idx) * m/tab)		# regularization term, n x npr matrix
		d2 <- d2 + m				# -alpha_n y_n (f(x_n) - beta_0) + ||beta|| * wts/sum(wts)
    }    
    }
# print(all(d1 == wts*d1))
# print(all(d2 == wts*d2))
# print(colSums(d1))
# print(colSums(d2))
# print(cor(cbind(d1,d2)))
    return(cbind(d1, d2))
}



#' @rdname svmModel
#'
#' @export

## todo: class labels can be interchanged/missing: correct sign/aggregation of decision.values???
predict.svmModel <- function(object, out = c("class", "posterior", "decision"), newdata, ...) {
	K <- length(object$labels)
	out <- match.arg(out)
	idx <- apply(newdata, 1, function(x) any(is.na(x)))
	pred <- switch(out,
		class = {
			pr <- rep(NA, length(idx))
			pr[!idx] <- NextMethod(object, newdata, ...)
			pr
		},
		posterior = {
			pred <- NextMethod(object, probability = TRUE, newdata, ...)
			post <- matrix(NA, length(idx), K)
			post[!idx,] <- attr(pred, "probabilities")
			colnames(post) <- colnames(attr(pred, "probabilities"))
			rownames(post) <- rownames(newdata)
			lapply(seq_len(nrow(post)), function(i) post[i,, drop = FALSE])
		},
		decision = {
			pred <- NextMethod(object, decision.values = TRUE, newdata, ...)
			decision <- matrix(NA, length(idx), K*(K-1)/2)
			decision[!idx,] <- attr(pred, "decision.values")
			colnames(decision) <- colnames(attr(pred, "decision.values"))
			rownames(decision) <- rownames(newdata)
			lapply(seq_len(nrow(decision)), function(i) decision[i,, drop = FALSE])
		})
	return(pred)
}
