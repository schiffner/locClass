#' Combine Model-Based Recursive Partitioning with Linear Discriminant Analysis.
#'
#' This page lists all ingredients to combine Linear Discriminant Analysis with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{ldaModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link{wlda}} model.
#'
#' Moreover, methods for \code{\link{wlda}} and \code{ldaModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with Linear Discriminant Analysis
#'
#' @param object An object of class "ldaModel" and "wlda", respectively.
#' @param x An object of class "wlda".
#' @param weights A vector of observation weights.
#' @param out Should class labels or posterior probabilities be returned?
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "ldaModel" object. \cr
#' \code{deviance}: The value of the deviance for Linear Discriminant Analysis extracted from \code{object}, i.e. the log-likelihood. \cr
#' \code{estfun}: The empirical estimating (or score) function for Linear Discriminant Analysis, i.e. the derivatives of the log-likelihood with respect
#'   to the parameters, evaluated at the training data. \cr
#' \code{predict}: Either a vector of predicted class labels or a matrix of class posterior probabilities.
#'
#' @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, \code{\link[stats]{predict}}.
#'
#' @family recursive_partitioning lda
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
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = ldaModel,
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
#' mean(predict(fit) != as.numeric(data$y))
#'
#' @rdname ldaModel 
#'
#' @import party
#' @export

ldaModel <- new("StatModel",
	name = "linear discriminant analysis",
	dpp = function(formula, data = list(), subset = NULL, na.action = NULL, 
			frame = NULL, enclos = sys.frame(sys.nframe()), other = list(), 
    		designMatrix = TRUE, responseMatrix = TRUE, setHook = NULL, ...) {
    		mf <- match.call(expand.dots = FALSE)
    		m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
		    mf <- mf[c(1, m)]
    		mf[[1]] <- as.name("model.frame")
    		mf$na.action <- stats::na.pass
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
                		assign("responseMatrix", MF[,1], envir = envir)
            		}
        		}
        		MEapply(MEF, MEF@hooks$set, clone = FALSE)
    		}
    		use.subset <- TRUE
    		MEF@set(which = NULL, data = data, frame = frame)
    		use.subset <- FALSE
    		if (!is.null(na.action)) 
        		MEF <- na.action(MEF)
	       	MEF
		},
	fit = function (object, weights = NULL, ...) {
    		if (is.null(weights)) {
       			z <- wlda(object@get("designMatrix"), object@get("responseMatrix"), method = "ML", ...)
    		} else {
        		z <- wlda(object@get("designMatrix"), object@get("responseMatrix"), method = "ML",
            		weights = weights, ...)
    		}
    		class(z) <- c("ldaModel", "wlda")
    		z$terms <- attr(object@get("input"), "terms")
    		z$contrasts <- attr(object@get("designMatrix"), "contrasts")
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$predict_response <- function(newdata = NULL) {#### prior as argument for predict?
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
    			lev1 <- names(z$prior)
    			ng <- length(lev1)
    			posterior <- matrix(0, ncol = ng, nrow = nrow(dm), dimnames = list(rownames(dm), lev1))
    			posterior[, lev1] <- sapply(lev1, function(y) log(z$prior[y]) - 
        			0.5 * mahalanobis(dm, center = z$means[y, ], cov = z$cov))
    			gr <- factor(lev1[max.col(posterior)], levels = z$lev)
			    names(gr) <- rownames(dm)
        		return(gr)
    		}
    		z$addargs <- list(...)
    		z$ModelEnv <- object
    		z$statmodel <- ldaModel
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
	


#' @rdname ldaModel
#'
#' @import party
#' @export
	
reweight.ldaModel <- function (object, weights, ...) {
    fit <- ldaModel@fit
    try(do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs)))
}



#' @noRd
#'
#' @importFrom stats model.matrix
#' @export

model.matrix.ldaModel <- function (object, ...) 
	object$ModelEnv@get("designMatrix")



#' @noRd

model.response.ldaModel <- function (object, ...)
	object$ModelEnv@get("responseMatrix")



#' @rdname ldaModel
#'
#' @importFrom stats deviance
#' @export

## negative log-likelihood for wlda
## if classes are missing in the training data their weights are 0
## instead of calculating the quantities for all observations and then multipliying by 0 or >0 before summing them up
## calculate them only for those observations with weights >0
deviance.wlda <- function (object, ...) {
    try({
		wts <- weights(object)
		if (is.null(wts)) 
			wts <- 1
		indw <- wts > 0
		xmat <- model.matrix(object, ...)[indw, , drop = FALSE]
		gr <- model.response.ldaModel(object, ...)[indw]
		## check
   		# ng <- nlevels(gr)
		# lev1 <- names(object$prior)
    	# post <- matrix(NA, ncol = ng, nrow = nrow(xmat), dimnames = list(rownames(xmat), levels(gr)))
    	# post[,lev1] <- sapply(lev1, function(z) log(object$prior[z]) + dmvnorm(xmat, object$means[z,], object$cov, log = TRUE))
# print(head(cbind(gr, post)))
    	# ll <- post[cbind(rownames(post), as.character(gr))]
# print(head(-ll))
		pr <- object$prior[as.character(gr)]
		z <- xmat - object$means[as.character(gr), , drop = FALSE]
# print(head(-log(pr) + ncol(xmat)/2 * log(2*pi) + 0.5 * determinant(object$cov)$modulus + 0.5 * mahalanobis(z, 0, object$cov)))
		return(sum(wts[indw] * (-log(pr) + 0.5 * determinant(object$cov)$modulus + 0.5 * mahalanobis(z, 0, object$cov))))
	})
	return(Inf)
}



#' @noRd
#'
#' @export

'deviance.try-error' <- function(object, ...) Inf



#' @rdname ldaModel
#'
#' @importFrom sandwich estfun
#' @export

estfun.wlda <- function(x, ...) {
    wts <- weights(x)
    if (is.null(wts)) 
        wts <- 1
	xmat <- model.matrix(x, ...)
    gr <- as.factor(model.response.ldaModel(x, ...))
	### scores with respect to priors
  	dPrior <- diag(nlevels(gr))[gr,]							# zero-one class indicator matrix, number of columns equals total number of classes
  	colnames(dPrior) <- levels(gr)
  	d <- dPrior <- dPrior[,names(x$prior), drop = FALSE]		# select columns that belong to classes present in the current subset
    dPrior <- wts * t(-t(dPrior) + as.vector(x$prior))			# calculate scores
	if (ncol(dPrior) > 1)	# if dPrior has more than 2 columns drop the first one in order to prevent linear dependencies (n x (K-1) matrix)	
		dPrior <- dPrior[,-1, drop = FALSE]
	# else: if dPrior has only one column there is only one class present in the training data and a try-error will occur in the fluctuation tets
	## scores with respect to means
    p <- ncol(xmat)
    n <- nrow(xmat)
    K <- ncol(d)
    z <- matrix(0, n, p)
    indw <- wts > 0
	z[indw,] <- xmat[indw, , drop = FALSE] - x$means[as.character(gr[indw]), , drop = FALSE]
	cov.inv <- solve(x$cov)	
	dMean <- d[,rep(1:K, each = p), drop = FALSE] * (-wts * z %*% cov.inv)[,rep(1:p, K), drop = FALSE]		# n x (K * V) matrix
	## scores with respect to cov
	inds <- cbind(rep(1:p, each = p), rep(1:p, p))
	inds <- inds[inds[,1] <= inds[,2], , drop = FALSE]
	f <- function(ind, cov.inv, z) {
		S <- cov.inv[,ind[1],drop = FALSE] %*% cov.inv[ind[2],,drop = FALSE]
		return(wts * 0.5 * (cov.inv[ind[1], ind[2]] - mahalanobis(z, center = 0, cov = S, inverted = TRUE)))
	}
	dCov <- apply(inds, 1, f, cov.inv = cov.inv, z = z)
# checks
# print(0.5 * cov.inv - 0.5 * cov.inv %*% z[1,] %*% t(z[1,]) %*% cov.inv)
# print(0.5 * cov.inv - 0.5 * cov.inv %*% z[2,] %*% t(z[2,]) %*% cov.inv)
# print(0.5 * cov.inv - 0.5 * cov.inv %*% z[3,] %*% t(z[3,]) %*% cov.inv)
# print(head(dCov))
# print(cbind(gr, dPrior, dMean, dCov))
# print(colSums(cbind(dPrior, dMean, dCov)))
# print(cor(cbind(dPrior, dMean, dCov)[x$weights>0,,drop = FALSE]))
	return(cbind(dPrior, dMean, dCov))
}



#' @rdname ldaModel
#'
#' @export

predict.ldaModel <- function(object, out = c("class", "posterior"), ...) {
	pred <- NextMethod(object, ...)
	out <- match.arg(out)
	pred <- switch(out,
		class = pred$class,
		posterior = {
			post <- pred$posterior
			lapply(seq_len(nrow(post)), function(i) post[i,, drop = FALSE])
		})
	return(pred)
}
