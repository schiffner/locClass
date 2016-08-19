#' Combine Model-based Recursive Partitioning with Quadratic Discriminant Analysis.
#'
#' This page lists all ingredients to combine Quadratic Discriminant Analysis with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{qdaModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link{wqda}} model.
#'
#' Moreover, methods for \code{\link{wqda}} and \code{qdaModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with Quadratic Discriminant Analysis
#'
#' @param object An object of class "qdaModel" and "wqda", respectively.
#' @param x An object of class "wqda".
#' @param weights A vector of observation weights.
#' @param out Should class labels or posterior probabilities be returned?
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "qdaModel" object. \cr
#' \code{deviance}: The value of the deviance for Quadratic Discriminant Analysis extracted from \code{object}, i.e. the log-likelihood. \cr
#' \code{estfun}: The empirical estimating (or score) function for Quadratic Discriminant Analysis, i.e. the derivatives of the log-likelihood with respect
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
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = qdaModel,
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
#' @rdname qdaModel
#' 
#' @import party
#' @export

qdaModel <- new("StatModel",
	name = "quadratic discriminant analysis",
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
       			z <- wqda(object@get("designMatrix"), object@get("responseMatrix"), method = "ML", ...)
    		} else {
        		z <- wqda(object@get("designMatrix"), object@get("responseMatrix"), method = "ML",
            		weights = weights, ...)
    		}
    		class(z) <- c("qdaModel", "wqda")
    		z$terms <- attr(object@get("input"), "terms")
    		z$contrasts <- attr(object@get("designMatrix"), "contrasts")
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$predict_response <- function(newdata = NULL) {#### prior as argument for predict
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
        			0.5 * determinant(z$cov[[y]])$modulus - 0.5 * mahalanobis(x, 
        			center = z$means[y, ], cov = z$cov[[y]]))
    			gr <- factor(lev1[max.col(posterior)], levels = z$lev)
			    names(gr) <- rownames(dm)
        		return(gr)
    		}
    		z$addargs <- list(...)
    		z$ModelEnv <- object
    		z$statmodel <- qdaModel
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
	

	
#' @rdname qdaModel
#'
#' @method reweight qdaModel
#' @S3method reweight qdaModel
#' @import party
	
reweight.qdaModel <- function (object, weights, ...) {
    fit <- qdaModel@fit
    try(do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs)))
}



#' @noRd
#'
#' @method model.matrix qdaModel
#' @S3method model.matrix qdaModel
#' @importFrom stats model.matrix

model.matrix.qdaModel <- function (object, ...) 
	object$ModelEnv@get("designMatrix")



#' @noRd

model.response.qdaModel <- function (object, ...)
	object$ModelEnv@get("responseMatrix")



#' @rdname qdaModel
#'
#' @method deviance wqda
#' @S3method deviance wqda
#' @importFrom stats deviance

## negative log-likelihood for wqda
## if classes are missing in the training data their weights are 0
## instead of calculating the quantities for all observations and then multipliying by 0 or >0 before summing them up
## calculate them only for those observations with weights >0
deviance.wqda <- function (object, ...) {
	try({
		wts <- weights(object)
		if (is.null(wts)) 
			wts <- 1
		indw <- wts > 0
		xmat <- model.matrix(object, ...)[indw, , drop = FALSE]
		gr <- model.response.qdaModel(object, ...)[indw]
		## check
   		# ng <- nlevels(gr)
		# lev1 <- names(object$prior)
    	# post <- matrix(NA, ncol = ng, nrow = nrow(xmat), dimnames = list(rownames(xmat), levels(gr)))
    	# post[,lev1] <- sapply(lev1, function(z) log(object$prior[z]) + dmvnorm(xmat, object$means[z,], object$cov[[z]], log = TRUE))
# print("deviance")
    	# ll <- post[cbind(rownames(post), as.character(gr))]
# print(head(-ll))
		pr <- object$prior[as.character(gr)]
		dets <- sapply(object$cov, function(x) determinant(x)$modulus)[as.character(gr)]
		mahal <- numeric(length(gr))
		for (g in names(object$prior)) {
			mahal[gr == g] <- mahalanobis(xmat[gr == g, , drop = FALSE], object$means[g,, drop = FALSE], object$cov[[g]])
		}
# print(head(-log(pr) + ncol(xmat)/2 * log(2*pi) + 0.5 * dets + 0.5 * mahal))
		return(sum(wts[indw] * (-log(pr) + 0.5 * dets + 0.5 * mahal)))
	})
	return(Inf)
}



#' @rdname qdaModel
#'
#' @method estfun wqda
#' @S3method estfun wqda
#' @importFrom sandwich estfun

estfun.wqda <- function(x, ...) {
    wts <- weights(x)
    if (is.null(wts)) 
        wts <- 1
	xmat <- model.matrix(x, ...)
    gr <- as.factor(model.response.qdaModel(x, ...))
	## scores with respect to priors
  	dPrior <- diag(nlevels(gr))[gr,]					# zero-one class indicator matrix, number of columns equals total number of classes
  	colnames(dPrior) <- levels(gr)
  	d <- dPrior <- dPrior[,names(x$prior), drop = FALSE]		# select columns that belong to classes present in the current subset
    dPrior <- wts * t(-t(dPrior) + as.vector(x$prior))			# calculate scores
	if (ncol(dPrior) > 1)	# if dPrior has more than 2 columns drop the first one in order to prevent linear dependencies (n x (K-1))	
		dPrior <- dPrior[,-1, drop = FALSE]
	# else: if dPrior has only one column there is only one class present in the training data and a try-error will occur in the fluctuation tets
	## scores with respect to means
    p <- ncol(xmat)
    n <- nrow(xmat)
    K <- ncol(d)
    z <- matrix(0, n, p)
    indw <- wts > 0
	z[indw,] <- xmat[indw, , drop = FALSE] - x$means[as.character(gr[indw]), , drop = FALSE]
	cov.inv <- lapply(x$cov, solve)
	dM <- matrix(0, n, p)
	for (g in names(x$prior)) {
		idx <- gr == g
		dM[idx,] <- -wts[idx] * z[idx,,drop = FALSE] %*% cov.inv[[g]]
	}
	dMean <- d[,rep(1:K, each = p), drop = FALSE] * dM[,rep(1:p, K), drop = FALSE]
	## scores with respect to cov
	inds <- cbind(rep(1:p, each = p), rep(1:p, p))
	inds <- inds[inds[,1] <= inds[,2], , drop = FALSE]
	f <- function(ind, cov.inv, z) {
		S <- lapply(cov.inv, function(x) x[,ind[1],drop = FALSE] %*% x[ind[2],,drop = FALSE])
		res <- numeric(n)
		for (g in names(x$prior)) {
			idx <- gr == g
			res[idx] <- 0.5 * wts[idx] * (cov.inv[[g]][ind[1],ind[2]] - mahalanobis(z[idx, , drop = FALSE], center = 0, cov = S[[g]], inverted = TRUE))
		}
		return(res)
	}
	dC <- apply(inds, 1, f, cov.inv = cov.inv, z = z)
	nc <- p*(p+1)/2
	dCov <- d[,rep(1:K, each = nc), drop = FALSE] * dC[,rep(1:nc, K), drop = FALSE]
# checks
# print(0.5 * cov.inv[[gr[1]]] - 0.5 * cov.inv[[gr[1]]] %*% z[1,] %*% t(z[1,]) %*% cov.inv[[gr[1]]])
# print(0.5 * cov.inv[[gr[2]]] - 0.5 * cov.inv[[gr[2]]] %*% z[2,] %*% t(z[2,]) %*% cov.inv[[gr[2]]])
# print(0.5 * cov.inv[[gr[3]]] - 0.5 * cov.inv[[gr[3]]] %*% z[3,] %*% t(z[3,]) %*% cov.inv[[gr[3]]])
# print(head(dCov))
# print(cbind(gr, dPrior, dMean, dCov))
# print(colSums(cbind(dPrior, dMean, dCov)))
	return(cbind(dPrior, dMean, dCov))
}



#' @rdname qdaModel
#'
#' @method predict qdaModel
#' @S3method predict qdaModel

predict.qdaModel <- function(object, out = c("class", "posterior"), ...) {
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
