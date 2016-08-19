#' @description Concomitant model class.
#' 
#' @title Classes "FLXPwlda" and "FLXPwqda"
#
# @section Slots:
#  \describe{
#    \item{\code{name}:}{Character string used in print methods.}
#    \item{\code{formula}:}{Formula describing the model.}
#    \item{\code{x}:}{Model matrix.}
#    \item{\code{fit}:}{Function returning the fitted prior probabilities.}
#    \item{\code{refit}:}{Function returning the fitted concomitant model.}
#    \item{\code{coef}:}{Matrix containing the fitted parameters.}
#    \item{\code{df}:}{Function for determining the number of degrees of
#      freedom used.}
#  }
#
#' @rdname FLXPwlda-class
# @aliases FLXPwlda-class
#
#' @family mixtures
#'
#' @import flexmix
#' @export
#'
#' @keywords class
#'
#' @seealso \code{\link[flexmix]{FLXP-class}}.

setClass("FLXPwlda", contains = "FLXP")
setClass("FLXPwqda", contains = "FLXP")



#' @title Creator Functions for the Concomitant Variable Model based on Linear or Quadratic Discriminant Analysis
#'
#' @description Creator function for the concomitant variable model. Priors are modeled by Linear or Quadratic Discriminant Analysis.
#'
#' @param formula A formula for determining the model matrix of the concomitant variables.
#'
#' @return Object of class \code{FLXPwlda} or \code{FLXPwqda} which both extend class \code{FLXP} directly and are used for method dispatching.
#'
#' @import flexmix
#' @export
#'
#' @rdname FLXPwlda
# @aliases FLXPwlda
#'
#' @seealso \code{\link[flexmix]{FLXPmultinom}}.
#'
#' @family mixtures svm
#'
#' @examples
#' library(benchData)
#' data <- flashData(1000)
#' x1 <- seq(-6,6,0.2)
#' x2 <- seq(-4,4,0.2)
#' grid <- expand.grid(x.1 = x1, x.2 = x2)
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLmultinom(trace = FALSE)
#' fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster, control = list(verb = 1))
#'
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
#' image(x1, x2, matrix(pred.grid[[1]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[1]][,1], length(x1)), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' image(x1, x2, matrix(pred.grid[[2]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[2]][,1], length(x1)), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## prediction with aggregation depending on membership in mixture components
#' pred.grid <- mypredict(fit, newdata = grid, aggregate = TRUE)
#' image(x1, x2, matrix(pred.grid[[1]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[1]][,1], length(x1)), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## local membership
#' loc.grid <- prior(fit, newdata = grid)
#' contour(x1, x2, matrix(loc.grid[,1], length(x1)), add  = TRUE)

FLXPwlda <- function (formula = ~ .) {
    z <- new("FLXPwlda", name = "FLXPwlda", formula = formula)
    wlda.fit <- function(x, y, w, ...) {
        n <- nrow(x)
		if (missing(w) || is.null(w)) 
            w <- rep(1, n)
print(head(x))
print(w)
print(head(y))
        J <- ncol(y)
        class.weights <- colSums(w * y)
        prior <- class.weights/sum(w)
print(prior)		
 		xwt <- crossprod(w * y, x)	# J x V ## no intercept
 		means <- xwt/class.weights	# J x V
print(means)
 		covs <- sapply(seq_len(J), function(j) {
	 		z <- sweep(x, 2, means[j,])
 			return(crossprod(w * y[,j] * z, z))
 		})
		cov <- matrix(rowSums(covs)/sum(w), ncol(x))
print(cov)
		return(list(prior = prior, means = means, cov = cov))
    }
	z@fit <- function(x, y, w, ...) {
		## returns unormalized posteriors !!!
		fit <- wlda.fit(x, y, w, ...)
		J <- length(fit$prior)
   		# post <- sapply(seq_len(J), function(j) fit$prior[j] * dmvnorm(x, fit$means[j,], fit$cov))
	    post <- sapply(seq_len(J), function(j) log(fit$prior[j]) - 
    	    0.5 * mahalanobis(x, center = fit$means[j, ], cov = fit$cov))	
    	post <- exp(post)
    	return(post)
	}
    z@refit <- function(x, y, w, ...) {
		## just the coefficients
		fit <- wlda.fit(x, y, w, ...)
        fit$weights <- w
        return(fit)
    }
    z
}



#' @import flexmix
#' @export
#'
#' @family mixtures
#'
#' @rdname FLXPwlda
# @aliases FLXPwlda

FLXPwqda <- function (formula = ~ .) {
    z <- new("FLXPwqda", name = "FLXPwqda", formula = formula)
    wqda.fit <- function(x, y, w, ...) {
        n <- nrow(x)
		if (missing(w) || is.null(w)) 
            w <- rep(1, n)
print(head(x))
print(w)
print(head(y))
        J <- ncol(y)
        class.weights <- colSums(w * y)
        prior <- class.weights/sum(w)
print(prior)		
 		xwt <- crossprod(w * y, x)	# J x V ## no intercept
 		means <- xwt/class.weights	# J x V
print(means)
 		covs <- lapply(seq_len(J), function(j) {
	 		z <- sweep(x, 2, means[j,])
 			return(crossprod(w * y[,j] * z, z)/class.weights)
 		})
		# cov <- matrix(rowSums(covs)/sum(w), ncol(x))
		## FIXME
print(covs)
		return(list(prior = prior, means = means, covs = covs))
    }
	z@fit <- function(x, y, w, ...) {
		## returns unormalized posteriors !!!
		fit <- wqda.fit(x, y, w, ...)
		J <- length(fit$prior)
   		# post <- sapply(seq_len(J), function(j) fit$prior[j] * dmvnorm(x, fit$means[j,], fit$cov))
	    post <- sapply(seq_len(J), function(j) log(fit$prior[j]) - 
    	    0.5 * mahalanobis(x, center = fit$means[j, ], cov = fit$covs[[j]]))	
    	post <- exp(post)
    	return(post)
	}
    z@refit <- function(x, y, w, ...) {
		## just the coefficients
		fit <- wqda.fit(x, y, w, ...)
        fit$weights <- w
        return(fit)
    }
    z
}



# @rdname FLXPwlda
# @aliases FLXgetModelmatrix,FLXPwlda-method
#
# @import flexmix
# @export
#' @noRd
setMethod("FLXgetModelmatrix", signature(model = "FLXPwlda"), function (model, data, groups, lhs, ...) {
    mt <- terms(model@formula, data = data)
	attr(mt, "intercept") <- 0	# important: delete intercept
    mf <- model.frame(delete.response(mt), data = data, na.action = NULL)
    X <- model.matrix(mt, data = mf)
    if (nrow(X)) {
    	if (!flexmix:::checkGroup(X, groups$group)) 
            stop("model variables have to be constant for grouping variable")
        model@x <- X[groups$groupfirst, , drop = FALSE]
    } else {
        model@x <- matrix(1, nrow = sum(groups$groupfirst))
        ## FIXME
    }
    model
})


#' @noRd
setMethod("FLXgetModelmatrix", signature(model = "FLXPwqda"), function (model, data, groups, lhs, ...) {
    mt <- terms(model@formula, data = data)
	attr(mt, "intercept") <- 0	# important: delete intercept
    mf <- model.frame(delete.response(mt), data = data, na.action = NULL)
    X <- model.matrix(mt, data = mf)
    if (nrow(X)) {
    	if (!flexmix:::checkGroup(X, groups$group)) 
            stop("model variables have to be constant for grouping variable")
        model@x <- X[groups$groupfirst, , drop = FALSE]
    } else {
        model@x <- matrix(1, nrow = sum(groups$groupfirst))
        ## FIXME
    }
    model
})




## FIXME
# setMethod("refit_mstep", signature(object="FLXPmultinom", newdata="missing"),
# function(object, newdata, posterior, group, ...) {
  # groupfirst <- if (length(group)) groupFirst(group) else rep(TRUE, nrow(posterior))
  # object@refit(object@x, posterior[groupfirst,,drop=FALSE], ...)
# })


FLXfillConcomitant <- flexmix:::FLXfillConcomitant
## FIXME
# setMethod("FLXfillConcomitant", signature(concomitant="FLXP"), function(concomitant, posterior, weights) {
  # concomitant@coef <- concomitant@refit(concomitant@x, posterior, weights)
  # concomitant
# })
#' @noRd
setMethod("FLXfillConcomitant", signature(concomitant="FLXPwlda"), function(concomitant, posterior, weights) {
	print("FLXfillConcomitant")
	concomitant@coef <- as.matrix(concomitant@refit(concomitant@x, posterior, weights))
	concomitant
})


#' @noRd
setMethod("FLXfillConcomitant", signature(concomitant="FLXPwqda"), function(concomitant, posterior, weights) {
	print("FLXfillConcomitant")
	concomitant@coef <- as.matrix(concomitant@refit(concomitant@x, posterior, weights))
	concomitant
})



evalPrior <- flexmix:::evalPrior
# setMethod("evalPrior", signature(concomitant="FLXPmultinom"), function(prior, concomitant) {
  # exps <- exp(concomitant@x %*% concomitant@coef)
  # exps/rowSums(exps)
# })
#' @noRd
setMethod("evalPrior", signature(concomitant="FLXPwlda"), function(prior, concomitant) {
	fit <- concomitant@coef[,1]
	J <- length(fit$prior)
    post <- sapply(seq_len(J), function(j) log(fit$prior[j]) - 
        0.5 * mahalanobis(concomitant@x, center = fit$means[j, ], cov = fit$cov))	
    post <- exp(post - apply(post, 1L, max, na.rm = TRUE))
    post <- post/rowSums(post)
print("evalPrior")
print(post)
    return(post)
})



#' @noRd
setMethod("evalPrior", signature(concomitant="FLXPwqda"), function(prior, concomitant) {
	fit <- concomitant@coef[,1]
	J <- length(fit$prior)
    post <- sapply(seq_len(J), function(j) log(fit$prior[j]) - 
        0.5 * mahalanobis(concomitant@x, center = fit$means[j, ], cov = fit$covs[[j]]))	
    post <- exp(post - apply(post, 1L, max, na.rm = TRUE))
    post <- post/rowSums(post)
print("evalPrior")
print(post)
    return(post)
})



## FIXME
# setMethod("FLXgetParameters", signature(object="FLXPmultinom"),
# function(object, ...) {
  # coefficients <- object@coef[,-1,drop=FALSE]
  # if (ncol(coefficients) > 0) {
    # Names <- paste("Comp", rep(seq_len(ncol(coefficients)+1)[-1], each = nrow(coefficients)),
                   # rownames(coefficients), sep = ".")
    # coefficients <- as.vector(coefficients)
    # names(coefficients) <- paste("concomitant", Names, sep = "_")
    # return(coefficients)
  # }else return(NULL)
# })  



## FIXME
# setMethod("getPriors", signature(object="FLXPmultinom"),
# function(object, group, groupfirst) {
  # priors <- matrix(apply(object@coef, 2, function(x) exp(object@x %*% x)),
                   # nrow = nrow(object@x))
  # ungroupPriors(priors/rowSums(priors), group, groupfirst)
# })



## FIXME
# setMethod("FLXreplaceParameters", signature(object="FLXPmultinom"),
# function(object, parms) {
  # parms <- cbind(0, matrix(parms, nrow = nrow(object@coef)))
  # attributes(parms) <- attributes(object@coef)
  # object@coef <- parms
  # object
# })



## FIXME
# setMethod("dorelabel", signature(object="FLXPmultinom", perm="vector"), function(object, perm, ...) {
  # object@coef <- object@coef[,perm,drop=FALSE]
  # object@coef <- sweep(object@coef, 1, object@coef[,1], "-")
  # colnames(object@coef) <- sapply(seq_len(ncol(object@coef)), function(k)
                                  # gsub("[0-9]+", k, colnames(object@coef)[k]))
  # object
# })



# setMethod("determinePrior", signature(concomitant="FLXPmultinom"), function(prior, concomitant, group) {
  # exps <- exp(concomitant@x %*% concomitant@coef)
  # exps/rowSums(exps)
# })
#' @noRd
setMethod("determinePrior", signature(concomitant="FLXPwlda"), function(prior, concomitant, group) {
	fit <- concomitant@coef[,1]
	J <- length(fit$prior)
    post <- sapply(seq_len(J), function(j) log(fit$prior[j]) - 
        0.5 * mahalanobis(concomitant@x, center = fit$means[j, ], cov = fit$cov))	
    post <- exp(post - apply(post, 1L, max, na.rm = TRUE))
    post <- post/rowSums(post)
	print("determinePrior")
    print(post)
    return(post)
})



#' @noRd
setMethod("determinePrior", signature(concomitant="FLXPwqda"), function(prior, concomitant, group) {
	fit <- concomitant@coef[,1]
	J <- length(fit$prior)
    post <- sapply(seq_len(J), function(j) log(fit$prior[j]) - 
        0.5 * mahalanobis(concomitant@x, center = fit$means[j, ], cov = fit$covs[[j]]))	
    post <- exp(post - apply(post, 1L, max, na.rm = TRUE))
    post <- post/rowSums(post)
	print("determinePrior")
    print(post)
    return(post)
})
