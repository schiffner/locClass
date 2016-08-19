# Copyright (C) 2011-2012 Julia Schiffner
# Copyright (C) 2004-2011 Friedrich Leisch and Bettina Gruen
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



#' @rdname FLXMCL
#' @aliases FLXMCLlda-class
#'
#' @import flexmix
#' @export

setClass("FLXMCLlda", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} from package \pkg{flexmix} implementing mixtures of Linear Discriminant Analysis Models.
#'
#' @title Mixtures of Linear Discriminant Analysis Models
#'
#' @note \code{method = "ML"} is hard-coded.
#'
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
# @param method Method for scaling the pooled weighted covariance matrix, either \code{"unbiased"} or maximum-likelihood (\code{"ML"}). 
#   Defaults to \code{"unbiased"}.
#' @param \dots Further arguments to and from other methods, especially \code{\link{wlda}}.
#'
#' @return Returns an object of class \code{FLXMCLlda} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLlda
#' @aliases FLXMCLlda
#'
#' @import flexmix
#' @export
#'
#' @examples
#' library(locClassData)
#' data <- flashData(1000)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLlda()
#' fit <- flexmix(y ~ ., data = as.data.frame(data), model = model, cluster = cluster, control = list(verb = 1))
#' 
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
#'
#' # joint density of predictors and class variable for class 1
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' # posterior probability of class 1
#' pred.grid <- lapply(pred.grid, function(x) x/rowSums(x))
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## prediction with aggregation depending on membership in mixture components
#' pred.grid <- mypredict(fit, newdata = grid, aggregate = TRUE)
#'
#' # joint density of predictors and class variable for class 1
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' # posterior of class 1
#' pred.grid <- lapply(pred.grid, function(x) x/rowSums(x))
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## local membership
#' grid <- cbind(y = flashBayesClass(grid), grid)
#' loc.grid <- posterior(fit, newdata = grid)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)

FLXMCLlda <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLlda", weighted = TRUE, formula = formula,
		name = "Mixture of LDA models")
	z@defineComponent <- expression({
		predict <- function(x) {
			## returns unormalized posteriors !!!
    		ng <- length(attr(y, "lev"))
			lev1 <- names(fit$prior)
    		post <- matrix(0, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), attr(y, "lev")))
    		post[,lev1] <- sapply(lev1, function(z) fit$prior[z] * dmvnorm(x, fit$means[z,], fit$cov))
	    	return(post)
		}
		logLik <- function(x, y) {
			## unnormalized log posterior, joint log likelihood
    		ng <- length(attr(y, "lev"))
			lev1 <- names(fit$prior)
    		post <- matrix(-10000, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), attr(y, "lev")))
    		post[,lev1] <- sapply(lev1, function(z) log(fit$prior[z]) + dmvnorm(x, fit$means[z,], fit$cov, log = TRUE))
#print(head(post))
    		ll <- post[cbind(rownames(post), as.character(y))]
#print(head(ll))
	    	return(ll)
		}
		new("FLXcomponent", parameters = list(prior = fit$prior, means = fit$means, cov = fit$cov, 
			method = fit$method), logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(grouping) {
    	if (!is.factor(grouping))
        	warning("'grouping' was coerced to a factor")
	    g <- as.factor(grouping)
	    lev <- levels(g)
    	g <- as.matrix(g)
    	attr(g, "lev") <- lev
		return(g)
	}
	z@fit <- function(x, y, w) {
		fit <- wlda(x, factor(y, levels = attr(y, "lev")), weights = w, method = "ML", ...)
		K <- nrow(fit$means)
		d <- ncol(fit$means)
		fit$df <- K*d + d*(d-1)/2
		with(fit, eval(z@defineComponent))
	}
	z
}



#' @rdname FLXMCLlda
#' @aliases FLXgetModelmatrix,FLXMCLlda-method
#'
#' @import flexmix
#' @export
#'
#' @docType methods

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLlda"), 
	function (model, data, formula, lhs = TRUE, ...) {
    formula <- flexmix:::RemoveGrouping(formula)
    if (length(grep("\\|", deparse(model@formula)))) 
        stop("no grouping variable allowed in the model")
    if (is.null(model@formula)) 
        model@formula = formula
    model@fullformula = update(terms(formula, data = data), 
        model@formula)
    if (lhs) {
        mf <- if (is.null(model@terms)) 
            model.frame(model@fullformula, data = data, na.action = NULL)
        else model.frame(model@terms, data = data, na.action = NULL)
        model@terms <- attr(mf, "terms")
        modely <- model.response(mf)
        model@y <- model@preproc.y(modely)
    }
    else {
        mt1 <- if (is.null(model@terms)) 
            terms(model@fullformula, data = data)
        else model@terms
        mf <- model.frame(delete.response(mt1), data = data, 
            na.action = NULL)
        model@terms <- attr(mf, "terms")    
    }
    attr(model@terms, "intercept") <- 0  ## intercept removed
    X <- model.matrix(model@terms, data = mf)
    model@contrasts <- attr(X, "contrasts")
    model@x <- X
    model@x <- model@preproc.x(model@x)
    model@xlevels <- .getXlevels(model@terms, mf)
    model
})


## old version
# FLXMCLlda <- function(formula = . ~ ., method = c("unbiased", "ML"), ...) {
	# method <- match.arg(method)
	# z <- new("FLXMCLlda", weighted = TRUE, formula = formula,
		# name = "Mixture of LDA models")
	# z@defineComponent <- expression({
		# predict <- function(x, ...) {
			# pred <- getS3method("predict", "wlda")(fit, newdata = x, ...)
			# lev <- levels(pred$class)
			# ng <- length(lev)
			# if (ng > ncol(pred$posterior)) {
	        	# posterior <- matrix(0, nrow(pred$posterior), ng)
	        	# rownames(posterior) <- rownames(pred$posterior)
	        	# colnames(posterior) <- lev
	        	# posterior[,colnames(pred$posterior)] <- pred$posterior
			# } else
        		# posterior <- pred$posterior
        	# return(posterior)
		# }
		# logLik <- function(x, y, ...) {
			# ## log posterior
    		# post <- getS3method("predict", "wlda")(fit, newdata = x, ...)$posterior
    		# ng <- length(attr(y, "lev"))
# # print(head(post))
# # print(head(y))
    		# if (ng > ncol(post)) {
    			# ll <- rep(0, nrow(post))
    			# col.index <- match(y, colnames(post), 0)
    			# row.index <- which(col.index > 0)
    			# ll[row.index] <- post[cbind(row.index, col.index[row.index])]
    		# } else {
	    		# ll <- post[cbind(rownames(post), as.character(y))]
	    	# }
	    	# ll <- ifelse(ll == 0, -10000, log(ll))
# # print(head(ll))
	    	# return(ll)

			# # #unnormalized log posterior, joint log likelihood
    		# # ng <- length(attr(y, "lev"))
			# # lev1 <- names(fit$prior)
    		# # post <- matrix(-10000, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), attr(y, "lev")))
    		# # post[,lev1] <- sapply(lev1, function(z) log(fit$prior[z]) + dmvnorm(x, fit$means[z,], fit$cov, log = TRUE))
    		# # #post[,lev1] <- sapply(lev1, function(z) log(fit$prior[z]) - 0.5 * mahalanobis(x, center = fit$means[z,], cov = fit$cov))
# # #print(head(post))
    		# # ll <- post[cbind(rownames(post), as.character(y))]
# # #print(head(ll))
	    	# # return(ll)
		# }
		# new("FLXcomponent", parameters = list(prior = fit$prior, means = fit$means, cov = fit$cov), 
			# logLik = logLik, predict = predict, df = fit$df)
	# })
	# z@preproc.y <- function(grouping) {
    	# if (!is.factor(grouping))
        	# warning("'grouping' was coerced to a factor")
	    # g <- as.factor(grouping)
	    # lev <- levels(g)
    	# g <- as.matrix(g)
    	# attr(g, "lev") <- lev
		# return(g)
	# }
	# z@fit <- function(x, y, w) {
		# fit <- wlda(x, factor(y, levels = attr(y, "lev")), weights = w, method = method)
		# K <- nrow(fit$means)
		# d <- ncol(fit$means)
		# fit$df <- K*d + d*(d-1)/2
		# with(fit, eval(z@defineComponent))
	# }
	# z
# }
