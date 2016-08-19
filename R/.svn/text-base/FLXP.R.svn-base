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


## FLXPmultinom

# Methods for Class FLXPmultinom
#
# @param object An object of class "FLXPmultinom".
# @param newdata Dataframe containing new data.
# @param \dots ...
#
# @import flexmix
# @export
# 
# @rdname FLXPmultinom-methods
# @aliases predict,FLXPmultinom-method
#
# @docType methods
#
# @usage \S4method{predict}{object = "FLXPmultinom"}(object, newdata, ...)

# setMethod("predict", signature(object = "FLXPmultinom"), function(object, newdata, ...) {
	# # model <- FLXgetModelmatrix(object, newdata, ...)
	# mf <- model.frame(object@formula, data = newdata, na.action = NULL)
	# terms <- attr(mf, "terms")
	# x <- model.matrix(terms, data = mf)
	# coefx <- apply(object@coef, 2, function(z) x %*% z)
	# probs <- exp(coefx)/rowSums(exp(coefx))
	# probs
# })



# @param object An object of class "FLXPmultinom".
# @param \dots ...
#
# @import flexmix
# @export
# 
# @rdname FLXPmultinom-methods
# @aliases fitted,FLXPmultinom-method
#
# @docType methods
#
# @usage \S4method{fitted}{object = "FLXPmultinom"}(object, ...)

# setMethod("fitted", signature(object = "FLXPmultinom"), function(object, ...) {
	# coefx <- apply(object@coef, 2, function(z) object@x %*% z)
	# probs <- exp(coefx)/rowSums(exp(coefx))
	# probs
# })


# pr <- predict(res@concomitant, newdata = as.data.frame(d))
# plot(d$x, col = d$y, cex = pr[,1])
# plot(d$x, col = d$y, cex = pr[,2])


#' @noRd
determinePrior <- flexmix:::determinePrior



#' @noRd
setMethod("determinePrior", signature(concomitant = "FLXPmultinom"), function (prior, concomitant, group) {
	x <- concomitant@x %*% concomitant@coef
	m <- apply(x, 1, max)
	exps <- exp(x - m)
	exps/rowSums(exps)
})

#================================================================
## FLXPwlda

# Concomitant model class.
#
# @title Class "FLXPwlda"
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
# @rdname FLXPwlda-class
# @aliases FLXPwlda-class
#
# @import flexmix
# @export
#
# @docType class
#
# @keywords class
#
# @seealso \code{\link[flexmix]{FLXP-class}}.

# setClass("FLXPwlda", contains = "FLXP")



# Creator function for the concomitant variable model. Priors are modeled by Linear Discriminant Analysis.
#
# @title Creator Function for the Concomitant Variable Model based on Linear Discriminant Analysis
#
# @param formula A formula for determining the model matrix of the concomitant variables.
#
# @return Object of class \code{FLXPwlda} which extends class \code{FLXP} directly and is used for method dispatching.
#
# @import flexmix
# @export
#
# @rdname FLXPwlda
# @aliases FLXPwlda
#
# @seealso \code{\link[flexmix]{FLXPmultinom}}.
#
# @examples
# library(locClassData)
# data <- flashData(1000)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
# 
# cluster <- kmeans(data$x, center = 2)$cluster
# model <- FLXMCLlda()
# fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster) 
#
# ## prediction for single component models without aggregation
# pred.grid <- predict(fit, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
# points(data$x, pch = as.character(data$y))
# 
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)
# points(data$x, pch = as.character(data$y))
# 
# ## prediction with aggregation depending on membership in mixture components
# pred.grid <- mypredict(fit, newdata = grid, aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
# points(data$x, pch = as.character(data$y))
# 
# ## local membership
# loc.grid <- prior(fit, newdata = grid)
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)

# FLXPwlda <- function (formula = ~.) {
    # z <- new("FLXPwlda", name = "FLXPwlda", formula = formula)
	# z@fit <- function(x, y, w, ...) {
		# ## returns unormalized posteriors !!!
		# if (missing(w) || is.null(w)) 
            # w <- rep(1, nrow(y))
        # nc <- ncol(y)
 		# y <- factor(max.col(y), levels = seq_len(nc))
        # fit <- wlda(x, y, w, ...)
		# lev1 <- names(fit$prior)
   		# post <- matrix(0, ncol = nc, nrow = nrow(x), dimnames = list(rownames(x), seq_len(nc)))
   		# post[,lev1] <- sapply(lev1, function(z) fit$prior[z] * dmvnorm(x, fit$means[z,], fit$cov))
    	# return(post)
		# ## old version
		# # if (missing(w) || is.null(w)) 
            # # w <- rep(1, nrow(y))
        # # nc <- ncol(y)
 		# # y <- factor(max.col(y), levels = seq_len(nc))
        # # fit <- wlda(x, y, w, ...)
        # # pred <- predict(fit)
        # # if (nc > ncol(pred$posterior)) {
        	# # posterior <- matrix(0, nrow(pred$posterior), nc)
        	# # rownames(posterior) <- rownames(pred$posterior)
        	# # colnames(posterior) <- seq_len(nc)
        	# # posterior[,colnames(pred$posterior)] <- pred$posterior
        # # } else
        	# # posterior <- pred$posterior
        # # return(posterior)
	# }
    # z@refit <- function(x, y, w, ...) {
        # if (missing(w) || is.null(w)) 
            # w <- rep(1, nrow(y))
 		# y <- factor(max.col(y))
        # fit <- wlda(x, y, w, ...)
        # fit$weights <- w
        # class(fit) <- NULL
        # as.matrix(fit)
    # }
    # z
# }



# #' @noRd
# setMethod("determinePrior", signature(concomitant="FLXPwlda"), function(prior, concomitant, group) {
	# fit <- concomitant@coef
	# listnames <- rownames(fit)
	# attr(fit, "dim") <- NULL	
	# names(fit) <- listnames
	# class(fit) <- "wlda"
	# probs <- getS3method("predict", "wlda")(fit, newdata = concomitant@x)$posterior
	# probs
# })



# # Methods for Class FLXPwlda
# #
# # @param model An object of class "FLXPwlda".
# # @param data ...
# # @param groups ...
# # @param lhs ...
# # @param \dots ...
# #
# # @import flexmix
# # @export
# # 
# # @rdname FLXPwlda-methods
# # @aliases FLXgetModelmatrix,FLXPwlda-method
# #
# # @docType methods
# #
# # @usage \S4method{FLXgetModelmatrix}{object = "FLXPwlda"}(model, data, groups, lhs, ...)


# @rdname FLXPwlda
# @aliases FLXgetModelmatrix,FLXPwlda-method
#
# @import flexmix
# @export
#
# @docType methods

# setMethod("FLXgetModelmatrix", signature(model = "FLXPwlda"), function (model, data, groups, lhs, ...) {
    # mt <- terms(model@formula, data = data)
	# attr(mt, "intercept") <- 0	# important: delete intercept
    # mf <- model.frame(delete.response(mt), data = data, na.action = NULL)
    # X <- model.matrix(mt, data = mf)
    # if (nrow(X)) {
    	# if (!flexmix:::checkGroup(X, groups$group)) 
            # stop("model variables have to be constant for grouping variable")
        # model@x <- X[groups$groupfirst, , drop = FALSE]
    # } else {
        # model@x <- matrix(1, nrow = sum(groups$groupfirst))
    # }
    # model
# })



# @param object An object of class "FLXPwlda".
# @param newdata Dataframe containing new data.
# @param \dots ...
#
# @import flexmix
# @export
# 
# @rdname FLXPwlda-methods
# @aliases predict,FLXPwlda-method
#
# @docType methods
#
# @usage \S4method{predict}{object = "FLXPwlda"}(object, newdata, ...)

# setMethod("predict", signature(object="FLXdist"),
# function(object, newdata=list(), aggregate=FALSE, ...){
    # if (missing(newdata)) return(fitted(object, aggregate=aggregate, drop=FALSE))
    # x = list()
    # for(m in seq_along(object@model)) {
      # comp <- lapply(object@components, "[[", m)
      # x[[m]] <- predict(object@model[[m]], newdata, comp, ...)
    # }
    # if (aggregate) {
      # prior_weights <- prior(object, newdata)
      # z <- lapply(x, function(z) matrix(rowSums(do.call("cbind", z) * prior_weights), nrow = nrow(z[[1]])))
    # }
    # else {
      # z <- list()
      # for (k in seq_len(object@k)) {
        # z[[k]] <- do.call("cbind", lapply(x, "[[", k))
      # }
      # names(z) <- paste("Comp", seq_len(object@k), sep=".")
    # }
    # z
# })


# @param object An object of class "FLXPwlda".
# @param \dots ...
#
# @import flexmix
# @export
# 
# @rdname FLXPwlda-methods
# @aliases fitted,FLXPwlda-method
#
# @docType methods
#
# @usage \S4method{fitted}{object = "FLXPwlda"}(object, ...)

# setMethod("fitted", signature(object = "FLXPwlda"), function(object, ...) {
	# fit <- object@coef
	# listnames <- rownames(fit)
# #print(attributes(fit))	
	# attr(fit, "dim") <- NULL	
# #print(attributes(fit))
	# names(fit) <- listnames
	# class(fit) <- "wlda"
# #print(class(fit))
	# probs <- getS3method("predict", "wlda")(fit, newdata = object@x)$posterior
	# probs
# })
