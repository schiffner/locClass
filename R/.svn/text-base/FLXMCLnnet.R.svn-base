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
#' @aliases FLXMCLnnet-class
#'
#' @import flexmix
#' @export

setClass("FLXMCLnnet", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} implementing mixtures of Neural Netowrks.
#'
#' @title Mixtures of Neural Networks
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}.
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param size Size of hidden layer, see \code{\link[nnet]{nnet}}.
#' @param skip Logical. Should the neural net include skip layer connections? Defaults to \code{FALSE}, see also \code{\link[nnet]{nnet}}.
#' @param Wts Initial parameter vector. For convenience and since number of nodes in networks can vary, \code{Wts} does not need to be of correct length.
#' 	\code{Wts} should be a rather long vector. Depending on the number of nodes and connections in the network it is recycled or only a subset is used.
#' @param reps Neural networks are fitted repeatedly (\code{reps} times) for different initial values and the solution with largest likelihood
#'  value is kept. Defaults to 1. (\code{reps} larger one does not make sense if \code{Wts} is specified.)
#' @param \dots Further arguments to and from other methods, especially to \code{\link[nnet]{nnet}}.
#'
#' @return Returns an object of class \code{FLXMCLnnet} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLnnet
#' @aliases FLXMCLnnet
#'
#' @import flexmix nnet
#' @export
#'
#' @examples
#' library(locClassData)
#' data <- flashData(1000)
#' data$x <- scale(data$x)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLnnet(size = 1, trace = TRUE, reps = 5, decay = 0.1)
#' fit <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
#' 
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
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
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## local membership
#' loc.grid <- prior(fit, newdata = grid)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)


FLXMCLnnet <- function(formula = . ~ ., size, reps = 1, skip = FALSE, Wts = NULL, ...) {
	z <- new("FLXMCLnnet", weighted = TRUE, formula = formula,
		name = "Mixture of nnet models")
	z@defineComponent <- expression({
		predict <- function(x) {
			post <- getS3method("predict", "nnet")(fit, newdata = x, type = "raw", ...)
			lev <- fit$lev						# contains all levels
			ng <- length(lev)
			if (ncol(post) == 1) {				# 2-class case, length(fit$lev1) == 2
				post <- cbind(1-post, post)
				colnames(post) <- fit$lev1
			}
			if (ng > ncol(post)) {
				posterior <- matrix(0, nrow(post), ng)
	        	rownames(posterior) <- rownames(post)
	        	colnames(posterior) <- lev
	        	posterior[,colnames(post)] <- post
			} else {
				posterior <- post
			}		
			return(posterior)
		}
		logLik <- function(x, y) {
			post <- fitted(fit)
			if (ncol(post) == 1) {
				post <- cbind(1-post, post)
				colnames(post) <- fit$lev1
			}
			ll <- post[cbind(rownames(post), as.character(y))]
			ll <- ifelse(ll == 0, -10000, log(ll))
	    	return(list(lpost = ll, reg = sum(-fit$decay*fit$wts^2)))
			# post <- fitted(fit)		## nrow(post) <= nrow(x) because observations with zero weight are removed during fitting
# # print(head(post))
# # print(head(y))
    		# ng <- length(attr(y, "lev"))
			# if (ncol(post) == 1) {
				# post <- cbind(1-post, post)
				# colnames(post) <- fit$lev1
			# }
# # print(head(post))
			# ll <- rep(-10000, nrow(x))
    		# if (ng > ncol(post)) {
    			# l <- rep(0, nrow(post))
    			# col.index <- match(y[fit$ind], colnames(post), 0)
    			# row.index <- which(col.index > 0)
    			# l[row.index] <- post[cbind(row.index, col.index[row.index])]
    		# } else {
	    		# l <- post[cbind(rownames(post), as.character(y[fit$ind]))]
	    	# }
# # print(head(post))
	    	# l <- ifelse(l == 0, -10000, log(l))
	    	# ll[fit$ind] <- l
# # print(a <- sum(fit$weights * ll[fit$ind]))
# # print(b <- sum(-fit$decay * fit$wts^2))
# # print(a + b)
	    	# return(list(lpost = ll, reg = sum(-fit$decay*fit$wts^2)))
		}
		new("FLXcomponent", parameters = list(wts = fit$wts, softmax = fit$softmax, entropy = fit$entropy, 
			decay = fit$decay, n = fit$n, censored = fit$censored, reps = fit$reps), logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(y) { # y results from model response
    	if (!is.factor(y))
    		warning("currently only classification networks are supported and 'y' was coerced to a factor")
	    g <- as.factor(y)
	    lev <- levels(g)
    	g <- as.matrix(g)
    	attr(g, "lev") <- lev
		return(g)
	}
	z@fit <- function(x, y, w) {
    	class.ind <- function(cl) {
        	n <- length(cl)
        	x <- matrix(0, n, length(levels(cl)))
        	x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        	dimnames(x) <- list(names(cl), levels(cl))
        	x
    	}
    	# coerce y to a factor
       	lev <- lev1 <- attr(y, "lev")
		y <- factor(y, levels = lev)
# print("weight sum")
# print(tapply(as.vector(w), factor(y, levels = lev), sum))
		# remove observations with zero weight
		# ind <- w > 0
		# y <- y[ind]
		# x <- x[ind,,drop = FALSE]
		# w <- w[ind]
		# are classes missing?
       	counts <- table(y)
		if (any(counts == 0L)) {
			lev1 <- lev[counts > 0]
			empty <- lev[counts == 0L]
			warning(sprintf(ngettext(length(empty), "group %s is empty", 
				"groups %s are empty"), paste(empty, collapse = " ")), 
				domain = NA)
			y <- factor(y, levels = lev1)
		}
		if (length(lev1) == 2L) {
			y <- as.matrix(unclass(y)) - 1 ## original: as.vector
			if (!is.null(Wts)) {
				net <- list()
				net$n <- c(dim(x)[2L], size, dim(y)[2L])
				net$nunits <- as.integer(1L + sum(net$n))
				net$nconn <- rep(0, net$nunits + 1L)
				net$conn <- numeric(0L)
				net <- nnet:::norm.net(net)
				if (skip) 
					net <- nnet:::add.net(net, seq(1L, net$n[1L]), seq(1L + net$n[1L] + net$n[2L], net$nunits - 1L))
				nwts <- length(net$conn)
				Wts <- rep(Wts, length.out = nwts)
				fit <- nnet(x, y, w, entropy = TRUE, Wts = Wts, size = size, skip = skip, ...)
# print(fit$n)
# print(fit$wts)
# print(Wts)
				# attr(y, "lev") <- lev
				# attr(y, "entropy") <- TRUE
				# attr(y, "softmax") <- FALSE
			} else {
				fit <- nnetRep(reps, x = x, y = y, weights = w, entropy = TRUE, size = size, skip = skip, ...)
				# fit <- nnet(x, y, w, entropy = TRUE, size = size, skip = skip, ...)
# print(fit$n)
# print(fit$wts)
				# attr(y, "lev") <- lev
				# attr(y, "entropy") <- TRUE
				# attr(y, "softmax") <- FALSE
			}
		} else {
			y <- class.ind(y)
			if (!is.null(Wts)) {
				net <- list()
				net$n <- c(dim(x)[2L], size, dim(y)[2L])
				net$nunits <- as.integer(1L + sum(net$n))
				net$nconn <- rep(0, net$nunits + 1L)
				net$conn <- numeric(0L)
				net <- nnet:::norm.net(net)
				if (skip) 
					net <- nnet:::add.net(net, seq(1L, net$n[1L]), seq(1L + net$n[1L] + net$n[2L], net$nunits - 1L))
				nwts <- length(net$conn)
				Wts <- rep(Wts, length.out = nwts)
				fit <- nnet(x, y, w, softmax = TRUE, Wts = Wts, size = size, skip = skip, ...)
# print(fit$n)
# print(fit$wts)
# print(Wts)
				# attr(y, "lev") <- lev
				# attr(y, "entropy") <- FALSE
				# attr(y, "softmax") <- TRUE
			} else {
				fit <- nnetRep(reps, x = x, y = y, weights = w, softmax = TRUE, size = size, skip = skip, ...)
				# fit <- nnet(x, y, w, softmax = TRUE, size = size, skip = skip, ...)
# print(fit$n)
# print(fit$wts)
				# attr(y, "lev") <- lev
				# attr(y, "entropy") <- FALSE
				# attr(y, "softmax") <- TRUE
			}
		}
# print(lev)
# print(counts)
# print(lev1)
    	fit$lev <- lev				## contains all levels
    	fit$lev1 <- lev1			## contains present levels
    	# fit$ind <- ind				## logical, indicating if w > 0
		fit$df <- length(fit$wts)
		fit$reps <- reps
# fit$weights <- w
# print(fit$value)
		with(fit, eval(z@defineComponent))
	}
	z
}



#' @rdname FLXMCLnnet
#' @aliases FLXgetModelmatrix,FLXMCLnnet-method
#'
#' @import flexmix
#' @export
#'
#' @docType methods

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLnnet"), 
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
    attr(model@terms, "intercept") <- 0 ## intercept removed
    X <- model.matrix(model@terms, data = mf)
    model@contrasts <- attr(X, "contrasts")
    model@x <- X
    model@x <- model@preproc.x(model@x)
    model@xlevels <- .getXlevels(model@terms, mf)
    model
})


## old version
# FLXMCLnnet <- function(formula = . ~ ., ...) {
	# z <- new("FLXMCLnnet", weighted = TRUE, formula = formula,
		# name = "Mixture of nnet models")
	# z@defineComponent <- expression({
		# predict <- function(x, ...) {
			# post <- getS3method("predict", "nnet")(fit, newdata = x, type = "raw", ...)
			# if (ncol(post) == 1) {
				# post <- cbind(1-post, post)
				# colnames(post) <- fit$lev
			# }
			# return(post)
		# }
		# logLik <- function(x, y, ...) {
    		# post <- fitted(fit)
# # print(head(post))
# # print(head(y))
			# n <- nrow(post)
			# if (ncol(post) == 1) {
				# post <- cbind(1-post, post)
    			# ll <- post[cbind(1:n, y + 1)] # y in {0,1}; y == 1 iff second level, 0 otherwise
			# } else {
    			# ll <- t(post)[as.logical(t(y))]
			# }
	    	# ll <- ifelse(ll == 0, -10000, log(ll))
			# return(ll)
		# }
		# new("FLXcomponent", parameters = list(wts = fit$wts), 
			# logLik = logLik, predict = predict, df = fit$df)
	# })
	# z@preproc.y <- function(y) { # y results from model response
    	# class.ind <- function(cl) {
        	# n <- length(cl)
        	# x <- matrix(0, n, length(levels(cl)))
        	# x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        	# dimnames(x) <- list(names(cl), levels(cl))
        	# x
    	# }
    	# if (!is.factor(y))
    		# warning("currently only classification networks are supported and 'y' was coerced to a factor")
    	# y <- as.factor(y)
       	# lev <- levels(y)
       	# counts <- table(y)
		# if (any(counts == 0L)) {
			# empty <- lev[counts == 0L]
			# warning(sprintf(ngettext(length(empty), "group %s is empty", 
				# "groups %s are empty"), paste(empty, collapse = " ")), 
				# domain = NA)
			# y <- factor(y, levels = lev[counts > 0L])
		# }
		# if (length(lev) == 2L) { ### wirklich lev oder lev[counts > 0]
			# y <- as.matrix(unclass(y)) - 1 ## original: as.vector
			# attr(y, "lev") <- lev
			# attr(y, "entropy") <- TRUE
			# attr(y, "softmax") <- FALSE
		# }
		# else {
			# y <- class.ind(y)
			# attr(y, "lev") <- lev
			# attr(y, "entropy") <- FALSE
			# attr(y, "softmax") <- TRUE
		# }
    	# return(y)
	# }
	# z@fit <- function(x, y, w) {
		# lev <- attr(y, "lev")
		
		# # counts <- colSums(y[w > 0,, drop = FALSE])
		# # if (any(counts == 0L)) {
			
			
		# # } else {
			
			
		# # }
		
		
# ## nachgucken, ob Klassen komplett fehlen? sollte man evtl. Spalten aus y löschen und da Netz anders anpassen? hat dann auch auswirkungen auf predict und logLik
# print(colSums(y * w))
# #print(lev)
		# # if (is.null(lev)) {
			# # fit <- nnet(x, y, weights = w, ...)
		# # } else {
		# fit <- nnet(x, y, weights = w, entropy = attr(y, "entropy"), softmax = attr(y, "softmax"), ...)
		# fit$lev <- lev
		# # }			
		# fit$df <- length(fit$wts)
		# with(fit, eval(z@defineComponent))
	# }
	# z
# }



# #' @rdname FLXMCLnnet
# #' @aliases FLXgetModelmatrix,FLXMCLnnet-method
# #'
# #' @import flexmix
# #' @export
# #'
# #' @docType methods

# setMethod("FLXgetModelmatrix", signature(model = "FLXMCLnnet"), 
	# function (model, data, formula, lhs = TRUE, ...) {
    # formula <- flexmix:::RemoveGrouping(formula)
    # if (length(grep("\\|", deparse(model@formula)))) 
        # stop("no grouping variable allowed in the model")
    # if (is.null(model@formula)) 
        # model@formula = formula
    # model@fullformula = update(terms(formula, data = data), 
        # model@formula)
    # if (lhs) {
        # mf <- if (is.null(model@terms)) 
            # model.frame(model@fullformula, data = data, na.action = NULL)
        # else model.frame(model@terms, data = data, na.action = NULL)
        # model@terms <- attr(mf, "terms")
        # modely <- model.response(mf)
        # model@y <- model@preproc.y(modely)
    # }
    # else {
        # mt1 <- if (is.null(model@terms)) 
            # terms(model@fullformula, data = data)
        # else model@terms
        # mf <- model.frame(delete.response(mt1), data = data, 
            # na.action = NULL)
        # model@terms <- attr(mf, "terms")    
    # }
    # attr(model@terms, "intercept") <- 0 ## intercept removed
    # X <- model.matrix(model@terms, data = mf)
    # model@contrasts <- attr(X, "contrasts")
    # model@x <- X
    # model@x <- model@preproc.x(model@x)
    # model@xlevels <- .getXlevels(model@terms, mf)
    # model
# })
