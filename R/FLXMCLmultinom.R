#' @rdname FLXMCL
#' @aliases FLXMCLmultinom-class
#'
#' @family mixtures multinom
#'
#' @import flexmix
#' @export

setClass("FLXMCLmultinom", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} implementing mixtures of Multinomial Regression Models.
#'
#' @title Mixtures of Multinomial Regression Models
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param censored If the response is a matrix with more than two columns, interpret the entries as one for possible classes, zero for impossible classes, rather than as counts (see \code{\link[nnet]{multinom}}).
#' @param \dots Further arguments to and from other methods, especially \code{\link[nnet]{multinom}}.
#'
#' @return Returns an object of class \code{FLXMCLmultinom} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLmultinom
# @aliases FLXMCLmultinom
#'
#' @family mixtures multinom
#'
#' @import flexmix nnet
#' @export
#'
#' @examples
#' library(benchData)
#' data <- flashData(1000)
#' data$x <- scale(data$x)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLmultinom(trace = FALSE)
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


FLXMCLmultinom <- function(formula = . ~ ., censored = FALSE, ...) {
	z <- new("FLXMCLmultinom", weighted = TRUE, formula = formula,
		name = "Mixture of multinom models")
	z@defineComponent <- expression({
		predict <- function(x) {
# FIXME: does this work if y originally was a matrix?
			post <- getS3method("predict", "nnet")(fit, newdata = x)
			lev <- fit$lev												# contains all levels
			ng <- length(lev)
			if (ncol(post) == 1) {										# 2-class case, length(fit$lev1) == 2
				post <- cbind(1-post, post)
				colnames(post) <- fit$lev1
			}
			if (ng > ncol(post)) {										# missing levels in components
				posterior <- matrix(0, nrow(post), ng)
	        	rownames(posterior) <- rownames(post)
	        	colnames(posterior) <- lev
	        	posterior[,colnames(post)] <- post
				return(posterior)		
			} else {
				return(post)
			}
		}
		logLik <- function(x, y) {
			post <- fitted(fit)
			lev <- attr(y, "lev")
			if (!is.null(lev)) {							# y factor
				if (ncol(post) == 1) {
					post <- cbind(1-post, post)
					colnames(post) <- fit$lev1
				}
				ll <- post[cbind(rownames(post), as.character(y))]
			} else {										# y matrix
				if (ncol(post) == 1) {
    				post <- cbind(1-post, post)				# post corresponds to higher factor level
    				ll <- post[cbind(1:nrow(x), y + 1)]			# y in {0,1}; y == 1 iff second level, 0 otherwise
				} else {
    				ll <- t(post)[as.logical(t(y))]
				}
			}
	    	ll <- ifelse(ll == 0, -10000, log(ll))
	    	return(list(lpost = ll, reg = sum(-fit$decay*fit$wts^2))) ## FIXME: faktor 0.5?????
			# post <- fitted(fit)		## nrow(post) <= nrow(x) because observations with zero weight are removed for training
# # print(head(post))
# # print(head(y))
			# n <- nrow(x)
			# ll <- rep(-100000, n)
			# lev <- attr(y, "lev")
			# if (!is.null(lev)) {							# y factor
				# if (ncol(post) == 1) {
					# post <- cbind(1-post, post)
					# colnames(post) <- fit$lev1
				# }
	    		# ng <- length(lev)
# # print(head(post))
				# if (ng > ncol(post)) {
					# l <- rep(0, nrow(post))
					# col.index <- match(y[fit$ind], colnames(post), 0)
					# row.index <- which(col.index > 0)
					# l[row.index] <- post[cbind(row.index, col.index[row.index])]
				# } else {
					# l <- post[cbind(rownames(post), as.character(y[fit$ind]))]
				# }
			# } else {										# y matrix
				# if (ncol(post) == 1) {
    				# post <- cbind(1-post, post)				# post corresponds to higher factor level
    				# l <- post[cbind(1:n, y[fit$ind] + 1)]	# y in {0,1}; y == 1 iff second level, 0 otherwise
				# } else {
    				# l <- t(post)[as.logical(t(y[fit$ind,]))]
				# }
			# }
# # print(head(post))
	    	# l <- ifelse(l == 0, -10000, log(l))
	    	# ll[fit$ind] <- l
# # print(a <- sum(fit$weights*ll[fit$ind]))
# # print(b <- sum(-fit$decay*fit$wts^2))
# # print(a + b)
	    	# return(list(lpost = ll, reg = sum(-fit$decay*fit$wts^2)))
# cat("y\n", y, "\n")
# print(head(post))
# print(head(y))
# print(head(ll))
  		}
		new("FLXcomponent", parameters = list(wts = fit$wts, entropy = fit$entropy, softmax = fit$softmax, 
			mask = fit$mask, decay = fit$decay, censored = fit$censored), 
			logLik = logLik, predict = predict, df = fit$df)
	})
    z@preproc.y <- function(Y){				# Y results from model.response, can be matrix or factor or ...
    	if (!is.matrix(Y)) {
			if (!is.factor(Y))
				warning("'Y' was coerced to a factor")
			Y <- as.factor(Y)
			lev <- levels(Y)
			Y <- as.matrix(Y)
			attr(Y, "lev") <- lev
        }
		return(Y)
    }
    z@fit <- function(x, y, w) {
## FIXME: scale w
    	class.ind <- function(cl) {
        	n <- length(cl)
        	x <- matrix(0, n, length(levels(cl)))
        	x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        	dimnames(x) <- list(names(cl), levels(cl))
        	x
    	}
    	offset <- attr(x, "offset")
    	lev <- lev1 <- attr(y, "lev")
# print("weight sum")
# print(tapply(as.vector(w), factor(y, levels = lev), sum))
# print(as.vector(w))
    	# remove observations with zero weight
    	# ind <- w > 0
    	# if (length(offset) > 0L) {
    		# if (is.matrix(offset))
    			# offset <- offset[ind,]
			# else
				# offset <- offset[ind]
    	# }
    	# x <- x[ind,]
   		# y <- y[ind,]
    	# w <- w[ind]
    	# 
		if (!is.null(lev)) {
			y <- factor(y, levels = lev)
			# remove observations with zero weight
       		counts <- table(y)
        	if (any(counts == 0L)) {
            	empty <- lev[counts == 0L]
            	warning(sprintf(ngettext(length(empty), "group %s is empty", 
                	"groups %s are empty"), paste(sQuote(empty), 
                	collapse = " ")), domain = NA)
				lev1 <- lev[counts > 0L]
            	y <- factor(y, levels = lev1)
            	if (length(offset) > 0 && is.matrix(offset)) {
					# remove columns that corresponds to empty classes
            		offset <- offset[,counts > 0]
            	}
        	}
        	if (length(lev1) < 2L) 
				stop("need two or more classes to fit a multinom model")
			if (length(lev1) == 2L) 
				y <- as.vector(unclass(y)) - 1
			else
				y <- class.ind(y)
		}
		r <- ncol(x)
		if (is.matrix(y)) {
			# counts <- colSums(y)
			# remove columns of y and offset belonging to empty classes
			# stop if only one remains
			# how asses which are the remaining ones?
			p <- ncol(y)	# number of classes
			sY <- y %*% rep(1, p)
			if (any(sY == 0)) 
				stop("some case has no observations")
			if (!censored) {
				y <- y/matrix(sY, nrow(y), p)
				w <- w * sY
        	}
        	if (length(offset) > 1L) {
     	       if (ncol(offset) != p) 
     	           stop("ncol(offset) is wrong")
    	        mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
    	            r), rep(FALSE, p)), p - 1L))
				x <- cbind(x, offset)
				Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
				fit <- getS3method("nnet", "default")(x, y, w, Wts = Wts, mask = mask, 
					size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
					rang = 0, ...)
			} else {
				mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
					r)), p - 1L))
				fit <- getS3method("nnet", "default")(x, y, w, mask = mask, size = 0, 
					skip = TRUE, softmax = TRUE, censored = censored, 
					rang = 0, ...)
			}
		} else {
			if (length(offset) <= 1L) {
				mask <- c(FALSE, rep(TRUE, r))
				fit <- getS3method("nnet", "default")(x, y, w, mask = mask, size = 0, 
					skip = TRUE, entropy = TRUE, rang = 0, ...)
			} else {
				mask <- c(FALSE, rep(TRUE, r), FALSE)
				Wts <- c(rep(0, r + 1L), 1)
				x <- cbind(x, offset)
				fit <- getS3method("nnet", "default")(x, y, w, Wts = Wts, mask = mask, 
					size = 0, skip = TRUE, entropy = TRUE, rang = 0, ...)
			}
		}
    # fit$formula <- as.vector(attr(Terms, "formula"))
    # fit$terms <- Terms
    # fit$call <- call
    # fit$weights <- w
    # fit$lev <- lev
    # fit$deviance <- 2 * fit$value
    # fit$rank <- Xr
    # edf <- ifelse(length(lev) == 2L, 1, length(lev) - 1) * Xr
    # if (is.matrix(Y)) {
        # edf <- (ncol(Y) - 1) * Xr
        # if (length(dn <- colnames(Y)) > 0) 
            # fit$lab <- dn
        # else fit$lab <- 1L:ncol(Y)
    # }
    # fit$coefnames <- colnames(X)
    # fit$vcoefnames <- fit$coefnames[1L:r]
    # fit$na.action <- attr(m, "na.action")
    # fit$contrasts <- cons
    # fit$xlevels <- .getXlevels(Terms, m)
    # fit$edf <- edf
    # fit$AIC <- fit$deviance + 2 * edf
    # if (model) 
        # fit$model <- m
    # class(fit) <- c("multinom", "nnet")
    # if (Hess) 
        # fit$Hessian <- multinomHess(fit, X)
    # fit
		if (!is.null(lev)) {
			fit$lev <- lev
			fit$lev1 <- lev1
		}
		# fit$ind <- ind
		fit$mask <- mask
# fit$weights <- w
# print(fit$value)
# print(fit$decay)
		fit$df = sum(mask)				# number of optimized weights
		class(fit) <- c("multinom", "nnet")
		with(fit, eval(z@defineComponent))
    }
    z
}


#' @rdname FLXMCLmultinom
#' @aliases FLXgetModelmatrix,FLXMCLmultinom-method
#'
#' @family mixtures multinom
#'
#' @import flexmix
#' @export
#'
#' @docType methods

# offset?
setMethod("FLXgetModelmatrix", signature(model = "FLXMCLmultinom"), 
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
# print(str(modely))
        model@y <- model@preproc.y(modely)
# cat("attributes(modely)\n")
# print(attributes(modely))
        # a <- is.matrix(modely)
        # model@y <- modely
# cat("attributes(model@y)\n")
# print(attributes(model@y))
        # attr(model@y, "lev") <- attr(modely, "lev")
# print(str(model@y))
        # attr(model@y, "is.matrix") <- a
# cat("attributes(model@y)\n")
# print(attributes(model@y))
    }
    else {
        mt1 <- if (is.null(model@terms)) 
            terms(model@fullformula, data = data)
        else model@terms
        mf <- model.frame(delete.response(mt1), data = data, 
            na.action = NULL)
        model@terms <- attr(mf, "terms")  
        # offset <- model.offset(mf)
    }
    X <- model.matrix(model@terms, data = mf)
	offset <- model.offset(mf)
	# r <- ncol(X)
    # if (attr(model@y, "is.matrix")) {
		# p <- ncol(model@y)
        # # sY <- Y %*% rep(1, p)
        # # if (any(sY == 0)) 
            # # stop("some case has no observations")
        # # if (!censored) {
            # # Y <- Y/matrix(sY, nrow(Y), p)
            # # w <- w * sY
        # # }
        # if (length(offset) > 1L) {
            # if (ncol(offset) != p) 
                # stop("ncol(offset) is wrong")
            # mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
                # r), rep(FALSE, p)), p - 1L))
            # X <- cbind(X, offset)
            # # Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
            # # fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                # # size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
                # # rang = 0, ...)
        # }
        # else {
            # mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
                # r)), p - 1L))
            # # fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                # # skip = TRUE, softmax = TRUE, censored = censored, 
                # # rang = 0, ...)
        # }
        # attr(X, "softmax") <- TRUE
        # attr(X, "entropy") <- FALSE
    # } else {
        # if (length(offset) <= 1L) {
            # mask <- c(FALSE, rep(TRUE, r))
            # # fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                # # skip = TRUE, entropy = TRUE, rang = 0, ...)
        # }
        # else {
            # mask <- c(FALSE, rep(TRUE, r), FALSE)
            # # Wts <- c(rep(0, r + 1L), 1)
            # X <- cbind(X, offset)
            # # fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                # # size = 0, skip = TRUE, entropy = TRUE, rang = 0, 
                # # ...)
        # }
        # attr(X, "softmax") <- FALSE
        # attr(X, "entropy") <- TRUE
    # }
    # attr(X, "mask") <- mask
    model@contrasts <- attr(X, "contrasts")
    model@x <- X
    model@x <- model@preproc.x(model@x)
	attr(model@x, "offset") <- offset
    model@xlevels <- .getXlevels(model@terms, mf)
    model
})





### old version
# FLXMCLmultinom <- function(formula = . ~ ., ...) {
	# z <- new("FLXMCLmultinom", weighted = TRUE, formula = formula,
		# name = "Mixture of multinom models")
	# z@defineComponent <- expression({
		# predict <- function(x, ...) {
			# post <- getS3method("predict", "nnet")(fit, newdata = x)
# # cat("post1\n")			
# # print(post)			
			# if (ncol(post) == 1 && length(fit$lev) == 2) {  # fit$lev = NULL?
# # print("no matrix")
    			# post <- cbind(1-post, post)
    			# colnames(post) <- fit$lev
    		# }			
# # cat("post2\n")			
# # print(post)
			# return(post)
		# }
		# logLik <- function(x, y, ...) {
			# post <- fitted(fit)
			# n <- nrow(post)
			# if (ncol(post) == 1) {
    			# post <- cbind(1-post, post)	# post second level
    			# ll <- post[cbind(1:n, y + 1)] # y in {0,1}; y == 1 iff second level, 0 otherwise
			# } else {
    			# ll <- t(post)[as.logical(t(y))]
			# }
	    	# ll <- ifelse(ll == 0, -10000, log(ll))
# cat("ll\n")
# print(sum(fit$weights * ll))
			# return(ll)
  		# }
		# new("FLXcomponent", parameters = list(wts = fit$wts, softmax = fit$softmax, entropy = fit$entropy), 
			# logLik = logLik, predict = predict, df = fit$df)
	# })
    # z@preproc.y <- function(Y){ # Y results from model.response
    	# class.ind <- function(cl) {
        	# n <- length(cl)
        	# x <- matrix(0, n, length(levels(cl)))
        	# x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        	# dimnames(x) <- list(names(cl), levels(cl))
        	# x
    	# }
    	# if (!is.matrix(Y)) 
        	# Y <- as.factor(Y)
    	# lev <- levels(Y)
    	# if (is.factor(Y)) {
        	# counts <- table(Y)
        	# if (any(counts == 0L)) {
            	# empty <- lev[counts == 0L]
            	# warning(sprintf(ngettext(length(empty), "group %s is empty", 
               		# "groups %s are empty"), paste(sQuote(empty), 
                	# collapse = " ")), domain = NA)
            	# Y <- factor(Y, levels = lev[counts > 0L])
            	# lev <- lev[counts > 0L]
        	# }
        	# if (length(lev) < 2L) 
            	# stop("need two or more classes to fit a multinom model")
        	# if (length(lev) == 2L) 
            	# Y <- as.vector(unclass(Y)) - 1
        	# else Y <- class.ind(Y)
        	# attr(Y, "lev") <- lev
    	# }	
    	# if (is.matrix(Y)) {
        	# p <- ncol(Y)
        	# sY <- Y %*% rep(1, p)
        	# if (any(sY == 0)) 
            	# stop("some case has no observations")
 		# }
	 	# return(Y)
    # }
    # z@fit <- function(x, y, w, ...) {
# # cat("y\n")
# # print(y)
# # cat("attr(y, lev)\n")
# # print(lev)
# # cat("mask\n")
# # print(attr(x, "mask"))
# # cat("softmax\n")
# # print(attr(x, "softmax"))
# # cat("entropy\n")
# # print(attr(x, "entropy"))
# #		w <- w/sum(w) * nrow(x)
        # fit <- getS3method("nnet", "default")(x, y, w, mask = attr(x, "mask"), size = 0, skip = TRUE, 
            # softmax = attr(x, "softmax"), entropy = attr(x, "entropy"), 
            # rang = 0, trace = FALSE, ...)
# print(fit$wts)
# print(fit$convergence)
		# lev <- attr(y, "lev")
# #print(lev)
		# if (!is.null(lev))
			# fit$lev <- lev
		# fit$df = length(fit$wts)
# fit$weights <- w
		# class(fit) <- c("multinom", "nnet")
# cat("value\n")
# print(fit$value)
		# with(fit, eval(z@defineComponent))
    # }
    # z
# }



# #' @rdname FLXMCLmultinom
# #' @aliases FLXgetModelmatrix,FLXMCLmultinom-method
# #'
# #' @import flexmix
# #' @export
# #'
# #' @docType methods

# # offset?
# setMethod("FLXgetModelmatrix", signature(model = "FLXMCLmultinom"), 
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
        # modely <- model@preproc.y(modely)
# # cat("attributes(modely)\n")
# # print(attributes(modely))
        # a <- is.matrix(modely)
        # model@y <- as.matrix(modely)
# # cat("attributes(model@y)\n")
# # print(attributes(model@y))
        # attr(model@y, "lev") <- attr(modely, "lev")
        # attr(model@y, "is.matrix") <- a
# # cat("attributes(model@y)\n")
# # print(attributes(model@y))
    # }
    # else {
        # mt1 <- if (is.null(model@terms)) 
            # terms(model@fullformula, data = data)
        # else model@terms
        # mf <- model.frame(delete.response(mt1), data = data, 
            # na.action = NULL)
        # model@terms <- attr(mf, "terms")  
        # offset <- model.offset(mf)
    # }
    # X <- model.matrix(model@terms, data = mf)
	# r <- ncol(X)
    # if (attr(model@y, "is.matrix")) {
		# p <- ncol(model@y)
        # # sY <- Y %*% rep(1, p)
        # # if (any(sY == 0)) 
            # # stop("some case has no observations")
        # # if (!censored) {
            # # Y <- Y/matrix(sY, nrow(Y), p)
            # # w <- w * sY
        # # }
        # if (length(offset) > 1L) {
            # if (ncol(offset) != p) 
                # stop("ncol(offset) is wrong")
            # mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
                # r), rep(FALSE, p)), p - 1L))
            # X <- cbind(X, offset)
            # # Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
            # # fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                # # size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
                # # rang = 0, ...)
        # }
        # else {
            # mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
                # r)), p - 1L))
            # # fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                # # skip = TRUE, softmax = TRUE, censored = censored, 
                # # rang = 0, ...)
        # }
        # attr(X, "softmax") <- TRUE
        # attr(X, "entropy") <- FALSE
    # } else {
        # if (length(offset) <= 1L) {
            # mask <- c(FALSE, rep(TRUE, r))
            # # fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                # # skip = TRUE, entropy = TRUE, rang = 0, ...)
        # }
        # else {
            # mask <- c(FALSE, rep(TRUE, r), FALSE)
            # # Wts <- c(rep(0, r + 1L), 1)
            # X <- cbind(X, offset)
            # # fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                # # size = 0, skip = TRUE, entropy = TRUE, rang = 0, 
                # # ...)
        # }
        # attr(X, "softmax") <- FALSE
        # attr(X, "entropy") <- TRUE
    # }
    # attr(X, "mask") <- mask
    # model@contrasts <- attr(X, "contrasts")
    # model@x <- X
    # model@x <- model@preproc.x(model@x)
    # model@xlevels <- .getXlevels(model@terms, mf)
    # model
# })
