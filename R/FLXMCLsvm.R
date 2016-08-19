#' @rdname FLXMCL
# @aliases FLXMCLsvm-class
#'
#' @family mixtures svm
#'
#' @import flexmix
#' @export

setClass("FLXMCLsvm", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} implementing mixtures of Support Vector Machines for classification.
#'
#' @title Mixtures of Support Vector Machines
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} 
#' 	 using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param \dots Further arguments to and from other methods, especially to \code{\link{wsvm}}.
#'
#' @return Returns an object of class \code{FLXMCLsvm} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLsvm
# @aliases FLXMCLsvm
#'
#' @family mixtures svm
#'
#' @import flexmix
#' @export
#'
#' @examples
#' library(benchData)
#' data <- flashData(1000)
#' data$x <- scale(data$x)
#' grid <- expand.grid(x.1 = seq(-6,6,0.2), x.2 = seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLsvm(kernel = "linear", fitted = FALSE)
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

# library(mlr)
# task <- makeClassifTask(data = as.data.frame(data), target = "y")
# lrn <- makeLearner("classif.FLXMCLsvm", kernel = "linear", centers = 2)
# tr <- train(lrn, task = task)
# pr <- predict(tr, newdata = grid)

## report
# getMethod("determinePrior", signature = c("ANY","FLXPmultinom"))
# function (prior, concomitant, group) 
# {
    # exps <- exp(concomitant@x %*% concomitant@coef)
    # exps/rowSums(exps)
# }
# <environment: namespace:flexmix>
# exps kann Inf enthalten, wenn argument groß
# ergebnis ist NaN


FLXMCLsvm <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLsvm", weighted = TRUE, formula = formula,
		name = "Mixture of SVM models")
	z@defineComponent <- expression({
		predict <- function(x) {
			## returns class membership values (exp(-hinge loss)), these are not scaled and need not sum to unity
			nl <- length(fit$labels)							# number of present classes
			lev <- fit$levels
			ng <- length(lev)								# number of classes
			naidx <- apply(x, 1, function(z) any(is.na(z)))	# obs with missing values
			if (nl == 1) {									# no decision values are returned
				posterior <- matrix(0, nrow(x), ng)
	    	    rownames(posterior) <- rownames(x)
    	    	colnames(posterior) <- lev
				posterior[,fit$labels] <- 1					# since the only present class is predicted, set posterior to largest possible value
			} else {
				decs <- attr(getS3method("predict", "wsvm")(fit, newdata = x, decision.values = TRUE, ...), "decision.values")
				problems <- cbind(rep(fit$labels, nl:1-1), unlist(sapply(2:nl, function(x) fit$labels[x:nl])))	# binary classification problems
				classidx <- lapply(fit$labels, function(k) problems == k)					# binary problems where class k is involved
				y <- matrix(sapply(classidx, function(x) colSums(t(x)*c(1,-1))), ncol = nl)	# encoding of class k in individual binary problems
				classidx <- matrix(sapply(classidx, rowSums), ncol = nl)
				mode(classidx) <- "logical"
				colnames(classidx) <- colnames(y) <- fit$labels			
		        posterior <- matrix(0, nrow(x), ng)				# unscaled posteriors
	    	    rownames(posterior) <- rownames(x)
	        	colnames(posterior) <- lev
				post <- sapply(as.character(fit$labels), function(z) {
					H <- 1 - t(y[classidx[,z],z] * t(decs[,classidx[,z], drop = FALSE]))
					H[H < 0] <- 0								# hinge loss
					return(exp(-rowSums(H)))
				})
				posterior[!naidx,fit$labels] <- post
			}
			posterior[naidx,] <- NA
			return(posterior)
		}
		logLik <- function(x, y) {
# fit <- wsvm(Species ~ ., data = iris[1:100,])
# y <- iris$Species[1:100]
# l <- fit$decision.values
# library(mlbench)
# data(Glass)
# fit <- wsvm(Type ~ ., data = Glass)
# y <- Glass$Type
# l <- fit$decision.values
#lev <- fit$levels
#ng <- length(lev)
			y <- factor(y, levels = attr(y, "lev"))
			nl <- length(fit$labels)							# number of present classes
			if (nl == 1) {
				lpost <- rep(0, length(y))
				reg <- 0
			} else {
				l <- attr(getS3method("predict", "wsvm")(fit, newdata = x, decision.values = TRUE, ...), "decision.values")
# print(summary(l))
				ng <- length(fit$levels)						# number of classes
				problems <- cbind(rep(fit$labels, nl:1-1), unlist(sapply(2:nl, function(x) fit$labels[x:nl])))	# labels involved in particular binary problems
				npr <- nl*(nl-1)/2								# number of binary classification problems
				m <- matrix(NA, npr, ng)
				m[cbind(1:npr,problems[,1])] <- 1
				m[cbind(1:npr,problems[,2])] <- -1				# y coding matrix
				yind <- t(m[, as.numeric(y), drop = FALSE])		# -1/1 class indicators for binary problems, n x npr matrix
				lpost <- 1 - yind * l
				lpost[lpost < 0] <- 0							# Hinge loss max(1 - yind*l, 0), n x npr matrix
				lpost <- rowSums(-lpost, na.rm = TRUE)			# sum of negative Hinge loss over all binary problems = log posterior, n x 1 matrix
#print(lpost)
	# print(str(lpost))
	# plot(yind,l)
	# print(cbind(y,yind,l)[sample(nrow(x), size = 30),])
				co <- t(yind) * 0
				co[,fit$index][!is.na(co[,fit$index])] <- t(fit$coefs)		# coefficients: alpha_n * y_n, npr x n matrix
				lambda <- 1/(2 * fit$cost)									# regularization parameter
				reg <- lambda * t(co * (t(l) - fit$rho))					# n x npr matrix
				# -fit$rho is correct, because obj is -rowSums(abs(co), na.rm = TRUE) + 0.5 * colSums(t(co) * t((t(l) - fit$rho)), na.rm = TRUE)
				reg <- sum(-reg, na.rm = TRUE)					# sum of regularization terms over all binary problems
			}
# cat("sum alpha\n")
# print(rowSums(abs(co), na.rm = TRUE))
# cat("regularization\n")
# print(reg)
# cat("negative hinge loss\n")
# print(sum(fit$case.weight * lpost, na.rm = TRUE))
# cat("loglik\n")
# print(sum(fit$case.weight * lpost, na.rm = TRUE) + reg)
# cat("obj\n")
# print(sum(fit$obj))
# print(-rowSums(abs(co), na.rm = TRUE) + 0.5 * colSums(t(co) * t((t(l) - fit$rho)), na.rm = TRUE))
			return(list(lpost = lpost, reg = reg))
		}
		new("FLXcomponent", parameters = list(coefs = fit$coefs, kernel = fit$kernel, cost = fit$cost, 
			degree = fit$degree, coef0 = fit$coef0, gamma = fit$gamma, fitted = !is.null(fit$fitted)), logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(y) {
    	if (!is.factor(y))
        	warning("'grouping' was coerced to a factor")
	    g <- as.factor(y)
	    lev <- levels(g)
    	g <- as.matrix(g)
    	attr(g, "lev") <- lev
		return(g)
		# if (is.factor(y)) {
			# lev <- levels(y)
			# y <- as.matrix(y)
			# attr(y, "lev") <- lev
			# return(y)
		# } else
			# return(as.matrix(y))
	}
	z@fit <- function(x, y, w) {
		lev <- attr(y, "lev")
		# if (is.null(lev))
			# fit <- wsvm(x, as.vector(y), case.weights = w, ...)
		# else
#print(head(w,30))
		fit <- wsvm(x, factor(y, levels = lev), case.weights = w, ...)
		fit$df <- sum(fit$nSV)
		with(fit, eval(z@defineComponent))
	}
	z
}



#' @rdname FLXMCLsvm
# @aliases FLXgetModelmatrix,FLXMCLsvm-method
#'
#' @family mixtures svm
#'
#' @import flexmix
#' @export

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLsvm"), 
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
