#' @rdname FLXMCL
# @aliases FLXMCLqda-class
#' 
#' @family mixtures qda
#'
#' @import flexmix
#' @export

setClass("FLXMCLqda", contains = "FLXMCL")



#' @title Mixtures of Quadratic Discriminant Analysis Models
#'
#' @description This is a model driver for \code{\link[flexmix]{flexmix}} from package \pkg{flexmix} implementing mixtures of Quadratic Discriminant Analysis Models.
#'
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
# @param method Method for scaling the pooled weighted covariance matrix, either \code{"unbiased"} or maximum-likelihood (\code{"ML"}). 
#   Defaults to \code{"unbiased"}.
#' @param \dots Further arguments to and from other methods.
#'
#' @return Returns an object of class \code{FLXMCLqda} inheriting from \code{FLXMCL}.
#'
#' @note This method internally calls function \code{\link{wqda}}. \code{method = "ML"} is hard-coded.
#'
#' @rdname FLXMCLqda
#'
# @aliases FLXMCLqda
#'
#' @family mixtures qda
#'
#' @import flexmix
#'
#' @export
#'
#'
#' @examples
#' library(benchData)
#' data <- flashData(1000)
#' x1 <- seq(-6,6,0.2)
#' x2 <- seq(-4,4,0.2)
#' grid <- expand.grid(x.1 = x1, x.2 = x2)
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLqda()
#' fit <- flexmix(y ~ ., data = as.data.frame(data), model = model, cluster = cluster, control = list(verb = 1))
#' 
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
#'
#' # joint density of predictors and class variable for class 1
#' image(x1, x2, matrix(pred.grid[[1]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[1]][,1], length(x1)), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' image(x1, x2, matrix(pred.grid[[2]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[2]][,1], length(x1)), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' # posterior probability of class 1
#' pred.grid <- lapply(pred.grid, function(x) x/rowSums(x))
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
#'
#' # joint density of predictors and class variable for class 1
#' image(x1, x2, matrix(pred.grid[[1]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[1]][,1], length(x1)), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' # posterior of class 1
#' pred.grid <- lapply(pred.grid, function(x) x/rowSums(x))
#' image(x1, x2, matrix(pred.grid[[1]][,1], length(x1)))
#' contour(x1, x2, matrix(pred.grid[[1]][,1], length(x1)), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## local membership
#' grid <- cbind(y = flashBayesClass(grid), grid)
#' loc.grid <- posterior(fit, newdata = grid)
#' contour(x1, x2, matrix(loc.grid[,1], length(x1)), add  = TRUE)

FLXMCLqda <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLqda", weighted = TRUE, formula = formula,
		name = "Mixture of QDA models")
	z@defineComponent <- expression({
		predict <- function(x) {
			## returns the joint density p(x,y), not posteriors !!!
    		ng <- length(attr(y, "lev"))
			lev1 <- names(fit$prior)
    		post <- matrix(0, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), attr(y, "lev")))
    		post[,lev1] <- sapply(lev1, function(z) fit$prior[z] * dmvnorm(x, fit$means[z,], fit$cov[[z]]))
	    	return(post)
		}
		logLik <- function(x, y) {
			## unnormalized log posterior, joint log likelihood
    		ng <- length(attr(y, "lev"))
			lev1 <- names(fit$prior)
    		post <- matrix(-10000, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), attr(y, "lev")))
    		post[,lev1] <- sapply(lev1, function(z) log(fit$prior[z]) + dmvnorm(x, fit$means[z,], fit$cov[[z]], log = TRUE))
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
		g
	}
	z@fit <- function(x, y, w) {
#print(attr(y, "lev"))
		fit <- wqda(x, factor(y, levels = attr(y, "lev")), weights = w, method = "ML", ...)
		K <- nrow(fit$means)
		d <- ncol(fit$means)
		fit$df <- K*d + K*d*(d-1)/2
		with(fit, eval(z@defineComponent))
	}
	z
}



#' @rdname FLXMCLqda
# @aliases FLXgetModelmatrix,FLXMCLqda-method
#'
#' @family mixtures qda
#'
#' @import flexmix
#' @export

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLqda"), 
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
