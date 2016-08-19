#' @rdname FLXMCL
# @aliases FLXMCLmajority-class
#'
#' @family mixtures majority
#'
#' @import flexmix
#' @export

setClass("FLXMCLmajority", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} from package \pkg{flexmix} implementing mixtures of majority classifiers.
#'
#' @title Mixtures of Majority Classifiers
#'
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param \dots Further arguments to and from other methods.
#'
#' @return Returns an object of class \code{FLXMCLmajority} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLmajority
# @aliases FLXMCLmajority
#'
#' @family mixtures majority
#'
#' @import flexmix
#' @export
#'
#' @examples
#' library(benchData)
#' data <- flashData(1000)
#' data$x <- scale(data$x)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 4)$cluster
#' model <- FLXMCLmajority()
#' fit <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
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
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,2], length(seq(-6,6,0.2))), add  = TRUE)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,3], length(seq(-6,6,0.2))), add  = TRUE)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,4], length(seq(-6,6,0.2))), add  = TRUE)

FLXMCLmajority <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLmajority", weighted = TRUE, formula = formula,
		name = "Mixture of Majority Classifiers")
	z@defineComponent <- expression({
		predict <- function(x) {
			pred <- getS3method("predict", "majority")(fit, newdata = x, ...)
			lev <- levels(pred$class)
			ng <- length(lev)
			if (ng > ncol(pred$posterior)) {
	        	posterior <- matrix(0, nrow(pred$posterior), ng)
	        	rownames(posterior) <- rownames(pred$posterior)
	        	colnames(posterior) <- lev
	        	posterior[,colnames(pred$posterior)] <- pred$posterior
	        	return(posterior)
			} else
        		return(pred$posterior)
		}
		logLik <- function(x, y) {
    		post <- getS3method("predict", "majority")(fit, newdata = x, ...)$posterior
    		ng <- length(attr(y, "lev"))
# print(head(post))
# print(head(y))
    		if (ng > ncol(post)) {
    			ll <- rep(0, nrow(post))
    			col.index <- match(y, colnames(post), 0)
    			row.index <- which(col.index > 0)
    			ll[row.index] <- post[cbind(row.index, col.index[row.index])]
    		} else {
	    		ll <- post[cbind(rownames(post), as.character(y))]
	    	}
	    	ll <- ifelse(ll == 0, -10000, log(ll))
# print(head(ll))
	    	return(ll)
		}
		new("FLXcomponent", parameters = list(prior = fit$prior), 
			logLik = logLik, predict = predict, df = fit$df)
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
		lev <- attr(y, "lev")
		fit <- majority(x, factor(y, levels = lev), weights = w)
		fit$df <- length(lev) - 1
		with(fit, eval(z@defineComponent))
	}
	z
}


#' @rdname FLXMCLmajority
# @aliases FLXgetModelmatrix,FLXMCLmajority-method
#'
#' @family mixtures majority
#'
#' @import flexmix
#' @export

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLmajority"), 
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
