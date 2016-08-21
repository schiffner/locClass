#' @title Weighted Quadratic Discriminant Analysis
#'
#' @description
#'  A version of Quadratic Discriminant Analysis that can deal with observation weights.
#'
#' @details
#'  The formulas for the weighted estimates of the class means, the covariance matrices and the class priors are as follows:
#'
#'  Normalized weights:
#'  \deqn{w_n^* = \frac{w_n}{\sum_{m:y_m=y_n} w_m}}{w_n* = w_n/(sum_{m:y_m=y_n} w_m)}
#'  Weighted class means:
#'  \deqn{\bar x_g = \sum_{n:y_n=g} w_n^* x_n}{bar x_g = sum_{n:y_n=g} w_n* x_i}
#'  Weighted class covariance matrices: \cr
#'  \code{method = "ML"}:
#'  \deqn{S_g = \sum_{n:y_n=g} w_n^* (x_n - \bar x_g)(x_n - \bar x_g)'}{S_g = sum_{n:y_n=g} w_n* (x_n - bar x_g)(x_n - bar x_g)'}
#'  \code{method = "unbiased"}:
#'  \deqn{S_g = \frac{\sum_{n:y_n=g} w_n^* (x_n - \bar x_g)(x_n - \bar x_g)'}{1 - \sum_{n:y_n=g} w_n^{*2}}}{S_g = sum_{n:y_n=g} w_n* (x_n - bar x_g)(x_n - bar x_g)'/(1 - sum_{n:y_n=g} (w_n*)^2)}
#'  Weighted prior probabilities:
#'  \deqn{p_g = \frac{\sum_{n:y_n=g} w_n}{\sum_n w_n}}{p_g = \sum_{n:y_n=g} w_n/(\sum_n w_n)}
#'
#'  If the predictor variables include factors, the formula interface must be used in order
#'  to get a correct model matrix.
#'
#' @param formula A \code{formula} of the form \code{groups ~ x1 + x2 + \dots}, that is, the response
#'  is the grouping \code{factor} and the right hand side specifies the
#'  discriminators.
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param grouping (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying
#'  the class membership for each observation.
#' @param weights Observation weights to be used in the fitting process, must be non-negative.
#' @param method Method for scaling the pooled weighted covariance matrix, either \code{"unbiased"} or maximum-likelihood (\code{"ML"}). Defaults to \code{"unbiased"}.
#' @param ... Further arguments.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if \code{NA}'s are found. The default action is first
#'  the \code{na.action} setting of \code{\link{options}} and second \code{\link{na.fail}} if that is unset.
#'  An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required
#'  variable. (NOTE: If given, this argument must be named.)
#'
#' @return An object of class \code{"wqda"}, a \code{list} containing the following components:
#'   \item{prior}{Weighted class prior probabilities.}
#'   \item{counts}{The number of observations per class.}
#'   \item{means}{Weighted estimates of class means.}
#'   \item{covs}{Weighted estimates of the class covariance matrices.}
#'   \item{lev}{The class labels (levels of \code{grouping}).}
#'   \item{N}{The number of observations.}
#'   \item{weights}{The observation weights used in the fitting process.}
#'   \item{method}{The method used for scaling the weighted covariance matrix estimates.}
#'   \item{call}{The (matched) function call.}
#'
#' @family qda
#'
#' @keywords classif multivariate
#'
#' @export
#'
#'
#' @examples
#' library(mlbench)
#' data(PimaIndiansDiabetes)
#'
#' train <- sample(nrow(PimaIndiansDiabetes), 500)
#'
#' # weighting observations from classes pos and neg according to their
#' # frequency in the data set:
#' ws <- as.numeric(1/table(PimaIndiansDiabetes$diabetes)
#'     [PimaIndiansDiabetes$diabetes])
#'
#' fit <- wqda(diabetes ~ ., data = PimaIndiansDiabetes, weights = ws,
#'     subset = train)
#' pred <- predict(fit, newdata = PimaIndiansDiabetes[-train,])
#' mean(pred$class != PimaIndiansDiabetes$diabetes[-train])

wqda <- function(x, ...)
	UseMethod("wqda")



#' @rdname wqda
#' @export

wqda.formula <- function(formula, data, weights = rep(1,nrow(data)), ..., subset, na.action) {
	m <- match.call(expand.dots = FALSE)
	m$... <- NULL
	m[[1L]] <- as.name("model.frame")
	m$weights <- weights
	m <- eval.parent(m)
	Terms <- attr(m, "terms")
	weights <- model.weights(m)
	grouping <- model.response(m)
	x <- model.matrix(Terms, m)
	xint <- match("(Intercept)", colnames(x), nomatch = 0L)
	if (xint > 0)
		x <- x[, -xint, drop = FALSE]
	res <- wqda.default(x, grouping, weights = weights, ...)
	res$terms <- Terms
	cl <- match.call()
	cl[[1L]] <- as.name("wqda")
	res$call <- cl
	res$contrasts <- attr(x, "contrasts")
	res$xlevels <- .getXlevels(Terms, m)
	res$na.action <- attr(m, "na.action")
	res
}



#' @rdname wqda
#' @export

wqda.data.frame <- function (x, ...) {
	res <- wqda(structure(data.matrix(x, rownames.force = TRUE), class = "matrix"), ...)
	cl <- match.call()
	cl[[1L]] <- as.name("wqda")
	res$call <- cl
	res
}



#' @rdname wqda
#' @export

wqda.matrix <- function (x, grouping, weights = rep(1, nrow(x)), ..., subset, na.action = na.fail) {
	if (!missing(subset)) {
		weights <- weights[subset]
		x <- x[subset, , drop = FALSE]
		grouping <- grouping[subset]
	}
	if (missing(na.action)) {
		if (!is.null(naa <- getOption("na.action")))    # if options(na.action = NULL) the default of na.action comes into play
			if(!is.function(naa))
				na.action <- get(naa, mode = "function")
			else
				na.action <- naa
	}
	dfr <- na.action(structure(list(g = grouping, w = weights, x = x),
			class = "data.frame", row.names = rownames(x)))
	grouping <- dfr$g
	weights <- dfr$w
	x <- dfr$x
	res <- wqda.default(x, grouping, weights, ...)
	cl <- match.call()
	cl[[1L]] <- as.name("wqda")
	res$call <- cl
	res$na.action <- na.action
	res
}



#' @rdname wqda
#' @export

wqda.default <- function(x, grouping, weights = rep(1, nrow(x)), method = c("unbiased", "ML"), ...) {
	if (is.null(dim(x)))
		stop("'x' is not a matrix")
	x <- as.matrix(x)
	if (any(!is.finite(x)))
		stop("infinite, NA or NaN values in 'x'")
	n <- nrow(x)
	if (n != length(weights))
		stop("nrow(x) and length(weights) are different")
	if (any(weights < 0))
		stop("weights have to be larger or equal to zero")
	if (all(weights == 0))
		stop("all 'weights' are zero")
	if (n != length(grouping))
		stop("nrow(x) and length(grouping) are different")
	names(weights) <- rownames(x)
	if (!is.factor(grouping))
		warning("'grouping' was coerced to a factor")
	g <- as.factor(grouping)
	lev <- lev1 <- levels(g)
	counts <- as.vector(table(g))
	if (any(counts == 0)) {
		empty <- lev[counts == 0]
		warning(sprintf(ngettext(length(empty), "group %s is empty",
			"groups %s are empty"), paste(empty, collapse = ", ")),
			domain = NA)
		lev1 <- lev[counts > 0]
		g <- factor(g, levels = lev1)
		counts <- as.vector(table(g))
	}
	if (length(lev1) < 2L)
		stop("training data from only one group given")
	names(counts) <- lev1
	# for fitting remove all observations with weight 0
	index <- weights > 0
	x <- x[index, , drop = FALSE]
	g <- g[index]
	co <- as.vector(table(g))
	lev1 <- lev1[co > 0]
	g <- factor(g, levels = lev1)
	w <- weights[index]
	method <- match.arg(method)
	class.weights <- tapply(w, g, sum)
	prior <- c(class.weights/sum(w))
	ng <- length(prior)
	xwt <- w * x
	center <- t(matrix(sapply(lev1, function(z) colSums(xwt[g == z, , drop = FALSE])), ncol = ng, dimnames = list(colnames(x), lev1)))/as.numeric(class.weights)
	z <- sqrt(w) * (x - center[g, , drop = FALSE])
	covs <- lapply(lev1, function(y) crossprod(z[g == y, , drop =FALSE])/class.weights[y])
	names(covs) <- lev1
	if (method == "unbiased") {
		norm.weights <- w/class.weights[g]
		covs <- lapply(lev1, function(y) covs[[y]]/(1 - sum(norm.weights[g == y]^2)))
		names(covs) <- lev1
	}
	cl <- match.call()
	cl[[1L]] <- as.name("wqda")
	return(structure(list(prior = prior, counts = counts, means = center,
		covs = covs, lev = lev, N = n, weights = weights, method = method, call = cl), class = "wqda"))
}



# @param x A \code{wqda} object.
# @param \dots Further arguments to \code{\link{print}}.
#
#' @noRd
#'
#' @export

print.wqda <- function(x, ...) {
	if (!is.null(cl <- x$call)) {
		names(cl)[2L] <- ""
		cat("Call:\n")
		dput(cl, control = NULL)
	}
	cat("\nWeighted prior probabilities of groups:\n")
	print(x$prior, ...)
	cat("\nWeighted group means:\n")
	print(x$means, ...)
	cat("\nWeighted class covariance matrices:\n")
	print(x$cov, ...)
	invisible(x)
}



#' @title Classify Observations Based on Weighted Quadratic Discriminant Analysis
#'
#' @description
#'  Classify observations in conjunction with \code{\link{wqda}}.
#'
#' @details
#'	This function is a method for the generic function \code{predict()} for class
#'	\code{"wqda"}.
#'	It can be invoked by calling \code{predict(x)} for an object \code{x} of the
#'	appropriate class, or directly by calling \code{predict.wqda(x)} regardless of
#'	the class of the object.
#'
#' @param object Object of class \code{"wqda"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if \code{object} has a
#'   \code{formula}, a \code{data.frame} with columns of the same names as the
#'   variables used. A vector will be interpreted as a row
#'   vector. If \code{newdata} is missing, an attempt will be made to
#'   retrieve the data used to fit the \code{wqda} object.
#' @param prior The class prior probabilities. By default the proportions in the training data set.
#' @param \dots Further arguments.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @family qda
#'
#' @keywords classif multivariate
#'
#' @rdname predict.wqda
#'
#' @export
#'
#'
#' @examples
#' library(mlbench)
#' data(PimaIndiansDiabetes)
#'
#' train <- sample(nrow(PimaIndiansDiabetes), 500)
#'
#' # weighting observations from classes pos and neg according to their
#' # frequency in the data set:
#' ws <- as.numeric(1/table(PimaIndiansDiabetes$diabetes)
#'     [PimaIndiansDiabetes$diabetes])
#'
#' fit <- wqda(diabetes ~ ., data = PimaIndiansDiabetes, weights = ws,
#'     subset = train)
#' pred <- predict(fit, newdata = PimaIndiansDiabetes[-train,])
#' mean(pred$class != PimaIndiansDiabetes$diabetes[-train])

predict.wqda <- function(object, newdata, prior = object$prior, ...) {
	if (!inherits(object, "wqda"))
		stop("object not of class \"wqda\"")
	if (!is.null(Terms <- object$terms)) {
		Terms <- delete.response(Terms)
		if (missing(newdata))
			newdata <- model.frame(object)
		else {
			newdata <- model.frame(Terms, newdata, na.action = na.pass,
				xlev = object$xlevels)
			if (!is.null(cl <- attr(Terms, "dataClasses")))
				.checkMFClasses(cl, newdata)
		}
		x <- model.matrix(Terms, newdata, contrasts = object$contrasts)
		xint <- match("(Intercept)", colnames(x), nomatch = 0L)
		if (xint > 0)
			x <- x[, -xint, drop = FALSE]
	}
	else {
		if (missing(newdata)) {
			if (!is.null(sub <- object$call$subset))
				newdata <- eval.parent(parse(text = paste(deparse(object$call$x,
					backtick = TRUE), "[", deparse(sub, backtick = TRUE),
					",]")))
			else newdata <- eval.parent(object$call$x)
			if (!is.null(nas <- object$call$na.action))
				newdata <- eval(call(nas, newdata))
		}
		if (is.null(dim(newdata)))
			dim(newdata) <- c(1, length(newdata))
		x <- as.matrix(newdata)
	}
	if (ncol(x) != ncol(object$means))
		stop("wrong number of variables")
	if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$means)[[2L]]))
		warning("variable names in 'newdata' do not match those in 'object'")
	ng <- length(object$prior)
	if (!missing(prior)) {
		if (any(prior < 0) || round(sum(prior), 5) != 1)
			stop("invalid prior")
		if (length(prior) != ng)
			stop("'prior' is of incorrect length")
	}
	lev1 <- names(object$prior)
	# posterior <- matrix(0, ncol = length(object$lev), nrow = nrow(x), dimnames = list(rownames(x), object$lev))
	# posterior[,lev1] <- sapply(lev1, function(z) log(prior[z]) - 0.5 * determinant(object$cov[[z]])$modulus
		# - 0.5 * mahalanobis(x, center = object$means[z,], cov = object$cov[[z]]))
	# post <- posterior[,lev1, drop = FALSE]
	# gr <- factor(lev1[max.col(post)], levels = object$lev)
	# names(gr) <- rownames(x)
	# post <- exp(post - apply(post, 1L, max, na.rm = TRUE))
	# post <- post/rowSums(post)
	# posterior[,lev1] <- post
	posterior <- matrix(0, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), lev1))
	posterior[,lev1] <- sapply(lev1, function(z) log(prior[z]) - 0.5 * determinant(object$cov[[z]])$modulus
		- 0.5 * mahalanobis(x, center = object$means[z,], cov = object$cov[[z]]))
	gr <- factor(lev1[max.col(posterior)], levels = object$lev)
	names(gr) <- rownames(x)
	posterior <- exp(posterior - apply(posterior, 1L, max, na.rm = TRUE))
	posterior <- posterior/rowSums(posterior)
	if (any(is.infinite(posterior)))
		warning("infinite, NA or NaN values in 'posterior'")
	return(list(class = gr, posterior = posterior))
}



#' @noRd
#'
#' @importFrom stats weights
#' @export

weights.wqda <- function (object, ...) {
	if (is.null(object$weights))
		rep(1, object$N)
	else object$weights
}



#' @noRd
#'
#' @importFrom stats model.frame
#' @export

model.frame.wqda <- function (formula, ...) {
	oc <- formula$call
	oc$weights <- oc$method <- oc$wf <- oc$bw <- oc$k <- oc$nn.only <- oc$itr <- NULL
	oc[[1L]] <- quote(stats::model.frame)
	if (length(dots <- list(...))) {
		nargs <- dots[match(c("data", "na.action", "subset"),
			names(dots), 0)]
		oc[names(nargs)] <- nargs
	}
	if (is.null(env <- environment(formula$terms)))
		env <- parent.frame()
	eval(oc, env)
}
