#  Copyright (C) 2011 J. Schiffner
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

#' A classifier that always predicts the class with the highest weighted prior probability.
#'
#' This function is rather a helper function needed to combine mixture models and recursive partitioning with a constant classifier.
#' The weighted prior probabilities are calculated as
#' \deqn{p_g = \frac{\sum_{n:y_n=g} w_n}{\sum_n w_n}}{p_g = \sum_{n:y_n=g} w_n/\sum_n w_n}
#'
#' @title Constant Classifier
#'
#' @param formula A \code{formula} of the form \code{groups ~ x1 + x2 + \dots}, that is, the response
#' is the grouping \code{factor} and the right hand side specifies the (non-\code{factor})
#' discriminators.  
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param grouping (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying
#' the class membership for each observation.
#' @param weights Observation weights to be used in the fitting process, must be larger or equal to zero.
#' @param ... Further arguments.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if NAs are found. The default action is first
#' the \code{na.action} setting of \code{\link{options}} and second \code{\link{na.fail}} if that is unset. 
#' An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#' variable. (NOTE: If given, this argument must be named.)
#'
#' @return An object of class \code{"constant"}, a \code{list} containing the following components:
#'   \item{prior}{Weighted class prior probabilities.}
#'   \item{counts}{The number of observations per class.}
#'   \item{lev}{The class labels (levels of \code{grouping}).}  
#'   \item{N}{The number of observations.}
#'   \item{weights}{The observation weights used in the fitting process.}
#'   \item{predictors}{The names of the predictor variables.}
#'   \item{call}{The (matched) function call.}
#'
#' @seealso \code{\link{predict.constant}}.
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
#' fit <- constant(diabetes ~ ., data = PimaIndiansDiabetes, weights = ws, 
#'     subset = train)
#' pred <- predict(fit, newdata = PimaIndiansDiabetes[-train,])
#' mean(pred$class != PimaIndiansDiabetes$diabetes[-train])
#'
#' @keywords classif
#'
#' @aliases constant constant.data.frame constant.default constant.formula constant.matrix
#'
#' @export

constant <- function(x, ...)
	UseMethod("constant")
	
	
	
#' @rdname constant
#' @method constant formula
#'
#' @S3method constant formula

constant.formula <- function(formula, data, weights = rep(1, nrow(data)), ..., subset, na.action) {
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
    res <- constant.default(x, grouping, weights = weights, ...)
    res$terms <- Terms
    cl <- match.call()
    cl[[1L]] <- as.name("constant")
    res$call <- cl
    res$contrasts <- attr(x, "contrasts")
    res$xlevels <- .getXlevels(Terms, m)
    res$na.action <- attr(m, "na.action")
    res
}



#' @rdname constant
#' @method constant data.frame
#'
#' @S3method constant data.frame

constant.data.frame <- function (x, ...) {
    res <- constant(structure(data.matrix(x, rownames.force = TRUE), class = "matrix"), ...)
    cl <- match.call()
    cl[[1L]] <- as.name("constant")
    res$call <- cl
    res
}



#' @rdname constant
#' @method constant matrix
#'
#' @S3method constant matrix

constant.matrix <- function (x, grouping, weights = rep(1, nrow(x)), ..., subset, na.action = na.fail) {
    if (!missing(subset)) {
        weights <- weights[subset]
        x <- x[subset, , drop = FALSE]
        grouping <- grouping[subset]
    }
    if (missing(na.action)) {
        if (!is.null(naa <- getOption("na.action"))) {    # if options(na.action = NULL) the default na.fail comes into play
            if(!is.function(naa))
            	na.action <- get(naa, mode = "function")
            else
                na.action <- naa
		}
    } 
    dfr <- na.action(structure(list(g = grouping, w = weights, x = x), 
            class = "data.frame", row.names = rownames(x)))
    grouping <- dfr$g
    x <- dfr$x
    weights <- dfr$w
    res <- constant.default(x, grouping, weights, ...)
    cl <- match.call()
    cl[[1L]] <- as.name("constant")
    res$call <- cl
	res$na.action <- na.action
	res
}



#' @rdname constant
#' @method constant default
#'
#' @S3method constant default

constant.default <- function(x, grouping, weights = rep(1, nrow(x)), ...) {
    if (is.null(dim(x))) 
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    if (any(!is.finite(x))) 
        stop("infinite, NA or NaN values in 'x'")
    n <- nrow(x)
    if (n != length(weights))
        stop("nrow(x) and length(weights) are different")
    predictors <- colnames(x)
    if (any(weights < 0))
        stop("weights have to be larger or equal to zero")
    if (all(weights == 0))
        stop("all weights are zero")
    names(weights) <- rownames(x)
    if (n != length(grouping)) 
        stop("'nrow(x)' and 'length(grouping)' are different")
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
    g <- g[index]
    co <- as.vector(table(g))
    g <- factor(g, levels = lev1[co > 0])
    w <- weights[index]
	class.weights <- tapply(w, g, sum)
    prior <- c(class.weights/sum(w))
    cl <- match.call()
    cl[[1L]] <- as.name("constant")
    return(structure(list(prior = prior, counts = counts, lev = lev, N = n, weights = weights, predictors = predictors, call = cl), class = "constant"))
}



# @param x A \code{constant} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @method print constant
#' @noRd
#'
#' @S3method print constant

print.constant <- function(x, ...) {
    if (!is.null(cl <- x$call)) {
        names(cl)[2L] <- ""
        cat("Call:\n")
        dput(cl, control = NULL)
    }
    cat("\nWeighted prior probabilities of groups:\n")
    print(x$prior, ...)
    invisible(x)
}



#' Classify multivariate observations in conjunction with \code{\link{constant}}.
#'
#'	This function is a method for the generic function \code{predict()} for class 
#'	\code{"constant"}. 
#'	It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#'	appropriate class, or directly by calling \code{predict.constant(x)} regardless of 
#'	the class of the object.
#'
#' @title Classify Multivariate Observations Based on the Constant Classifier
#'
#' @param object Object of class \code{"constant"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if \code{object} has a
#'          \code{formula}, a \code{data.frame} with columns of the same names as the
#'          variables used.  A vector will be interpreted as a row
#'          vector.  If \code{newdata} is missing, an attempt will be made to
#'          retrieve the data used to fit the \code{constant} object.
#' @param \dots Further arguments.
#'
#' @return A list with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @seealso \code{\link{constant}}.
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
#' fit <- constant(diabetes ~ ., data = PimaIndiansDiabetes, weights = ws, 
#'     subset = train)
#' pred <- predict(fit, newdata = PimaIndiansDiabetes[-train,])
#' mean(pred$class != PimaIndiansDiabetes$diabetes[-train])
#'
#' @keywords classif
#'
#' @method predict constant
#' @rdname predict.constant
#'
#' @S3method predict constant

predict.constant <- function(object, newdata, ...) {
	if (!inherits(object, "constant"))
        stop("object not of class \"constant\"")
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

    if (ncol(x) != length(object$predictors))
        stop("wrong number of variables")
    if (length(colnames(x)) > 0L && any(colnames(x) != object$predictors))
        warning("variable names in 'newdata' do not match those in 'object'")
    ng <- length(object$prior)
    lev1 <- names(object$prior)
	posterior <- matrix(object$prior, nrow = nrow(x), ncol = ng, byrow = TRUE)
	dimnames(posterior) <- list(rownames(x), lev1)
    gr <- factor(lev1[max.col(posterior)], levels = object$lev)
    names(gr) <- rownames(x)
    return(list(class = gr, posterior = posterior))
}



#' @method weights constant
#' @noRd
#'
#' @S3method weights constant
#' @importFrom stats weights

weights.constant <- function (object, ...) {
    if (is.null(object$weights)) 
        rep(1, object$N)
    else object$weights
}



#' @method model.frame constant
#' @noRd
#'
#' @S3method model.frame constant
#' @importFrom stats model.frame

model.frame.constant <- function (formula, ...) {
    oc <- formula$call
    #m <- match(oc, c("formula", "data"))
    oc$weights <- NULL
    oc[[1L]] <- as.name("model.frame")
    if (length(dots <- list(...))) {
        nargs <- dots[match(c("data", "na.action", "subset"), 
            names(dots), 0)]
        oc[names(nargs)] <- nargs
    }
    if (is.null(env <- environment(formula$terms))) 
        env <- parent.frame()
    eval(oc, env)
}
