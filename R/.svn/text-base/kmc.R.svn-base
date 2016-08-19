#  Copyright (C) 2011 J. Schiffner
#  Copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
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
#' Classification based on K-means clustering within the classes.
#'
#' Prototype methods represent the training data by a set of points in feature
#' space. Each prototype has an associated class label, and classification of a query
#' point \eqn{x} is made to the class of the closest prototype.
#'
#' In order to use K-means clustering for classification of labeled data K-means 
#' clustering is applied to the training data of each class separately, using the number
#' of prototypes per class which is specified by \code{K} and defaults to 2.
#'
#' Usually for a test observation the class label of the closest prototype is predicted.
#' But it is also possible to use more than 1 prototype and to weigh the influence of the prototypes on 
#' the classification according to their distances from the observation to be classified.
#' This is controlled by the arguments \code{wf}, \code{k}, \code{bw} and \code{nn.only}
#' (see \code{\link[=biweight]{wfs}}).
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{kmc}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{kmc}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' It may be useful to \code{\link{scale}} the data first.
#'
#' If the predictor variables include factors, the formula interface must be used in order 
#' to get a correct model matrix.
#'
#' @title K-Means Classification
#'
#' @param formula A \code{formula} of the form \code{groups ~ x1 + x2 + \dots}, that is, the response is the 
#' grouping \code{factor} and the right hand side specifies the (usually non-\code{factor}) discriminators.  
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param grouping (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying the class membership for each observation.
#' @param K The number of prototypes per class, either a single number or a vector of length equal to the number of classes. 
#' The numbers of centers have to be in the same order as the levels of grouping. Default is \code{K = 2}.
#' @param wf A window function which is used to calculate weights that are introduced into 
#' the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#' For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param nstart The number of random starts of the K-means algorithm. See \code{\link{kmeans}}.
#' @param \dots Further arguments to be passed to \code{\link{kmeans}}.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if NAs are found. The default action is first
#' the \code{na.action} setting of \code{\link{options}} and second \code{\link{na.fail}} if that is unset. 
#' An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#' variable. (NOTE: If given, this argument must be named.)
#'
#' @return An object of class \code{"kmc"} containing the following components:
#' \item{counts}{The number of observations per class.}
#' \item{x}{A \code{matrix} of prototypes.}
#' \item{grouping}{A \code{factor} specifying the class membership for each prototype.}
#' \item{lev}{The class labels (the levels of \code{grouping}).}
#' \item{N}{The number of training observations.}
#' \item{K}{The (used) number of prototypes per class.}
#' \item{call}{The (matched) function call.}
#'
#' @references
#' T. Hastie, R. Tibshirani, and J. Friedman. The Elements of Statistical Learning: Data Mining,
#' Inference, and Prediction. Springer Series in Statistics. Springer, New York, 2001.
#'
#' @seealso \code{\link{predict.kmc}}, \code{\link{kmeans}}.
#'
#' @examples
#' # generate waveform data
#' library(mlbench)
#' data.train <- as.data.frame(mlbench.waveform(300))
#' 
#' # 3 centers per class
#' object <- kmc(classes ~ ., data = data.train, K = 3, wf = "rectangular", k = 1)
#' object <- kmc(data.train[,-22], data.train$classes, K = 3, wf = "rectangular", k = 1)
#' 
#' # 2 centers in class 1, 3 centers in class 2, 4 centers in class 3 
#' object <- kmc(classes ~ ., data = data.train, K = c(2,3,4), wf = "rectangular", k = 1)
#' object <- kmc(data.train[,-22], data.train$classes, K = c(2,3,4), wf = "rectangular", k = 1)
#'
#' @keywords classif multivariate
#'
#' @aliases kmc kmc.data.frame kmc.default kmc.formula kmc.matrix update.kmc
#'
#' @export

# \code{k} is the number of prototypes used for classification, \code{wf} the weight function. 
# Per default \code{wf = "none"}, that is
# no weighting is done, and \code{k = 1}. Thus the class label of the closest prototype is assigned to each new observation to be classified.
# Other choices of \code{wf} require specification of the \code{bw} argument, the width of the window function. In these cases the argument \code{k}  
# is optional. Per default all prototypes are used.
# This is similar to gaussian mixture-based discriminant analysis (cp. \code{\link[mda]{mda}}).

kmc <- function(x, ...)
    UseMethod("kmc")
    

    
#' @rdname kmc
#' @method kmc formula
#'
#' @S3method kmc formula

kmc.formula <- function(formula, data, ..., subset, na.action) {
    m <- match.call(expand.dots = FALSE)
    m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    grouping <- model.response(m)
    x <- model.matrix(Terms, m)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (xint > 0) 
        x <- x[, -xint, drop = FALSE]
    res <- kmc.default(x, grouping, ...)
    res$terms <- Terms
    cl <- match.call()
    cl[[1L]] <- as.name("kmc")
    res$call <- cl
    res$contrasts <- attr(x, "contrasts")
    res$xlevels <- .getXlevels(Terms, m)
    res$na.action <- attr(m, "na.action")
    res
}



#' @rdname kmc
#' @method kmc data.frame
#'
#' @S3method kmc data.frame

kmc.data.frame <- function (x, ...) {
    res <- kmc(structure(data.matrix(x, rownames.force = TRUE), class = "matrix"), ...)
    cl <- match.call()
    cl[[1L]] <- as.name("kmc")
    res$call <- cl
    res
}



#' @rdname kmc
#' @method kmc matrix
#'
#' @S3method kmc matrix

kmc.matrix <- function (x, grouping, ..., subset, na.action = na.fail) {
    if (!missing(subset)) {
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
    dfr <- na.action(structure(list(g = grouping, x = x), 
            class = "data.frame", row.names = rownames(x)))
    grouping <- dfr$g
    x <- dfr$x
    res <- kmc.default(x, grouping, ...)
    cl <- match.call()
    cl[[1L]] <- as.name("kmc")
    res$call <- cl
	res$na.action <- na.action
    res
}



#' @rdname kmc
#' @method kmc default
#'
#' @S3method kmc default

kmc.default <- function(x, grouping, K = 2, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	"exponential", "gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only = TRUE, nstart = 1, ...) {
    if (is.null(dim(x)))
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    n <- nrow(x)
    if (any(!is.finite(x)))
        stop("infinite, NA or NaN values in 'x'")
    #if (missing(grouping))
    #    stop('argument "grouping" is missing, with no default')
    if (!is.factor(grouping))
        warning("'grouping' was coerced to a factor")
	g <- as.factor(grouping)
    if (n != length(g))
        stop("'nrow(x)' and 'length(grouping)' are different")
    lev <- lev1 <- levels(g)
    if ((!is.numeric(K)) || !length(K))
        stop("'K' must be numeric of length > 0")
    if (length(K) != 1 && length(K) != length(lev))
        stop("'length(K)' has to be 1 or equal to the number of classes")
    if(any(abs(K - round(K)) > .Machine$double.eps^0.5)) {
        K <- round(K)
        warning("'K' must contain whole numbers and was rounded")
    }
    if (any(K < 0)) 
    	stop("'K' must be positive")
    counts <- as.vector(table(g))
    if (any(counts == 0)) {
        empty <- counts == 0
        warning(sprintf(ngettext(length(lev[empty]), "class %s is empty", 
            "classes %s are empty"), paste(lev[empty], collapse = ", ")), 
            domain = NA)
        lev1 <- lev[counts > 0]
        g <- factor(g, levels = lev1)
        counts <- counts[counts > 0]
        if (length(K) != 1) {
            K <- K[counts > 0]
        }    
    }
    if (any(K > counts)) {
    	large <- K > counts
    	stop(sprintf(ngettext(length(lev1[large]), "too many prototypes for class %s", 
            "too many prototypes for classes %s"), paste(lev1[large], collapse = ", ")), 
            domain = NA)
	}
	nproto <- ifelse(length(K) == 1, length(lev1) * K, sum(K))
	if (!missing(nstart)) {
    	if (!is.numeric(nstart) || !length(nstart))
        	stop("'nstart' must be numeric of length > 0")
    	if (nstart <= 0)
        	stop("'nstart' must be larger than zero")
    	if (abs(nstart - round(nstart)) > .Machine$double.eps^0.5) {
        	nstart <- round(nstart)
        	warning("'nstart' must be a whole number and was rounded")
    	}
    }
    if (is.character(wf)) {
    	m <- match.call(expand.dots = FALSE)
    	m$n <- nproto
    	m[[1L]] <- as.name("generatewf")
    	wf <- eval.parent(m)
	} else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    	if(!is.null(attr(wf, "adaptive"))) {
    		if(attr(wf, "adaptive")) {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") + 1 > n)
    				stop("'k + 1' is larger than 'nrow(x)'")
    		} else {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") > n)
    				stop("'k' is larger than 'nrow(x)'")
    		}
    	}
    } else
    	stop("argument 'wf' is neither a character nor a function")
    if (length(K) == 1) {
        proto <- lapply(lev1, function(z) kmeans(x[g == z, , drop = FALSE], centers = K, nstart = nstart)$centers)
        #by(x, g, function(z) kmeans(z, centers = K, nstart = nstart, ...)$centers) slower
        cl <- factor(rep(lev1, each = K))
    } else {
        proto <- lapply(seq(along = lev1), function(z) kmeans(x[g == lev1[z], , drop = FALSE], centers = K[z], nstart = nstart, ...)$centers)
		#lapply(lev1, function(z) kmeans(x[g == z,], centers = K[z], nstart = nstart, ...)$centers)
        cl <- factor(rep(lev1, times = K))    
        names(K) <- lev1
    }
    proto <- do.call(rbind, proto)  
    colnames(proto) <- colnames(x)
    call <- match.call()
    call[[1]] <- as.name("kmc")
    res <- structure(list(x = proto, grouping = cl, counts = counts, lev = lev, N = n, K = K, wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), 
    	nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive"), call = call), class = "kmc")
    return(res)
}



# @param x A \code{kmc} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @method print kmc
#' @noRd
#'
#' @S3method print kmc

print.kmc <- function(x, ...) {
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        cat("Call:\n")
        dput(cl, control = NULL)
    }
    cat("\nNumber of prototypes per class:\n")
    if (length(x$K) == 1) {
        cat(x$K, "\n")
    } else {    
        print(x$K, ...)
    }
    cat("\nPrototypes:\n")
    print(data.frame(grouping = x$grouping, x$x, row.names = NULL), ...) # evtl. rownames weglassen
	if(!is.null(attr(x$wf, "name"))) {
        cat("\nWindow function: ")
        cat(deparse(attr(x$wf, "name")), sep="\n")  
    } else {
        cat("\nWindow function:\n")
        cat(deparse(x$wf), sep="\n")  
    }
    if(!is.null(x$bw))
        cat("Bandwidth: ", x$bw, "\n")
    if(!is.null(x$k))
        cat("k: ", x$k, "\n")
    if(!is.null(x$nn.only))
        cat("Nearest neighbors only: ", x$nn.only, "\n")
    if(!is.null(x$adaptive))
        cat("Adaptive bandwidth: ", x$adaptive, "\n")
    invisible(x)
}



#' @rdname kmc
#' @method update kmc
#'
#' @S3method update kmc
#' @importFrom stats update
#'
#' @param object An object of class \code{"kmc"}.
# @param wf A window function which is used to calculate weights that are introduced into 
# the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
# For details see the documentation for \code{\link[=biweight]{wfs}}.
# @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
# @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. (See \code{\link[=biweight]{wfs}}.)
# @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
# only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)

update.kmc <- function(object, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	"exponential", "gaussian", "optcosine", "rectangular", "triangular"), 
	bw, k, nn.only, ...) {
	if (!inherits(object, "kmc"))
		stop("object not of class \"kmc\"")
    if (is.character(wf)) {
		#if (!missing(k))
    	#	if (k > nrow(object$x))
	   	#		stop("'k' is larger than total number of prototypes")
    	m <- match.call(expand.dots = FALSE)
    	m$object <- NULL
    	m$n <- nrow(object$x)
    	m[[1L]] <- as.name("generatewf")
    	wf <- eval.parent(m)
	} else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    } else
    	stop("argument 'wf' is neither a character nor a function")
  	object$wf <- wf
	object["bw"] <- list(bw = attr(wf, "bw"))
	object["k"] <- list(bw = attr(wf, "k"))
	object["nn.only"] <- list(bw = attr(wf, "nn.only"))
	object["adaptive"] <- list(bw = attr(wf, "adaptive"))
	object$call$wf <- m$wf
	object$call$bw <- m$bw
	object$call$k <- m$k
	object$call$nn.only <- m$nn.only
	return(object)
}



#' Classify multivariate observations using the prototype representation of the data 
#' obtained with the \code{\link{kmc}} function. 
#'
#' Classification of a new observation is based on the prototypes found in the training data by function \code{\link{kmc}}. 
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"kmc"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.kmc(x)} regardless of 
#' the class of the object. 
#'
#' @title K-means Classification
#'
#' @param object An object of class \code{"kmc"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if object has a \code{formula}, a \code{data.frame}
#' with columns of the same names as the variables used. A vector will be interpreted
#' as a row vector. If \code{newdata} is missing, an attempt will be made to
#' retrieve the data used to fit the \code{kmc} object.
#' @param \dots Further arguments. Currently unused.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @references
#' T. Hastie, R. Tibshirani, and J. Friedman. The Elements of Statistical Learning: Data Mining,
#' Inference, and Prediction. Springer Series in Statistics. Springer, New York, 2001.
#'
#' @seealso \code{\link{kmc}}.
#'
#' @examples
#' # generate waveform data
#' library(mlbench)
#' data.train <- as.data.frame(mlbench.waveform(300))
#' data.test <- as.data.frame(mlbench.waveform(200))
#' 
#' # prediction based on the nearest prototype
#' object <- kmc(classes ~ ., data = data.train, K = 3, wf = "rectangular", k = 1)
#' pred <- predict(object, data.test[,-22])
#' mean(pred$class != data.test$classes)
#' 
#' # prediction based on the 3 nearest prototypes with weighting
#' object <- kmc(classes ~ ., data = data.train, K = 3, wf = "gaussian", bw = 1, k = 3)
#' pred <- predict(object, data.test[,-22])
#' mean(pred$class != data.test$classes)
#'
#' @keywords classif multivariate
#'
#' @method predict kmc
#' @rdname predict.kmc
#'
#' @S3method predict kmc

# todo:
#system.time(replicate(1000, apply(object$x, 1, function(y) sum((y - newx)^2))))
#system.time(replicate(1000, colSums((t(object$x) - newx)^2)))
#system.time(replicate(1000, tapply(dense, object$grouping, sum)))
#system.time(replicate(1000, by(dense, object$grouping, sum)))

predict.kmc <- function(object, newdata = NULL, ...) {
    weighted <- function(newx, object) {
        distance <- sqrt(colSums((t(object$x) - newx)^2))
		weights <- object$wf(distance)
        res <- tapply(weights, object$grouping, sum)
        return(res)
	}
    if (!inherits(object, "kmc")) 
        stop("object not of class \"kmc\"")
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
    if (ncol(x) != ncol(object$x)) 
        stop("wrong number of variables")
    if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$x)[[2L]])) 
        warning("variable names in 'newdata' do not match those in 'object'")
    lev1 <- levels(object$grouping)
	posterior <- matrix(0, nrow = nrow(x), ncol = length(lev1), dimnames = list(rownames(x), lev1))
	posterior[,lev1] <- t(apply(x, 1, weighted, object = object))
    gr <- factor(lev1[max.col(posterior)], levels = object$lev)
    names(gr) <- rownames(x)
    posterior <- posterior/rowSums(posterior)
    res <- list(class = gr, posterior = posterior)
    return(res)
}



#' @method model.frame kmc
#' @noRd
#'
#' @S3method model.frame kmc
#' @importFrom stats model.frame

model.frame.kmc <- function (formula, ...) {
    oc <- formula$call
    oc $K <- oc$wf <- oc$bw <- oc$k <- oc$nn.only <- oc$nstart <- NULL
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
