#  Copyright (C) 2011 J. Schiffner
#  Copyright (C) Evgenia Dimitriadou, Kurt Hornik, Friedrich Leisch, David Meyer, and Andreas Weingessel  
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or fiTNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

#' A localized version of Support Vector Machines.
#'
#' This is a localized version of the Support Vector Machine where for each test observation
#' an individual SVM is fitted. Training observations are weighted according to their Euclidean
#' distance from the test observation.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{predict.ossvm}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{ossvm}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' For multiclass-classification with \eqn{k} levels, \eqn{k>2}, \sQuote{libsvm} uses the 'one-against-one'-approach, 
#' in which \eqn{k(k-1)/2} binary classifiers are trained; the appropriate class is found by a voting 
#' scheme.
#'
#' \sQuote{libsvm} internally uses a sparse data representation, which is also high-level supported by 
#' the package \pkg{SparseM}.
#'
#' If the predictor variables include factors, the formula interface must be used to get a 
#' correct model matrix.
#'
#' The probability model for classification fits a logistic distribution using maximum likelihood 
#' to the decision values of all binary classifiers, and computes the a-posteriori class 
#' probabilities for the multi-class problem using quadratic optimization. The probabilistic 
#' regression model assumes (zero-mean) laplace-distributed errors for the predictions, and 
#' estimates the scale parameter using maximum likelihood.
#'
#' @title Observation Specific Support Vector Machines
#' 
#' @param formula A symbolic description of the model to be fit.
#' @param data An optional data frame containing the variables in the model. By default
#'   the variables are taken from the environment which \code{ossvm} is called from.
#' @param x (Required if no \code{formula} is given as principal argument.) A data matrix, a 
#'   vector, or a sparse matrix (object of class \code{\link[Matrix]{Matrix}} provided by the 
#'   \pkg{Matrix} package, or of class \code{\link[SparseM]{matrix.csr}} provided by the \pkg{SparseM} 
#'   package, or of class \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam} package).
#' @param y (Only if no \code{formula} is given as principal argument.) A response vector with 
#'   one label for each row/component of \code{x}. Can be either a factor (for classification tasks) or 
#'   a numeric vector (for regression).
#' @param scale A logical vector indicating the variables to be scaled. If \code{scale} is of 
#'   length 1, the value is recycled as many times as needed. Per default, 
#'   data are scaled internally (both \code{x} and \code{y} variables) to zero mean and unit variance. 
#'   The center and scale values are returned and used for later predictions.
#' @param type \code{ossvm} can be used as a classification machine, as a regression machine, or 
#'   for novelty detection. Depending of whether \code{y} is a factor or not, 
#'   the default setting for \code{type} is \code{C-classification} or \code{eps-regression}, 
#'   respectively, but may be overwritten by setting an explicit value.
#'   \cr
#'   Valid options are:
#'   \itemize{
#'   \item \code{C-classification}
#'   \item \code{nu-classification}
#'   \item \code{one-classification} (for novelty detection)
#'   \item \code{eps-regression}
#'   \item \code{nu-regression}
#'   }
#' @param kernel The kernel used in training and predicting. You might consider changing some of 
#'    the following parameters, depending on the kernel type.
#'    \describe{
#'      \item{linear:}{\eqn{u'v}{u'*v}}
#'      \item{polynomial:}{\eqn{(\gamma u'v + coef0)^{degree}}{(gamma*u'*v + coef0)^degree}}
#'      \item{radial basis:}{\eqn{\exp(-\gamma |u-v|^2)}{exp(-gamma*|u-v|^2)}}
#'      \item{sigmoid:}{\eqn{\tanh(\gamma u'v + coef0)}{tanh(gamma*u'*v + coef0)}}
#'      }
#' @param degree Parameter needed for kernel of type \code{polynomial} (default: 3).
#' @param gamma Parameter needed for all kernels except \code{linear} (default: 1/(data dimension)).
#' @param coef0 Parameter needed for kernels of type \code{polynomial} and \code{sigmoid} (default: 0).
#' @param cost Cost of constraints violation (default: 1)--it is the \sQuote{C}-constant of the 
#'   regularization term in the Lagrange formulation.
#' @param nu Parameter needed for \code{nu-classification}, \code{nu-regression}, and \code{one-classification}.
#' @param class.weights A named vector of weights for the different classes, used for asymmetric class sizes. 
#'    Not all factor levels have to be supplied (default weight: 1). 
#'    All components have to be named.
#' @param cachesize Cache memory in MB (default 40).
#' @param tolerance Tolerance of termination criterion (default: 0.001).
#' @param epsilon epsilon in the insensitive-loss function (default: 0.1).
#' @param shrinking Option whether to use the shrinking-heuristics (default: \code{TRUE}).
# @param cross If a integer value k>0 is specified, a k-fold cross validation on the training data is 
#   performed to assess the quality of the model: the accuracy 
#   rate for classification and the Mean Squared Error for regression.
# @param fitted Logical indicating whether the fitted values should be computed and included 
#   in the model or not (default: \code{TRUE}).
# @param probability Logical indicating whether the model should allow for probability predictions.
# @param seed Integer seed for libsvm (used for cross-validation and probability prediction models).
#' @param seed Integer seed for libsvm (used for probability prediction models).
#' @param \dots Additional parameters for the low level fitting function \code{ossvm.default}.
#' @param subset An index vector specifying the cases to be used in the training sample. 
#'  (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if \code{NA}s are found. 
#'   The default action is \code{na.omit}, which 
#'   leads to rejection of cases with missing values on any required variable. An alternative 
#'   is \code{na.fail}, which causes an 
#'   error if \code{NA} cases are found. (NOTE: If given, this argument must be named.)
#' @param wf A window function which is used to calculate weights that are introduced into 
#'   the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#'   For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the 
#'   window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision 
#'   boundary to be used in the fitting process. 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with 
#'  infinite support and if \code{k} is specified.) Should
#'  only the \code{k} nearest neighbors or all observations receive positive weights? 
#'  (See \code{\link[=biweight]{wfs}}.)
#'
#' @return An object of class \code{"ossvm"}, a \code{list} containing all information about 
#'   the SVM model to be fitted, mainly for internal use. It includes:
#'   \item{call}{The (matched) function call.}
#'   \item{x}{The explanatory variables (already scaled if scaling was desired).}
#'   \item{y}{The response vector (appropriately modified).}
#'   \item{wf}{The window function used.}
#'   \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions 
#'      documented in \code{\link[=biweight]{wfs}}.) 
#'	  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#' 	 \item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions 
#'  	documented in \code{\link[=biweight]{wfs}}.) 
#'	  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#'   \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of 
#'  	the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	 specified.) \code{TRUE} if only the \code{k} nearest neighbors receive a positive 
#'   weight, \code{FALSE} otherwise.}
#'   \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the 
#'  	local density of data points, \code{FALSE} if the bandwidth
#'	  is fixed.}
#'   \item{variant}{(Only if \code{wf} is a string or one of the window functions documented in 
#'  	\code{\link[=biweight]{wfs}} is used, for internal use only). 
#'	  An integer indicating which weighting scheme is implied by \code{bw}, \code{k} and \code{nn.only}.}
#'	Moreover, information about the chosen SVM \code{type}, the \code{kernel} function and the SVM parameters is 
#'  included.
#   \item{type}{}
#   \item{kernel}{}
#   \item{cost}{}
#   \item{degree}{}
#   \item{gamma}{}
#   \item{coef0}{}
#   \item{cachesize}{}
#   \item{tolerance}{}
#   \item{weightlabels}{}
#   \item{class.weights}{}
#   \item{shrinking}{}
#   \item{seed}{}
#   \item{nu}{}
#   \item{epsilon}{}
#   \item{sparse}{}
#   \item{scaled}{}
#   \item{x.scale}{}
#   \item{y.scale}{}
#   \item{nclasses}{}
#   \item{labels}{}
#   \item{levels}{}
#   \item{na.action}{}
#'
#' @seealso \code{\link{predict.ossvm}}.
#'
#' @examples
#' fit <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 0.5, 
#'     kernel = "polynomial", coef0 = 0.5)
#' pred <- predict(fit)
#' mean(pred != iris$Species)
#'
#' @keywords classif regression
#'
#' @aliases ossvm ossvm.default ossvm.formula
#'
#' @export

ossvm <- function(x, ...)
	UseMethod("ossvm")
	
	
	
#' @rdname ossvm
#' @method ossvm formula
#'
#' @S3method ossvm formula

ossvm.formula <- function (formula, data = NULL, ..., subset, na.action = na.omit, scale = TRUE) {
    call <- match.call()
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    if (identical(class(eval.parent(m$data)), "matrix"))
        m$data <- as.data.frame(eval.parent(m$data))
    m$... <- NULL
    m$scale <- NULL
    m[[1]] <- as.name("model.frame")
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
    attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")
    if (length(scale) == 1)
        scale <- rep(scale, ncol(x))
    if (any(scale)) {
        remove <- unique(c(which(labels(Terms) %in%
                                 names(attr(x, "contrasts"))),
                           which(!scale)
                           )
                         )
        scale <- !attr(x, "assign") %in% remove
    }
    ret <- ossvm.default (x, y, scale = scale, ..., na.action = na.action)
    ret$call <- call
    ret$call[[1]] <- as.name("ossvm")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("ossvm.formula", class(ret))
    return (ret)
}



#' @rdname ossvm
#' @method ossvm default
#'
#' @S3method ossvm default

ossvm.default <- function (x, 
							y			= NULL,
							scale		= TRUE,
							type		= NULL,
							kernel		= "radial",
							degree		= 3,
							gamma       = if (is.vector(x)) 1 else 1 / ncol(x),
          					coef0       = 0,
          					cost        = 1,
          					nu          = 0.5,
							wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
								"exponential", "gaussian", "optcosine", "rectangular", "triangular"), 
							bw, 
							k, 
							nn.only = TRUE,
          					class.weights = NULL,
				          	cachesize   = 40,
          					tolerance   = 0.001,
          					epsilon     = 0.1,
          					shrinking   = TRUE,
          					seed        = 1L,## argument of predict???
          					...,
          					subset      = NULL,
          					na.action = na.omit)
{

    if(inherits(x, "Matrix")) {
        library("SparseM")
        library("Matrix")
        x <- as(x, "matrix.csr")
    }
    if(inherits(x, "simple_triplet_matrix")) {
        library("SparseM")
        ind <- order(x$i, x$j)
        x <- new("matrix.csr",
                 ra = x$v[ind],
                 ja = x$j[ind],
                 ia = as.integer(cumsum(c(1, tabulate(x$i[ind])))),
                 dimension = c(x$nrow, x$ncol))
    }
    if (sparse <- inherits(x, "matrix.csr"))
        library("SparseM")

	formula <- !is.null(attr(x, "assign"))
	#print(formula)

    ## NULL parameters?
    if(is.null(degree)) stop(sQuote("degree"), " must not be NULL!")
    if(is.null(gamma)) stop(sQuote("gamma"), " must not be NULL!")
    if(is.null(coef0)) stop(sQuote("coef0"), " must not be NULL!")
    if(is.null(cost)) stop(sQuote("cost"), " must not be NULL!")
    if(is.null(nu)) stop(sQuote("nu"), " must not be NULL!")
    if(is.null(epsilon)) stop(sQuote("epsilon"), " must not be NULL!")
    if(is.null(tolerance)) stop(sQuote("tolerance"), " must not be NULL!")

    #xhold   <- if (fitted) x else NA
    x.scale <- y.scale <- NULL
    
    ## determine model type
    if (is.null(type)) type <-
        if (is.null(y)) "one-classification"
        else if (is.factor(y)) "C-classification"
        else "eps-regression"

    type <- pmatch(type, c("C-classification",
                           "nu-classification",
                           "one-classification",
                           "eps-regression",
                           "nu-regression"), 99) - 1

    if (type > 10) stop("wrong type specification!")

    kernel <- pmatch(kernel, c("linear",
                               "polynomial",
                               "radial",
                               "sigmoid"), 99) - 1

    if (kernel > 10) stop("wrong kernel specification!")

    nac <- attr(x, "na.action")
    
    ## scaling, subsetting, and NA handling
    if (sparse) {
        scale <- rep(FALSE, ncol(x))
        if(!is.null(y)) na.fail(y)
        x <- SparseM::t(SparseM::t(x)) ## make sure that col-indices are sorted
    } else {
    	x <- as.matrix(x, rownames.force = TRUE)
        ## subsetting and na-handling for matrices
        if (!formula) {
            if (!is.null(subset)) {
            	x <- x[subset,]
            	if (!is.null(y)) y <- y[subset]
            }
            if (is.null(y)) {
            	x <- na.action(x)
            	#df <- na.action(data.frame(cw, x))##
            	#cw <- df[,1])##
            	#x <- as.matrix(df[,-1], rownames.force = TRUE)##
            } else {
                # df <- na.action(data.frame(y, x))
                df <- na.action(structure(list(y = y, x = x), class = "data.frame", row.names = rownames(x)))
                y <- df$y #[,1]
                x <- df$x #as.matrix(df[,-1], rownames.force = TRUE)
                nac <-
                    attr(x, "na.action") <-
                        attr(y, "na.action") <-
                            attr(df, "na.action")
            }
        }
        #xhold   <- if (fitted) x else NA
        
        ## scaling
        if (length(scale) == 1)
            scale <- rep(scale, ncol(x))
        if (any(scale)) {
            co <- !apply(x[,scale, drop = FALSE], 2, var)
            if (any(co)) {
                warning(paste("Variable(s)",
                              paste(sQuote(colnames(x[,scale,
                                                      drop = FALSE])[co]),
                                    sep="", collapse=" and "),
                              "constant. Cannot scale data.")
                        )
                scale <- rep(FALSE, ncol(x))
            } else {
                xtmp <- scale(x[,scale])
                x[,scale] <- xtmp
                x.scale <- attributes(xtmp)[c("scaled:center","scaled:scale")]
                if (is.numeric(y) && (type > 2)) {
                    y <- scale(y)
                    y.scale <- attributes(y)[c("scaled:center","scaled:scale")]
                    y <- as.vector(y)
                }
            }
        }
    }

    ## further parameter checks
    nr <- nrow(x)

    if (!is.vector(y) && !is.factor (y) && type != 2)
        stop("y must be a vector or a factor.")
    if (type != 2 && length(y) != nr)
        stop("x and y don't match.")

    if (cachesize < 0.1)
        cachesize <- 0.1

    if (type > 2 && !is.numeric(y))
        stop("Need numeric dependent variable for regression.")

    lev <- lev1 <- NULL
    weightlabels <- NULL
    
    ## in case of classification: transform factors into integers
    if (type == 2) # one class classification --> set dummy
        y <- rep(1, nr)
    else
        if (is.factor(y)) {
            lev <- lev1 <- levels(y)
###
       		counts <- as.vector(table(y))
    		if (any(counts == 0)) {
        		empty <- lev[counts == 0]
        		warning(sprintf(ngettext(length(empty), "group %s is empty", 
            		"groups %s are empty"), paste(empty, collapse = ", ")), 
            		domain = NA)
        		lev1 <- lev[counts > 0]
    		}
			if (length(lev1) == 1L)
				stop("training data from only one class")
###            
            y <- as.integer(y)
            if (!is.null(class.weights)) {
                if (is.null(names(class.weights)))
                    stop("Weights have to be specified along with their according level names !")
                weightlabels <- match (names(class.weights), lev)
                if (any(is.na(weightlabels)))
                    stop("At least one level name is missing or misspelled.")
            }
        } else {
            if (type < 3) {
                if(any(as.integer(y) != y))
                    stop("dependent variable has to be of factor or integer type for classification mode.")
                y <- as.factor(y)
                lev <- levels(y)
###
    			if (length(lev) == 1L)
    				stop("training data from only one class")
###            
                y <- as.integer(y)
            } else lev <- unique(y)
        }

    nclass <- 2
    labels <- rep(0, 2)
    if (type < 2) {
    	nclass <- ifelse(is.null(lev1), length(lev), length(lev1))
    	labels <- unique(y)
    }

    if (type > 1 && length(class.weights) > 0) {
        class.weights <- NULL
        warning(sQuote("class.weights"), " are set to NULL for regression mode. For classification, use a _factor_ for ", sQuote("y"),
", or specify the correct ", sQuote("type"), " argument.")
    }

	## checks on k and bw
    if (is.character(wf)) {
    	m <- match.call(expand.dots = FALSE)
    	#m$x <- m$y <- m$scale <- m$type <- m$kernel <- m$degree <- m$gamma <- m$coef0 <- m$cost <- m$nu <-
    	#m$class.weights <- m$cachesize <- m$tolerance <- m$epsilon <- m$shrinking <- m$seed <- m$... <-
    	#m$subset <- m$na.action <- NULL
    	m$n <- nr
    	m[[1L]] <- as.name("checkwf")
   		if (formula)
    		check <- eval.parent(m)
    	else		
    		check <- eval.parent(m, n = 0)
    	cl <- match.call()
    	cl[[1]] <- as.name("ossvm")
    	return(structure(list (
        	call     = match.call(),
            x		 = x,
            y		 = y,
            wf		 = check$wf,
            bw       = check$bw,
            k        = check$k,
            nn.only  = check$nn.only, 
            adaptive = check$adaptive,
            variant  = check$variant,
            type     = type,
            kernel   = kernel,
            cost     = cost,
            degree   = degree,
            gamma    = gamma,
            coef0    = coef0,
            cachesize = cachesize,
            tolerance = tolerance,
            weightlabels = weightlabels,
            class.weights = class.weights,
            shrinking = shrinking,
            seed 	 = seed,
            nu       = nu,
            epsilon  = epsilon,
            sparse   = sparse,
            scaled   = scale,
            x.scale  = x.scale,
            y.scale  = y.scale,

			nclasses = nclass,
			labels   = labels,
            levels   = lev,
            na.action = nac), 
            class = "ossvm"))
    } else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    	if (!is.null(attr(wf, "adaptive"))) {
    		if (attr(wf, "adaptive")) {			# adaptive bandwidth
     				if (attr(wf, "k") + 1 > nr)
    					stop("'k + 1' is larger than 'nrow(x)'")
     				if (attr(wf, "nn.only")) 	# only knn
    					variant <- 3
    				else						# all observations
    					variant <- 4
    		} else {							# fixed bandwidth
    			if (!is.null(attr(wf, "k"))) {
    				if (attr(wf, "k") > nr)
    					stop("'k' is larger than 'nrow(x)'")
    				variant <- 2
    			} else 
    				variant <- 1
    		}
    	} else
    		variant <- NULL
    	return(structure(list (
        	call     = match.call(),
        	x		  = x,
            y		  = y,
            wf		  = wf,
            bw       = attr(wf, "bw"),
            k        = attr(wf, "k"),
            nn.only  = attr(wf, "nn.only"), 
            adaptive = attr(wf, "adaptive"),
            variant  = variant,
            type     = type,
            kernel   = kernel,
            cost     = cost,
            degree   = degree,
            gamma    = gamma,
            coef0    = coef0,
            cachesize = cachesize,
            tolerance = tolerance,
            weightlabels = weightlabels,
            class.weights = class.weights,
            shrinking = shrinking,
            seed 	 = seed,
            nu       = nu,
            epsilon  = epsilon,
            sparse   = sparse,
            scaled   = scale,
            x.scale  = x.scale,
            y.scale  = y.scale,

			nclasses = nclass,
			labels   = labels,
            levels   = lev,
            na.action = nac), 
            class = "ossvm"))
    } else
		stop("argument 'wf' has to be either a character or a function")
}



#' Classify multivariate observations in conjunction with \code{\link{ossvm}}.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"ossvm"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.ossvm(x)} regardless of 
#' the class of the object. 
#'
#' @title Predict New Observations with Observation Specific Support Vector Machines
#'
#' @param object Object of class \code{"ossvm"}.
#' @param newdata An object containing the new input data: either a matrix or a sparse matrix 
#' (object of class \code{\link[Matrix]{Matrix}} provided by the \pkg{Matrix} package, or of class 
#' \code{\link[SparseM]{matrix.csr}} provided by the \pkg{SparseM} package, or of class 
#' \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam} package). A vector will be 
#' transformed to a (n x 1) matrix.
#  A \code{data.frame} of cases to be classified or, if \code{object} has a
# \code{formula}, a \code{data.frame} with columns of the same names as the
# variables used. A vector will be interpreted as a row
# vector. If \code{newdata} is missing, an attempt will be made to
# retrieve the data used to fit the \code{ossvm} object.
#' @param decision.values Logical controlling whether the decision values of all binary classifiers 
#' computed in multiclass classification shall be computed and returned.
#' @param probability Logical indicating whether class probabilities should be computed and returned. 
#' @param \dots Further arguments.
#' @param na.action A function to specify the action to be taken if 'NA's are found. The default 
#' action is na.omit, which leads to rejection of cases with missing 
#'   values on any required variable. An alternative is na.fail, which causes an error if NA cases are found. 
#' (NOTE: If given, this argument must be named.)
#'
#' @return A vector of predicted values (for classification: a vector of labels, for density 
#' estimation: a logical vector). If \code{decision.value} is \code{TRUE}, 
#' the vector gets a \code{"decision.values"} attribute containing a \eqn{n * c} matrix (\eqn{n} number of 
#' predicted values, \eqn{c} number of classifiers) of all \eqn{c} binary 
#' classifiers' decision values. There are \eqn{k * (k - 1) / 2} classifiers (\eqn{k} number of classes). 
#' The colnames of the matrix indicate the labels of the two classes. 
#' If \code{probability} is \code{TRUE}, the vector gets a \code{"probabilities"} attribute 
#' containing a \eqn{n * k} matrix of the class probabilities.
#'
#' @seealso \code{\link{ossvm}}.
#'
#' 
#' @keywords classif
#' 
#' @method predict ossvm
#' @rdname predict.ossvm
#'
#' @S3method predict ossvm
#'
#' @useDynLib locClass

# seed argument of predict???
predict.ossvm <-
function (object, newdata,
          decision.values = FALSE,
          probability = FALSE,
          ...,
          na.action = na.omit)
{
	
	if (missing(newdata)) {
		newdata <- object$x
		
		sparse <- object$sparse
    	if (sparse)
       		library("SparseM")

		act <- attr(newdata, "na.action")
    	
    	rowns <- if (!is.null(rownames(newdata)))
        	rownames(newdata)
    	else
        	1:nrow(newdata)
		
   		# if (!is.null(Terms <- object$terms)) { ## same as inherits(object, "ossvm.formula")?
   			# newdata <- model.frame(object)
        	# #x <- model.matrix(delete.response(Terms), newdata, contrasts = object$contrasts)
       		# #xint <- match("(Intercept)", colnames(x), nomatch = 0L)
        	# #if (xint > 0L) 
            # #	x <- x[, -xint, drop = FALSE]
   		# } else {
            # newdata <- eval.parent(object$call$x)
        	# #if (is.null(dim(newdata))) 
            # #	dim(newdata) <- c(1L, length(newdata))
        	# #x <- as.matrix(newdata)
            # #if (any(is.na(x))) 
            # #    stop("missing values in 'x'")
   		# }
    } else {
    	if (inherits(newdata, "Matrix")) {
        	library("SparseM")
        	library("Matrix")
        	newdata <- as(newdata, "matrix.csr")
    	}
    	if (inherits(newdata, "simple_triplet_matrix")) {
       		library("SparseM")
       		ind <- order(newdata$i, newdata$j)
       		newdata <- new("matrix.csr",
            		        ra = newdata$v[ind],
                   			ja = newdata$j[ind],
                  		    ia = as.integer(cumsum(c(1, tabulate(newdata$i[ind])))),
                      		dimension = c(newdata$nrow, newdata$ncol))
   		}

    	sparse <- inherits(newdata, "matrix.csr")
    	if (object$sparse || sparse)
       		library("SparseM")
    
    	act <- NULL
    	if ((is.vector(newdata) && is.atomic(newdata)))
        	newdata <- t(t(newdata))
    	if (sparse)
        	newdata <- SparseM::t(SparseM::t(newdata))
    	preprocessed <- !is.null(attr(newdata, "na.action"))
    
    	rowns <- if (!is.null(rownames(newdata)))
        	rownames(newdata)
    	else
        	1:nrow(newdata)
    	if (!object$sparse) {
        	if (inherits(object, "ossvm.formula")) {
            	if(is.null(colnames(newdata)))
                	colnames(newdata) <- colnames(object$SV)
            	newdata <- na.action(newdata)
            	act <- attr(newdata, "na.action")
            	newdata <- model.matrix(delete.response(terms(object)),
                	                    as.data.frame(newdata))
        	} else {
            	newdata <- na.action(as.matrix(newdata))
            	act <- attr(newdata, "na.action")
        	}
    	}

    	if (!is.null(act) && !preprocessed)
        	rowns <- rowns[-act]

    	if (any(object$scaled))
        	newdata[,object$scaled] <-
            	scale(newdata[,object$scaled, drop = FALSE],
                	center = object$x.scale$"scaled:center",
                  	scale  = object$x.scale$"scaled:scale"
                  	)

	}


	wfs <- c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian",
		"optcosine", "rectangular", "triangular")
	if (is.function(object$wf) && !is.null(attr(object$wf,"name")) && attr(object$wf, "name") %in% wfs)
		object$wf <- attr(object$wf, "name")	
    if (is.character(object$wf)) {
		wfs <- paste(wfs, rep(1:3, each = length(wfs)), sep = "")
		wfs <- c(wfs, "cauchy4", "exponential4", "gaussian4")
		object$wf <- paste(object$wf, object$variant, sep = "")
		object$wf <- match(object$wf, wfs)
	}
# print(object$wf)
# print(class(object$wf))
#print(head(newdata))
	ret <- 	.Call("predossvm", 
		# train data
		as.double	(object$y),
		as.double	(if(object$sparse) object$x@ra else t(object$x)),
		as.integer	(nrow(object$x)), as.integer(ncol(object$x)),
		# sparse index info
		as.integer	(if(object$sparse) object$x@ia else 0),
		as.integer	(if(object$sparse) object$x@ja else 0),
		as.integer	(object$sparse),
		# weighting
		object$wf, #SEXP
		ifelse(is.integer(object$wf) && !is.null(object$bw), object$bw, 0),
		ifelse(is.integer(object$wf) && !is.null(object$k), as.integer(object$k), 0L),
		new.env(),
		# parameters
		as.integer (object$type),
        as.integer (object$kernel),
        as.integer (object$degree),
        as.double  (object$gamma),
        as.double  (object$coef0),
        as.double  (object$cachesize),
        as.double  (object$tolerance),
        as.double  (object$cost),
        as.double  (object$nu),
        as.integer (object$weightlabels),
        as.double  (object$class.weights),
        as.integer (length (object$class.weights)),
        as.double  (object$epsilon),
        as.integer (object$shrinking),
        # output        
        as.integer (decision.values),
        as.integer (probability),
        # seed
        as.integer (object$seed),
        # 
        nclasses = object$nclasses, ### length(object$levels)???
        # test data
        as.double  (if (sparse) newdata@ra else t(newdata)),
        as.integer (nrow(newdata)),
        as.integer (if (sparse) newdata@ia else 0),
        as.integer (if (sparse) newdata@ja else 0),
        as.integer (sparse))

	names(ret) <- c("error", "ret", "prob", "dec")
#	names(ret) <- c("error", "ret", "prob", "dec", "dist", "weights")
#print(ret)

    if (!is.null(ret$error))
        stop(paste(ret$error, "!", sep=""))

    ret2 <- if (is.character(object$levels)) # classification: return factors
        factor (object$levels[ret$ret], levels = object$levels)
    else if (object$type == 2) # one-class-classification: return TRUE/FALSE
        ret$ret == 1
    else if (any(object$scaled) && !is.null(object$y.scale)) # return raw values, possibly scaled back
        ret$ret * object$y.scale$"scaled:scale" + object$y.scale$"scaled:center"
    else
        ret$ret

    names(ret2) <- rowns
    ret2 <- napredict(act, ret2)

    if (decision.values) {
        colns = c()
        for (i in 1:(object$nclasses - 1))
            for (j in (i + 1):object$nclasses)
                colns <- c(colns,
                           paste(object$levels[sort(object$labels)[i]],#object$levels[object$labels[i]],
                                 "/", object$levels[sort(object$labels)[j]],#object$levels[object$labels[j]],
                                 sep = ""))
        attr(ret2, "decision.values") <-
            napredict(act,
                      matrix(ret$dec, nrow = nrow(newdata), byrow = TRUE,
                             dimnames = list(rowns, colns)
                             )
                      )
    }

    if (probability && object$type < 2)
        attr(ret2, "probabilities") <-
            napredict(act,
                      matrix(ret$prob, nrow = nrow(newdata), byrow = TRUE,
                             dimnames = list(rowns, object$levels[sort(object$labels)])#object$levels[object$labels])
                             )
                      )

    ret2
}



# @param x An \code{ossvm} object.
# @param \dots Further arguments to \code{\link{print}}.
#
#' @method print ossvm
#' @noRd
#'
#' @S3method print ossvm

print.ossvm <- function (x, ...) {
#    if (!is.null(cl <- x$call)) {
#        names(cl)[2L] <- ""
#        cat("Call:\n")
#        dput(cl, control = NULL)
#    }
    cat("\nCall:", deparse(x$call, 0.8 * getOption("width")), "\n", sep="\n")
    cat("Parameters:\n")
    cat("   SVM-Type: ", c("C-classification",
                           "nu-classification",
                           "one-classification",
                           "eps-regression",
                           "nu-regression")[x$type+1], "\n")
    cat(" SVM-Kernel: ", c("linear",
                           "polynomial",
                           "radial",
                           "sigmoid")[x$kernel+1], "\n")
    if (x$type==0 || x$type==3 || x$type==4)
        cat("       cost: ", x$cost, "\n")
    if (x$kernel==1)
        cat("     degree: ", x$degree, "\n")
    cat("      gamma: ", x$gamma, "\n")
    if (x$kernel==1 || x$kernel==3)
        cat("     coef.0: ", x$coef0, "\n")
    if (x$type==1 || x$type==2 || x$type==4)
        cat("         nu: ", x$nu, "\n")
    if (x$type==3) {
        cat("    epsilon: ", x$epsilon, "\n\n")
        if (x$compprob)
            cat("Sigma: ", x$sigma, "\n\n")
    }
    if(is.character(x$wf)) {
	    cat("\nWindow function: ")
        cat(x$wf, sep="\n")  
    } else {
        cat("\nWindow function:\n")
	    cat(deparse(x$wf), sep = "\n")  
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
