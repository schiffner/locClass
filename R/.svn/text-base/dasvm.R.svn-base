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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

#' A local version of Support Vector Machines for classification that puts increased emphasis on a good model 
#' fit near the decision boundary.
#'
#' The idea of Hand and Vinciotti (2003) to put increased weight on observations near 
#' the decision boundary is generalized to the multiclass case and applied to 
#' Support Vector Machines (SVM).
#'
#' Two different methods are implemented to achieve this.
#' The first one is based on the decision values. 
#' In order to deal with multiclass problems with k classes, \eqn{k>2}, \sQuote{libsvm} uses the
#' \sQuote{one-against-one}-approach, in which \eqn{k(k-1)/2} binary classifiers are
#' trained; the appropriate class is found by a voting scheme.
#' Hence, there are decision values for every binary classification problem. The absolute decision 
#' values are proportional to the distance between the training observations and the decision boundary.
#' A window function is applied to these distances in order to get observation weights.
#' 
#' The second method is based on posterior probabilities. 
#' The probability model for classification fits a logistic distribution
#' using maximum likelihood to the decision values of all binary
#' classifiers, and computes the a-posteriori class probabilities for the
#' multi-class problem using quadratic optimization. The probabilistic
#' regression model assumes (zero-mean) laplace-distributed errors for the
#' predictions, and estimates the scale parameter using maximum likelihood.
#' Observation weights are calculated based on the differences between the two largest estimated 
#' posterior probabilities.
#'
#' Since the decision boundary is not known in advance an iterative procedure is required.
#' First, an unweighted SVM is fitted to the data. 
#' Then either based on the estimated decision values or the estimated posterior probabilities 
#' observation weights are calculated.
#' Then a weighted SVM (see \code{\link{wsvm}}) is fitted using these weights. 
#' Calculation of weights and model fitting is done several times in turn. 
#' The number of iterations is determined by the \code{itr}-argument that defaults to 3.
#'
#' In order to calculate the weights a window function is applied to the decision values or 
#' posterior probabilities.
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{dasvm}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{dasvm}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' \code{dasvm} calls \code{\link{wsvm}} internally which is a version of Support Vector Machines that
#' can deal with case weights and which is a modified version of the \code{\link[e1071]{svm}} 
#' function in package \pkg{e1071} written by David Meyer 
#' (based on C/C++-code by Chih-Chung Chang and Chih-Jen Lin).
#' An extension of LIBSVM that can deal with case weights written by
#' Ming-Wei Chang, Hsuan-Tien Lin, Ming-Hen Tsai, Chia-Hua Ho and Hsiang-Fu Yu
#' is used. It is available at
#' \url{http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/#weights_for_data_instances}. 
#'
#'  \code{libsvm} internally uses a sparse data representation, which is 
#'  also high-level supported by the package \pkg{SparseM}.
#'  
#'  If the predictor variables include factors, the formula interface must be used to get a
#'  correct model matrix.
#'
#' Data are scaled internally, usually yielding better results. 
#' Parameters of SVM-models usually \emph{must} be tuned to yield sensible results!
#'
#'
#' @title Discriminant Adaptive Support Vector Machine
#'
#' @param formula A symbolic description of the model to be fit.
#' @param data An optional data frame containing the variables in the model. By default 
#'   the variables are taken from the environment which \code{dasvm} is called from.
#' @param x (Required if no \code{formula} is given as principal argument.) A data matrix, a 
#'   vector, or a sparse matrix (object of class \code{\link[Matrix]{Matrix}} provided by the 
#'   \pkg{Matrix} package, or of class \code{\link[SparseM]{matrix.csr}} provided by the \pkg{SparseM} 
#'   package, or of class \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam} package).
#' @param y (Only if no \code{formula} is given as principal argument.) A response vector with 
#'   one label for each row/component of x. Should be a factor, otherwise it is coerced to a factor
#'   with a warning.
#' @param wf A window function which is used to calculate weights that are introduced into 
#' the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#' For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision 
#'   boundary to be used in the fitting process. 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite 
#'   support and if \code{k} is specified.) Should
#'   only the \code{k} nearest neighbors or all observations receive positive weights? 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param itr Number of iterations for model fitting, defaults to 3. See also the Details section.
#' @param case.weights Initial observation weights (defaults to a vector of 1s).
#' @param method The method for adaptation to the decision boundary, either \code{"prob"} or \code{"decision"}. 
#'   Defaults to "prob".
#' @param scale A logical vector indicating the variables to be scaled. If \code{scale} is of length 1, the 
#'   value is recycled as many times as needed. Per default, data are scaled internally (both \code{x} and \code{y} 
#'   variables) to zero mean and unit variance. The center and scale values are returned and used for later 
#'   predictions.
#' @param type \code{dasvm} can be used only as a classification machine, hence valid options are:
#'   \itemize{
#'   \item \code{C-classification}
#'   \item \code{nu-classification}
#'   }
#'   \code{C-classification} is default.
#' @param \dots Further parameters that are passed to \code{wsvm.default}, e.g. \code{kernel}, \code{degree}, 
#'   \code{gamma}, \code{coef0}, \code{cost}, \code{nu}, \code{class.weights}, \code{case.weights}, \code{cachesize}, 
#'   \code{tolerance}, \code{shrinking}, \code{cross}, \code{probability}, \code{fitted}, and 
#'   \code{seed}. Note that \code{epsilon} is a parameter that is only needed in the regression case and thus will
#'   have no effect if specified.
#' @param subset An index vector specifying the cases to be used in the training sample. 
#'   (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if \code{NA}s are found. The default action is 
#'   \code{na.omit}, which leads to rejection of cases with missing values on any required variable. An alternative 
#'   is \code{na.fail}, which causes an error if \code{NA} cases are found. (NOTE: If given, this argument must be named.)
#'
#' @return
#' An object of class \code{"dasvm.formula"} or \code{"dasvm"} inheriting from \code{"wsvm.formula"} or 
#' \code{"wsvm"} and \code{"svm"}, a \code{list} containing the following components:
#'  \item{case.weights}{A list of length \code{itr + 1}. The initial observation weights (a vector of 1s if none were given) 
#'    and the observation weights calculated in the individual iterations.}
#'  \item{itr}{The number of iterations used.}
#'  \item{wf}{The window function used. Always a function, even if the input was a string.}
#'  \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#'	\item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#'  \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of the functions documented 
#'	in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	 specified.) \code{TRUE} if only the \code{k} nearest neighbors recieve a positive weight, \code{FALSE} otherwise.}
#'  \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the local density of data points, 
#'	\code{FALSE} if the bandwidth is fixed.}
#'  \item{call}{The (matched) function call.}
#'
#' @references Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' @seealso \code{\link{predict.dasvm}}, \code{\link{wsvm}} for a weighted version of Support Vector Machines.
#'
#'
#' @keywords classif
#'
#' @aliases dasvm dasvm.default dasvm.formula
#'
#' @export
#'
#' @import e1071
#' @examples
#' fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 0.5)
#' pred <- predict(fit)
#' mean(pred != iris$Species)

# todo: cross: even if cross = TRUE set to FALSE during iterations to avoid unnecessary cross-validation?

dasvm <- function(x, ...)
	UseMethod("dasvm")	



#' @rdname dasvm
#' @method dasvm formula
#'
#' @S3method dasvm formula

dasvm.formula <-
function (formula, data = NULL, case.weights = rep(1, nrow(data)), ..., subset, na.action = na.omit, scale = TRUE)
{
    call <- match.call()
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    if (identical(class(eval.parent(m$data)), "matrix"))
        m$data <- as.data.frame(eval.parent(m$data))
    m$case.weights <- case.weights
    m$... <- NULL
    m$scale <- NULL
    m[[1]] <- as.name("model.frame")
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    case.weights <- m[,"(case.weights)"]
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
	#y <- model.response(m)
    attr(x, "na.action") <- attr(y, "na.action") <- attr(case.weights, "na.action") <- attr(m, "na.action")
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
    ret <- dasvm.default (x, y, scale = scale, case.weights = case.weights, ..., na.action = na.action)
    ret$call <- call
    ret$call[[1]] <- as.name("dasvm")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("dasvm.formula", "wsvm.formula", class(ret))
    return (ret)
}



#' @rdname dasvm
#' @method dasvm default
#'
#' @S3method dasvm default

dasvm.default <-
function (x,
          y           = NULL,
		  wf 		  = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular"), 
		  bw, 
		  k, 
		  nn.only, 
		  itr = 3,  
		  method 	  = c("prob", "decision"),
          scale       = TRUE,
          type        = NULL,
          case.weights = rep(1, nrow(x)),
          ...,
          subset       = NULL,
          na.action    = na.omit)
{
	dasvm.fit.prob <- function(x, y, wf, itr, case.weights = rep(1, nrow(x)), scale, type, probability = FALSE, ...) {
		w <- list()
		n <- nrow(x)
		case.weights <- case.weights/sum(case.weights) * n		# rescale the weights such that they sum up to n
		w[[1]] <- case.weights
		names(w[[1]]) <- rownames(x)
		res <- wsvm.default(x = x, y = y, scale = scale, type = type, case.weights = case.weights, probability = TRUE, ...)
		for (i in seq_len(itr)) {
			# 1. prediction
			post <- attr(predict(res, newdata = x, probability = TRUE, ...), "probabilities")
			if (any(!is.finite(post)))
				stop("inifinite, NA or NaN values in 'post', may indiciate numerical problems due to small observation weights, please check your settings of 'bw', 'k' and 'wf'")
			# 2. calculate weights and fit model
			spost <- apply(post, 1, sort, decreasing = TRUE)
			weights <- wf((spost[1,] - spost[2,]))    	# largest if both probabilities are equal
			if (all(weights == 0)) {
				stop("all observation weights are zero")
			}
			weights <- weights/sum(weights) * n			# rescale the weights such that they sum up to n
			names(weights) <- rownames(x)
			# 3. check if break		
			freqs <- tapply(weights, y, sum)
			if (any(freqs == 0L, na.rm = TRUE))               	# classes where all weights are zero
				warning("for at least one class all weights are zero")
			if (sum(freqs > 0, na.rm = TRUE) <= 1L) { 			# additionally look if only one single class left
				warning("training data from only one class, breaking out of iterative procedure")
				itr <- i - 1
				break
			} else {			 			
				w[[i+1]] <- weights
				res <- wsvm.default(x = x, y = y, scale = scale, type = type, case.weights = weights, probability = TRUE, ...)
			}
		}
		if (!probability) {
			res$compprob = FALSE
			res["probA"] <- list(probA = NULL)
			res["probB"] <- list(probB = NULL)
			res["sigma"] <- list(sigma = NULL)
		}
		names(w) <- seq_along(w) - 1
		res$case.weights <- w
		attr(res$case.weights, "na.action") <- attr(w[[1]], "na.action")
		attr(res$case.weights[[1]], "na.action") <- NULL
		res$itr <- itr
		return(res)
	}
	dasvm.fit.decision <- function(x, y, wf, itr, case.weights = rep(1, nrow(x)), scale, type, ...) {
		w <- list()
		n <- nrow(x)
		case.weights <- case.weights/sum(case.weights) * n		# rescale the weights such that they sum up to n
		w[[1]] <- case.weights
		names(w[[1]]) <- rownames(x)
		res <- wsvm.default(x = x, y = y, scale = scale, type = type, case.weights = case.weights, ...)
		
		for (i in seq_len(itr)) {
			# 1. prediction
			decision <- attr(predict(res, newdata = x, decision.values = TRUE, ...), "decision.values")
#print(decision)
			if (ncol(decision) == 1L) {
#print("2 classes")
				decision <- as.vector(decision)
				# 2. calculate weights and fit model
				weights <- wf(abs(decision))    			# largest if decision value = 0
				if (all(weights == 0)) {
					stop("all observation weights are zero")
				}
				weights <- weights/sum(weights) * n		# rescale the weights such that they sum up to n					
				names(weights) <- rownames(x)
				# 3. check if break
				freqs <- tapply(weights, y, sum)
#cat("iteration", i, "\n")
#print(freqs)
				if (any(freqs == 0L, na.rm = TRUE))               	# classes where all weights are zero
					warning("for at least one class all weights are zero")
				if (sum(freqs > 0, na.rm = TRUE) <= 1L) { 			# additionally look if only one single class left
					warning("training data from only one class, breaking out of iterative procedure")
					itr <- i - 1
					break
				} else {			 			
					w[[i+1]] <- weights
					res <- wsvm.default(x = x, y = y, scale = scale, type = type, case.weights = weights, ...)
				}
			} else {
#print("> 2 classes")
				# for each training observation consider the two-class problems it is involved in,
				# take the minimum absolute decision value over all two-class problems (i.e. determine the two-class
				# problem for which the training observation is closest to the decision boundary) and use this decision
				# value to determine the weight
				# 2. calculate weights and fit model
				prob <- sapply(1:n, function(x) min(abs(decision[x, grep(y[x], colnames(decision))])))
				weights <- wf(prob)    				# largest if decision value is zero
				weights <- weights/sum(weights) * n	# rescale the weights such that they sum up to n
				names(weights) <- rownames(x)
				# 3. check if break
				freqs <- tapply(weights, y, sum)
#cat("iteration", i, "\n")
#print(freqs)
				if (any(freqs == 0L, na.rm = TRUE))               	# classes where all weights are zero
					warning("for at least one class all weights are zero")
				if (sum(freqs > 0, na.rm = TRUE) <= 1L) { 			# additionally look if only one single class left
					warning("training data from only one class, breaking out of iterative procedure")
					itr <- i - 1
					break
				} else {			 			
					w[[i+1]] <- weights
					res <- wsvm.default(x = x, y = y, scale = scale, type = type, case.weights = weights, ...)
				}
			}
		}
		names(w) <- seq_along(w) - 1
		res$case.weights <- w
		attr(res$case.weights, "na.action") <- attr(w[[1]], "na.action")
		attr(res$case.weights[[1]], "na.action") <- NULL
		res$itr <- itr
		return(res)
	}
	
    if (inherits(x, "Matrix")) {
        library("SparseM")
        library("Matrix")
        x <- as(x, "matrix.csr")
    }
    if (inherits(x, "simple_triplet_matrix")) {
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

	method <- match.arg(method)
	
	n <- nrow(x)

	if (is.null(y))
		stop("y is missing")
	# else {
		# if (!is.factor(y)) {
			# y <- as.factor(y)
			# warning("'y' was coerced to a factor")
		# }
	# }	
	
    x.scale <- y.scale <- NULL
    formula <- !is.null(attr(x, "assign"))

	## check/determine model type
    if (is.null(type)) 
    	type <- "C-classification"
        
    if (!type %in% c("C-classification", "nu-classification"))
    	stop ("only classification is supported by dasvm")

    nac <- attr(x, "na.action")
    
    if (length(case.weights) != n)
    	stop("'nrow(x)' and 'length(case.weights)' are different")
   
    ## scaling, subsetting, and NA handling
    if (sparse) {
        scale <- rep(FALSE, ncol(x))
        if (!is.null(y)) na.fail(y)
        na.fail(case.weights) #?
        x <- SparseM::t(SparseM::t(x)) ## make sure that col-indices are sorted
    } else {
    	x <- as.matrix(x, rownames.force = TRUE)

        ## subsetting and na-handling for matrices
        if (!formula) {
            if (!is.null(subset)) {
            	case.weights <- case.weights[subset]  
            	x <- x[subset, , drop = FALSE]
            	if (!is.null(y)) y <- y[subset]
            }
            df <- na.action(structure(list(y = y, case.weights = case.weights, x = x), class = "data.frame", row.names = rownames(x)))
            y <- df$y
            case.weights <- df$case.weights
            x <- df$x
                nac <-
                    attr(x, "na.action") <-
                        attr(y, "na.action") <-
                        	attr(case.weights, "na.action") <-
                            	attr(df, "na.action")
        }

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
            }
        }
    }

    
    if (any(case.weights < 0))
        stop("'case.weights' have to be larger or equal to zero")
    if (all(case.weights == 0))
        stop("all 'case.weights' are zero")

    if (!missing(itr)) {
    	if (itr < 1)
			stop("'itr' must be >= 1")
    	if (abs(itr - round(itr)) > .Machine$double.eps^0.5)
       		warning("'itr' is not a natural number and is rounded off")
    }
    if (is.character(wf)) {
    	m <- match.call(expand.dots = FALSE)
    	m$n <- n
    	m[[1L]] <- as.name("generatewf")
    	if (formula)
    		wf <- eval.parent(m)
    	else		
    		wf <- eval.parent(m, n = 0)
    } else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    	if (!is.null(attr(wf, "adaptive"))) {
    		if (attr(wf, "adaptive")) {
    			if (!is.null(attr(wf, "k")) && attr(wf, "k") + 1 > n)
    				stop("'k + 1' is larger than 'nrow(x)'")
    		} else {
    			if (!is.null(attr(wf, "k")) && attr(wf, "k") > n)
    				stop("'k' is larger than 'nrow(x)'")
    		}
    	}
    } else
    	stop("argument 'wf' has to be either a character or a function")

	if (method == "prob")	
		res <- dasvm.fit.prob(x = x, y = y, wf = wf, itr = itr, scale = scale, type = type, case.weights = case.weights, ...)
	else
		res <- dasvm.fit.decision(x = x, y = y, wf = wf, itr = itr, scale = scale, type = type, case.weights = case.weights, ...)
    res <- c(res, list(wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive"), method = method))
    res$x.scale <- x.scale
    cl <- match.call()
    cl[[1]] <- as.name("dasvm")
    res$call <- cl
    class(res) <- c("dasvm", "wsvm", "svm")
    return(res)
}



# @param x A \code{dasvm} object.
# @param ... Further arguments to \code{\link{print}}.
#
#' @method print dasvm
#' @noRd
#'
#' @S3method print dasvm

print.dasvm <- function (x, ...) {
    NextMethod(x, ...)
    if(!is.null(attr(x$wf, "name"))) {
        cat("\nWindow function: ")
        cat(deparse(attr(x$wf, "name")), sep = "\n")  
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
    cat("Iterations: ", x$itr, "\n")
    cat("Method: ", x$method, "\n")    
	invisible(x)
}



#' This function predicts values based upon a model trained by \code{\link{dasvm}}.
#'
#' @title Predict Method for Discriminant Adaptive Support Vector Machines
#'
#' @param object Object of class \code{"dasvm"}, created by \code{dasvm}.
#' @param newdata An object containing the new input data: either a
#'  matrix or a sparse matrix (object of class
#'    \code{\link[Matrix]{Matrix}} provided by the \pkg{Matrix} package,
#'    or of class \code{\link[SparseM]{matrix.csr}}
#'    provided by the \pkg{SparseM} package, or of class
#'    \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam}
#'    package). A vector will
#'    be transformed to a n x 1 matrix.
#' @param \dots Further arguments to \code{\link{wsvm}}, e.g. \code{decision.values}, 
#'   \code{probability}, \code{na.action}.
#'
#' @return
#'  A vector of predicted values (for classification: a vector of labels, for density
#'  estimation: a logical vector). If \code{decision.value} is
#'  \code{TRUE}, the vector gets a \code{"decision.values"} attribute
#'  containing a \eqn{n * c} matrix (n number of predicted values, \eqn{c} number of
#'  classifiers) of all \eqn{c} binary classifiers' decision values. There are 
#'  \eqn{k * (k - 1) / 2} classifiers (\eqn{k} number of classes). The colnames of
#'  the matrix indicate the labels of the two classes. If \code{probability} is
#'  \code{TRUE}, the vector gets a \code{"probabilities"} attribute
#'  containing a \eqn{n * k} matrix of the class probabilities.
#'
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' @note If the training set was scaled by \code{\link{dasvm}} (done by default), the
#'  new data is scaled accordingly using scale and center of
#'  the training data.
#'
#' @examples
#' fit1 <- wsvm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear", 
#' 	  probability = TRUE)
#' pred <- predict(fit1)
#' mean(pred != iris$Species)
#' 
#' fit2 <- dasvm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear",
#'     wf = "gaussian", bw = 0.5, method = "prob", probability = TRUE)
#' pred <- predict(fit2)
#' mean(pred != iris$Species)
#'
#' fit3 <- dasvm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear",
#'     wf = "gaussian", bw = 0.8, method = "decision", probability = TRUE)
#' pred <- predict(fit3)
#' mean(pred != iris$Species)
#' 
#' ## plot of decision boundary (maximum posterior probabilities):
#' grid <- expand.grid(Sepal.Length = seq(4,8,0.1), Sepal.Width = seq(2,5,0.1)) 
#' 
#' probgrid1 <- attr(predict(fit1, newdata = grid, probability = TRUE), "probabilities")
#' decgrid1 <- attr(predict(fit1, newdata = grid, decision.values = TRUE), "decision.values")
#' probgrid2 <- attr(predict(fit2, newdata = grid, probability = TRUE), "probabilities")
#' decgrid3 <- attr(predict(fit3, newdata = grid, decision.values = TRUE), "decision.values")
#' 
#' par(mfrow = c(1,2))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(apply(probgrid1, 1, max)), nrow = length(seq(4,8,0.1))))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(apply(probgrid2, 1, max)), nrow = length(seq(4,8,0.1))))
#' points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#'     cex = fit2$case.weights[[3]]*2, col = as.numeric(iris$Species))
#'
#' par(mfrow = c(1,2))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(decgrid1[,1]), nrow = length(seq(4,8,0.1))))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(decgrid3[,1]), nrow = length(seq(4,8,0.1))))
#' points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#'     cex = fit3$case.weights[[3]]*2, col = as.numeric(iris$Species))
#'
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(decgrid1[,2]), nrow = length(seq(4,8,0.1))))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(decgrid3[,2]), nrow = length(seq(4,8,0.1))))
#' points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#'     cex = fit3$case.weights[[3]]*2, col = as.numeric(iris$Species))
#'
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(decgrid1[,3]), nrow = length(seq(4,8,0.1))))
#' contour(seq(4,8,0.1), seq(2,5,0.1), 
#'     matrix(as.numeric(decgrid3[,3]), nrow = length(seq(4,8,0.1))))
#' points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#'     cex = fit3$case.weights[[3]]*2, col = as.numeric(iris$Species))
#'
#'
#' @keywords neural, nonlinear, classif
#'
#' @seealso \code{\link{dasvm}}, \code{\link{wsvm}}, \code{\link[e1071]{svm}}.
#'
#' @method predict dasvm
#' @rdname predict.dasvm
#'
#' @S3method predict dasvm

#data(iris)
#attach(iris)
#
### classification mode
## default with factor response:
#model <- wsvm(Species ~ ., data = iris)
#
## alternatively the traditional interface:
#x <- subset(iris, select = -Species)
#y <- Species
#model <- wsvm(x, y, probability = TRUE) 
#
#print(model)
#summary(model)
#
## test with train data
#pred <- predict(model, x)
## (same as:)
#pred <- fitted(model)
#
## compute decision values and probabilites
#pred <- predict(model, x, decision.values = TRUE, probability = TRUE)
#attr(pred, "decision.values")[1:4,]
#attr(pred, "probabilities")[1:4,]
#
### try regression mode on two dimensions
#
## create data
#x <- seq(0.1, 5, by = 0.05)
#y <- log(x) + rnorm(x, sd = 0.2)
#
## estimate model and predict input values
#m   <- wsvm(x, y)
#new <- predict(m, x)
#
## visualize
#plot   (x, y)
#points (x, log(x), col = 2)
#points (x, new, col = 4)
#
### density-estimation
#
## create 2-dim. normal with rho=0:
#X <- data.frame(a = rnorm(1000), b = rnorm(1000))
#attach(X)
#
## traditional way:
#m <- wsvm(X, gamma = 0.1)
#
## formula interface:
#m <- wsvm(~., data = X, gamma = 0.1)
## or:
#m <- wsvm(~ a + b, gamma = 0.1)
#
## test:
#newdata <- data.frame(a = c(0, 4), b = c(0, 4))
#predict (m, newdata)
#
## visualize:
#plot(X, col = 1:1000 \%in\% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
#points(newdata, pch = "+", col = 2, cex = 5)


predict.dasvm <- function(object, newdata, ...) {
    if (!inherits(object, "dasvm")) 
        stop("object not of class \"dasvm\"")
    NextMethod(object, newdata, ...)
}



#' @method weights dasvm
#' @noRd
#'
#' @S3method weights dasvm
#' @importFrom stats weights

weights.dasvm <- function (object, ...) {
    if (!inherits(object, "dasvm")) 
        stop("object not of class \"dasvm\"")
	NextMethod(object, ...)
}
