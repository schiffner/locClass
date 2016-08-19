#' This is a modification  of the \code{\link[e1071]{svm}} function in package \pkg{e1071} 
#' that can deal with observation weights.
#'
#' \code{wsvm} is used to train a support vector machine with case weights. 
#' It can be used to carry
#' out general regression and classification (of nu and epsilon-type), as
#' well as density-estimation. A formula interface is provided.
#'
#' This function is a modification of the \code{\link[e1071]{svm}} function in package 
#' \pkg{e1071} written by David Meyer 
#' (based on C/C++-code by Chih-Chung Chang and Chih-Jen Lin).
#' An extension of LIBSVM that can deal with case weights written by
#' Ming-Wei Chang, Hsuan-Tien Lin, Ming-Hen Tsai, Chia-Hua Ho and Hsiang-Fu Yu
#' is used. It is available at
#' \url{http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/#weights_for_data_instances}. 
#'
#'  For multiclass-classification with \eqn{k} levels, \eqn{k>2}, \code{libsvm} uses the
#'  \sQuote{one-against-one}-approach, in which \eqn{k(k-1)/2} binary classifiers are
#'  trained; the appropriate class is found by a voting scheme.
#'  
#'  \code{libsvm} internally uses a sparse data representation, which is 
#'  also high-level supported by the package \pkg{SparseM}.
#'  
#'  If the predictor variables include factors, the formula interface must be used to get a
#'  correct model matrix.
#'
#'  \code{plot.svm} allows a simple graphical
#'  visualization of classification models.
#'
#'  The probability model for classification fits a logistic distribution
#'  using maximum likelihood to the decision values of all binary
#'  classifiers, and computes the a-posteriori class probabilities for the
#'  multi-class problem using quadratic optimization. The probabilistic
#'  regression model assumes (zero-mean) laplace-distributed errors for the
#'  predictions, and estimates the scale parameter using maximum likelihood.
#'
#' Data are scaled internally, usually yielding better results. 
#' Parameters of SVM-models usually \emph{must} be tuned to yield sensible results!
#'
#' @title Weighted Support Vector Machines
#'
#' @param formula A symbolic description of the model to be fit.
#' @param data An optional data frame containing the variables in the model. By default 
#'   the variables are taken from the environment which \code{wsvm} is called from.
#' @param x (Required if no \code{formula} is given as principal argument.) A data matrix, a 
#'   vector, or a sparse matrix (object of class \code{\link[Matrix]{Matrix}} provided by the 
#'   \pkg{Matrix} package, or of class \code{\link[SparseM]{matrix.csr}} provided by the \pkg{SparseM} 
#'   package, or of class \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam} package).
#' @param y (Only if no \code{formula} is given as principal argument.) A response vector with 
#'   one label for each row/component of \code{x}. Can be either a factor (for classification tasks) or 
#'   a numeric vector (for regression).
#' @param scale A logical vector indicating the variables to be scaled. If \code{scale} is of length 1, 
#'   the value is recycled as many times as needed. Per default, 
#'   data are scaled internally (both \code{x} and \code{y} variables) to zero mean and unit variance. 
#'   The center and scale values are returned and used for later predictions.
#' @param type \code{wsvm} can be used as a classification machine, as a regression machine, or for 
#'   novelty detection. Depending of whether \code{y} is a factor or not, 
#'   the default setting for \code{type} is \code{C-classification} or \code{eps-regression}, respectively, 
#'   but may be overwritten by setting an explicit value.
#'   \cr
#'   Valid options are:
#'   \itemize{
#'   \item \code{C-classification}
#'   \item \code{nu-classification}
#'   \item \code{one-classification} (for novelty detection)
#'   \item \code{eps-regression}
#'   \item \code{nu-regression}
#'   }
#' @param kernel The kernel used in training and predicting. You might consider changing some of the 
#'    following parameters, depending on the kernel type. 
#'	  \cr
#'    \describe{
#'      \item{linear:}{\eqn{u'v}{u'*v}}
#'      \item{polynomial:}{\eqn{(\gamma u'v + coef0)^{degree}}{(gamma*u'*v + coef0)^degree}}
#'      \item{radial basis:}{\eqn{\exp(-\gamma |u-v|^2)}{exp(-gamma*|u-v|^2)}}
#'      \item{sigmoid:}{\eqn{\tanh(\gamma u'v + coef0)}{tanh(gamma*u'*v + coef0)}}
#'    }
#' @param degree Parameter needed for kernel of type \code{polynomial} (default: 3).
#' @param gamma Parameter needed for all kernels except \code{linear} (default: 1/(data dimension)).
#' @param coef0 Parameter needed for kernels of type \code{polynomial} and \code{sigmoid} (default: 0).
#' @param cost Cost of constraints violation (default: 1) --- it is the \sQuote{C}-constant of the 
#'   regularization term in the Lagrange formulation.
#' @param nu Parameter needed for \code{nu-classification}, \code{nu-regression}, and \code{one-classification}.
#' @param class.weights A named vector of weights for the different classes, used for asymmetric class sizes. 
#'    Not all factor levels have to be supplied (default weight: 1). 
#'    All components have to be named.
#' @param case.weights A vector of observation weights (default: a vector of 1s).
#' @param cachesize Cache memory in MB (default: 40).
#' @param tolerance Tolerance of termination criterion (default: 0.001).
#' @param epsilon epsilon in the insensitive-loss function (default: 0.1).
#' @param shrinking Option whether to use the shrinking-heuristics (default: \code{TRUE}).
#' @param cross If an integer value k>0 is specified, a k-fold cross validation on the training data 
#'   is performed to assess the quality of the model: the accuracy 
#'   rate for classification and the Mean Squared Error for regression.
#' @param fitted Logical indicating whether the fitted values should be computed and included in 
#'   the model or not (default: \code{TRUE}).
#' @param probability Logical indicating whether the model should allow for probability predictions 
#'   (default: \code{FALSE}).
#' @param seed Integer seed for libsvm (used for cross-validation and probability prediction models).
#' @param \dots Additional parameters for the low level fitting function \code{wsvm.default}.
#' @param subset An index vector specifying the cases to be used in the training sample. 
#'   (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if \code{NA}s are found. 
#'   The default action is \code{na.omit}, which 
#'   leads to rejection of cases with missing values on any required variable. An alternative is 
#'   \code{na.fail}, which causes an 
#'   error if \code{NA} cases are found. (NOTE: If given, this argument must be named.)
#'
#' @return An object of class \code{"wsvm"}, inheriting from \code{"svm"} containing the 
#'  fitted model, including:
#'  \item{SV}{The resulting support vectors (possibly scaled).}
#'  \item{index}{The index of the resulting support vectors in the data
#'    matrix. Note that this index refers to the preprocessed data (after
#'    the possible effect of \code{na.omit} and \code{subset})}
#'  \item{coefs}{The corresponding coefficients times the training labels.}
#'  \item{rho}{The negative intercept.}
#'  \item{obj}{The value(s) of the objective function.}
#'  \item{sigma}{In case of a probabilistic regression model, the scale
#'    parameter of the hypothesized (zero-mean) laplace distribution estimated by
#'    maximum likelihood.}
#'  \item{probA, probB}{numeric vectors of length k(k-1)/2, k number of
#'    classes, containing the parameters of the logistic distributions fitted to
#'    the decision values of the binary classifiers (1 / (1 + exp(a x + b))).}
#'
#' @note This modification is not well-tested.
#'
#' @references
#'      Chang, Chih-Chung and Lin, Chih-Jen:\cr
#'      \emph{LIBSVM: a library for Support Vector Machines}\cr
#'      \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}
#'
#'      Exact formulations of models, algorithms, etc. can be found in the
#'      document:\cr
#'      Chang, Chih-Chung and Lin, Chih-Jen:\cr
#'      \emph{LIBSVM: a library for Support Vector Machines}\cr
#'      \url{http://www.csie.ntu.edu.tw/~cjlin/papers/libsvm.ps.gz}
#'
#'      More implementation details and speed benchmarks can be found on:
#'      Rong-En Fan and Pai-Hsune Chen and Chih-Jen Lin:\cr
#'      \emph{Working Set Selection Using the Second Order Information for Training SVM}\cr
#'      \url{http://www.csie.ntu.edu.tw/~cjlin/papers/quadworkset.pdf}
#'    
#' 
#' @seealso \code{\link{predict.wsvm}}, \code{\link[e1071]{plot.svm}} in package \pkg{e1071}, 
#'    \code{\link[SparseM]{matrix.csr}} (in package \pkg{SparseM}).
#'
#' @family svm
#'
#' @examples
#'   data(iris)
#'   attach(iris)
#'
#'  ## classification mode
#'  ## default with factor response:
#'    model <- wsvm(Species ~ ., data = iris)
#'
#' # alternatively the traditional interface:
#'   x <- subset(iris, select = -Species)
#'   y <- Species
#'   model <- wsvm(x, y) 
#'
#'   print(model)
#'  summary(model)
#'
#'  # test with train data
#'   pred <- predict(model, x)
#'   # (same as:)
#'    pred <- fitted(model)
#'
#'    # Check accuracy:
#'    table(pred, y)
#'
#'  # compute decision values and probabilities:
#'   pred <- predict(model, x, decision.values = TRUE)
#'   attr(pred, "decision.values")[1:4,]
#'
#'   ## visualize (classes by color, SV by crosses):
#'   plot(cmdscale(dist(iris[,-5])),
#'     col = as.integer(iris[,5]),
#'     pch = c("o","+")[1:150 %in% model$index + 1])
#'
## try regression mode on two dimensions
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
#plot(x, y)
#points(x, log(x), col = 2)
#points(x, new, col = 4)
#
#'  ## density-estimation
#'
#'   # create 2-dim. normal with rho=0:
#'   X <- data.frame(a = rnorm(1000), b = rnorm(1000))
#'   attach(X)
#'
#'   # traditional way:
#'    m <- wsvm(X, gamma = 0.1)
#'
#'   # formula interface:
#'   m <- wsvm(~., data = X, gamma = 0.1)
#'
#'   # test:
#'   newdata <- data.frame(a = c(0, 4), b = c(0, 4))
#'   predict (m, newdata)
#'
#'  ## visualize:
#'  plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
#'   points(newdata, pch = "+", col = 2, cex = 5)
#'
#'  ## weights: (example not particularly sensible)
#'  i2 <- iris
#'  levels(i2$Species)[3] <- "versicolor"
#'  summary(i2$Species)
#'  wts <- 100 / table(i2$Species)
#'  wts
#'  m <- wsvm(Species ~ ., data = i2, class.weights = wts)
#'
#'  ## case.weights:
#'  fit <- wsvm(Species ~ ., data = iris, wf = "gaussian", bw = 0.5, case.weights = rep(c(0.5,1),75))
#'  pred <- predict(fit)
#'  mean(pred != iris$Species)
#'
#' @keywords neural nonlinear classif
#' 
#' @useDynLib locClass
#'
#' @aliases wsvm wsvm.default wsvm.formula
#'
#' @export
#'
#' @import e1071



wsvm <- function (x, ...)
    UseMethod ("wsvm")



#' @family svm
#'
#' @rdname wsvm
#' @export

wsvm.formula <- function (formula, data = NULL, case.weights = rep(1, nrow(data)), ..., subset, na.action = na.omit, scale = TRUE) {
    call <- match.call()
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    if (identical(class(eval.parent(m$data)), "matrix"))
        m$data <- as.data.frame(eval.parent(m$data))
    m$... <- NULL
    m$scale <- NULL
    m$weights <- case.weights
    m[[1]] <- as.name("model.frame")
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    case.weights <- model.weights(m)
    #cw <- m[,"(cw)"]
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
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
    ret <- wsvm.default (x, y, scale = scale, case.weights = case.weights, ..., na.action = na.action)
    ret$call <- call
    ret$call[[1]] <- as.name("wsvm")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("wsvm.formula", class(ret))
    return (ret)
}



#' @family svm
#'
#' @rdname wsvm
#' @export

wsvm.default <-
function (x,
          y           = NULL,
          scale       = TRUE,
          type        = NULL,
          kernel      = "radial",
          degree      = 3,
          gamma       = if (is.vector(x)) 1 else 1 / ncol(x),
          coef0       = 0,
          cost        = 1,
          nu          = 0.5,
          class.weights = NULL,
          case.weights = rep(1, nrow(x)),
          cachesize   = 40,
          tolerance   = 0.001,
          epsilon     = 0.1,
          shrinking   = TRUE,
          cross       = 0,
          probability = FALSE,
          fitted      = TRUE,
          seed        = 1L,
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
    formula <- !is.null(attr(x, "assign"))
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
    
    if (length(case.weights) != nrow(x))
    	stop("'nrow(x)' and 'length(case.weights)' are different")
    ## scaling, subsetting, and NA handling
    if (sparse) {
        scale <- rep(FALSE, ncol(x))
        if (!is.null(y)) na.fail(y)
        na.fail(case.weights) #?
        x <- SparseM::t(SparseM::t(x)) ## make sure that col-indices are sorted
    } else {
    	x <- as.matrix(x, rownames.force = TRUE)
		#x <- structure(data.matrix(x, rownames.force = TRUE), class = "matrix")

        ## subsetting and na-handling for matrices
        if (!formula) {
            if (!is.null(subset)) {
            	case.weights <- case.weights[subset]  
            	x <- x[subset, , drop = FALSE]
            	if (!is.null(y)) y <- y[subset]
            }
            if (is.null(y)) {
            	#df <- na.action(data.frame(case.weights, x))
            	df <- na.action(structure(list(case.weights = case.weights, x = x), class = "data.frame", row.names = rownames(x)))
            	case.weights <- df$case.weights #[,1]
            	x <- df$x #as.matrix(df[,-1], rownames.force = TRUE)
                nac <-
                    attr(x, "na.action") <-
                    	attr(case.weights, "na.action") <-
							attr(df, "na.action")
#print(nac)
#print(attributes(df))
            } else {
                #df <- na.action(data.frame(y, case.weights, x)) 
                df <- na.action(structure(list(y = y, case.weights = case.weights, x = x), class = "data.frame", row.names = rownames(x)))
                y <- df$y#[,1]
                case.weights <- df$case.weights#[,2]
                x <- df$x #as.matrix(df[,-c(1:2)], rownames.force = TRUE)
                nac <-
                    attr(x, "na.action") <-
                        attr(y, "na.action") <-
                        	attr(case.weights, "na.action") <-
                            	attr(df, "na.action")
#print(nac)
#print(attributes(df))                           	
            }
        }
        xhold   <- if (fitted) x else NA

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
    if (cross > nr)
        stop(sQuote("cross"), " cannot exceed the number of observations!")

    if (!is.vector(y) && !is.factor (y) && type != 2)
        stop("y must be a vector or a factor.")
    if (type != 2 && length(y) != nr)
        stop("x and y don't match.")

    if (cachesize < 0.1)
        cachesize <- 0.1

    if (type > 2 && !is.numeric(y))
        stop("Need numeric dependent variable for regression.")

    lev <- NULL
    weightlabels <- NULL
    
    if (any(case.weights < 0))
        stop("'case.weights' have to be larger or equal to zero")
    if (all(case.weights == 0))
        stop("all 'case.weights' are zero")

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
			if (length(lev1) < 2L)
				stop("need training data from at least two classes")
			# if (sum(table(y[case.weights != 0]) > 0) <= 1)
				# stop("need training data with positive 'case.weights' from at least two classes")
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
            if (type < 3) { ### ?
                if(any(as.integer(y) != y))
                    stop("dependent variable has to be of factor or integer type for classification mode.")
                y <- as.factor(y)
                lev <- levels(y)
###
    			if (length(lev) < 2L)
    				stop("need training data from at least two classes")
###            
                y <- as.integer(y)
            } else
            	lev <- unique(y)
        }

    nclass <- 2
    if (type < 2)
    	nclass <- length(lev)

    if (type > 1 && length(class.weights) > 0) {
        class.weights <- NULL
        warning(sQuote("class.weights"), " are set to NULL for regression mode. For classification, use a _factor_ for ", sQuote("y"),
", or specify the correct ", sQuote("type"), " argument.")
    }

    err <- empty_string <- paste(rep(" ", 255), collapse = "")
    cret <- .C ("svmtrain",
                ## data
                as.double  (if (sparse) x@ra else t(x)),
                as.integer (nr), as.integer(ncol(x)),
                as.double  (y),
                ## sparse index info
                as.integer (if (sparse) x@ia else 0),
                as.integer (if (sparse) x@ja else 0),

                ## parameters
                as.integer (type),
                as.integer (kernel),
                as.integer (degree),
                as.double  (gamma),
                as.double  (coef0),
                as.double  (cost),
                as.double  (nu),
                as.integer (weightlabels),
                as.double  (class.weights),
                as.integer (length (class.weights)),
         		as.double  (case.weights),
                as.double  (cachesize),
                as.double  (tolerance),
                as.double  (epsilon),
                as.integer (shrinking),
                as.integer (cross),
                as.integer (sparse),
                as.integer (probability),
                as.integer (seed),

                ## results
                nclasses = integer  (1),
                nr       = integer  (1), # nr of support vectors
                index    = integer  (nr),
                labels   = integer  (nclass),
                nSV      = integer  (nclass),
                rho      = double   (nclass * (nclass - 1) / 2),
                obj      = double   (nclass * (nclass - 1) / 2),
                coefs    = double   (nr * (nclass - 1)),
                sigma    = double   (1),
                probA    = double   (nclass * (nclass - 1) / 2),
                probB    = double   (nclass * (nclass - 1) / 2),

                cresults = double   (cross),
                ctotal1  = double   (1),
                ctotal2  = double   (1),
                error    = err,

                PACKAGE = "locClass")

    if (cret$error != empty_string)
        stop(paste(cret$error, "!", sep=""))
#print(cret)

    ret <- list (
                 call     = match.call(),
                 type     = type,
                 kernel   = kernel,
                 cost     = cost,
                 degree   = degree,
                 gamma    = gamma,
                 coef0    = coef0,
                 nu       = nu,
                 epsilon  = epsilon,
                 sparse   = sparse,
                 scaled   = scale,
                 x.scale  = x.scale,
                 y.scale  = y.scale,

                 nclasses = cret$nclasses, #number of classes
                 levels   = lev,
                 tot.nSV  = cret$nr, #total number of sv
                 nSV      = cret$nSV[1:cret$nclasses], #number of SV in diff. classes
                 labels   = cret$label[1:cret$nclasses], #labels of the SVs.
                 SV       = if (sparse) SparseM::t(SparseM::t(x[cret$index[1:cret$nr],]))
                 else t(t(x[cret$index[1:cret$nr],])), #copy of SV
                 index    = cret$index[1:cret$nr],  #indexes of sv in x
                 ##constants in decision functions
                 rho      = cret$rho[1:(cret$nclasses * (cret$nclasses - 1) / 2)],
                 obj      = cret$obj[1:(cret$nclasses * (cret$nclasses - 1) / 2)],
                 ##probabilites
                 compprob = probability,
                 probA    = if (!probability) NULL else
                 cret$probA[1:(cret$nclasses * (cret$nclasses - 1) / 2)],
                 probB    = if (!probability) NULL else
                 cret$probB[1:(cret$nclasses * (cret$nclasses - 1) / 2)],
                 sigma    = if (probability) cret$sigma else NULL,
                 ##coefficiants of sv
                 coefs    = if (cret$nr == 0) NULL else
                 t(matrix(cret$coefs[1:((cret$nclasses - 1) * cret$nr)],
                          nrow = cret$nclasses - 1,
                          byrow = TRUE)),
                 na.action = nac
                 )

	if (type < 2) {
		if (ret$nclasses < length(ret$levels)) 
			warning("some groups are empty")
	}
	
    ## cross-validation-results
    if (cross > 0)
        if (type > 2) {
            scale.factor     <- if (any(scale)) crossprod(y.scale$"scaled:scale") else 1;
            ret$MSE          <- cret$cresults * scale.factor;
            ret$tot.MSE      <- cret$ctotal1  * scale.factor;
            ret$scorrcoeff   <- cret$ctotal2;
        } else {
            ret$accuracies   <- cret$cresults;
            ret$tot.accuracy <- cret$ctotal1;
        }
        

    class (ret) <- c("wsvm", "svm")

    if (fitted) {
        ret$fitted <- na.action(predict(ret, xhold,
                                        decision.values = TRUE))
        ret$decision.values <- attr(ret$fitted, "decision.values")
        attr(ret$fitted, "decision.values") <- NULL
        if (type > 1) ret$residuals <- y - ret$fitted
    }

    ret$case.weights <- case.weights
    names(ret$case.weights) <- rownames(x)
   
    ret
}



#' This function predicts values based upon a model trained by \code{\link{wsvm}}.
#'
#' @title Predict New Examples by a Trained (Weighted) Support Vector Machine
#'
#' @param object Object of class \code{"wsvm"}, created by \code{wsvm}.
#' @param newdata An object containing the new input data: either a
#'  matrix or a sparse matrix (object of class
#'    \code{\link[Matrix]{Matrix}} provided by the \pkg{Matrix} package,
#'    or of class \code{\link[SparseM]{matrix.csr}}
#'    provided by the \pkg{SparseM} package, or of class
#'    \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam}
#'    package). A vector will
#'    be transformed to a n x 1 matrix.
#'  @param decision.values Logical controlling whether the decision values
#'    of all binary classifiers computed in multiclass classification
#'    shall be computed and returned.
#'  @param probability Logical indicating whether class probabilities
#'    should be computed and returned. Only possible if the model was
#'    fitted with the \code{probability} option enabled.
#'  @param na.action A function to specify the action to be taken if \sQuote{NA}s are
#'    found. The default action is \code{na.omit}, which leads to rejection of cases
#'    with missing values on any required variable. An alternative
#'    is \code{na.fail}, which causes an error if \code{NA} cases
#'    are found. (NOTE: If given, this argument must be named.)
#' @param \dots Currently not used.
#'
#' @return A vector of predicted values (for classification: a vector of labels, for density
#'  estimation: a logical vector). If \code{decision.value} is
#'  \code{TRUE}, the vector gets a \code{"decision.values"} attribute
#'  containing a \eqn{n * c} matrix (\eqn{n} number of predicted values, \eqn{c} number of
#'  classifiers) of all \eqn{c} binary classifiers' decision values. There are \eqn{k * (k - 1) / 2} 
#'  classifiers (\eqn{k} number of classes). The colnames of
#'  the matrix indicate the labels of the two classes. If \code{probability} is
#'  \code{TRUE}, the vector gets a \code{"probabilities"} attribute
#'  containing a \eqn{n * k} matrix of the class probabilities.
#'
#' @note If the training set was scaled by \code{wsvm} (done by default), the
#'  new data is scaled accordingly using scale and center of
#'  the training data.
#'
#  @author David Meyer (based on C++-code by Chih-Chung Chang and Chih-Jen Lin)
#'
#' @keywords neural nonlinear classif
#'
#' @seealso \code{\link{wsvm}}, \code{\link[e1071]{predict.svm}}, \code{\link[e1071]{svm}}.
#'
#' @family svm
#'
#' @rdname predict.wsvm
#'
#' @export

# examples
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

predict.wsvm <-
function (object, newdata,
          decision.values = FALSE,
          probability = FALSE,
          ...,
          na.action = na.omit)
{
    if (missing(newdata))
        return(fitted(object))

    # if (object$tot.nSV < 1)
        # stop("Model is empty!")


    if(inherits(newdata, "Matrix")) {
        library("SparseM")
        library("Matrix")
        newdata <- as(newdata, "matrix.csr")
    }
    if(inherits(newdata, "simple_triplet_matrix")) {
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
        if (inherits(object, "wsvm.formula")) {
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

    if (ncol(object$SV) != ncol(newdata))
        stop ("test data does not match model !")

    ret <- .C ("svmpredict",
               as.integer (decision.values),
               as.integer (probability),

               ## model
               as.double  (if (object$sparse) object$SV@ra else t(object$SV)),
               as.integer (nrow(object$SV)), as.integer(ncol(object$SV)),
               as.integer (if (object$sparse) object$SV@ia else 0),
               as.integer (if (object$sparse) object$SV@ja else 0),
               as.double  (as.vector(object$coefs)),
               as.double  (object$rho),
               as.double  (object$obj),
               as.integer (object$compprob),
               as.double  (object$probA),
               as.double  (object$probB),
               as.integer (object$nclasses),
               as.integer (object$tot.nSV),
               as.integer (object$labels),
               as.integer (object$nSV),
               as.integer (object$sparse),

               ## parameter
               as.integer (object$type),
               as.integer (object$kernel),
               as.integer (object$degree),
               as.double  (object$gamma),
               as.double  (object$coef0),

               ## test matrix
               as.double  (if (sparse) newdata@ra else t(newdata)),
               as.integer (nrow(newdata)),
               as.integer (if (sparse) newdata@ia else 0),
               as.integer (if (sparse) newdata@ja else 0),
               as.integer (sparse),

               ## decision-values
               ret = double(nrow(newdata)),
               dec = double(nrow(newdata) * object$nclasses * (object$nclasses - 1) / 2),
               prob = double(nrow(newdata) * object$nclasses)#,

               #PACKAGE = "e1071"
               )

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
	    if (length(ret$dec) == 0) {
	        attr(ret2, "decision.values") <- NA
    	} else {
	        colns = c()
    	    for (i in 1:(object$nclasses - 1))
        	    for (j in (i + 1):object$nclasses)
            	    colns <- c(colns,
                	           paste(object$levels[object$labels[i]],
                    	             "/", object$levels[object$labels[j]],
                        	         sep = ""))
	        attr(ret2, "decision.values") <-
    	        napredict(act,
        	              matrix(ret$dec, nrow = nrow(newdata), byrow = TRUE,
            	                 dimnames = list(rowns, colns)
                	             )
                    	  )
		}
    }

    if (probability && object$type < 2)
        attr(ret2, "probabilities") <-
            napredict(act,
                      matrix(ret$prob, nrow = nrow(newdata), byrow = TRUE,
                             dimnames = list(rowns, object$levels[object$labels])
                             )
                      )

    ret2
}



#' @noRd
#'
#' @importFrom stats weights
#' @export

weights.wsvm <- function (object, ...) {
	if (!inherits(object, "wsvm"))
		stop("object not of class \"wsvm\"")
	object$case.weights
}



# @param x An object of class \code{"wsvm"}.
# @param \dots Further arguments.
# 
# @rdname wsvm
#
# @export

# print.wsvm <-
# function (x, ...) {
	# if (!inherits())
		# stop("object not of class \"wsvm\"")
	# NextMethod(x, ...)
	# # if (!is.null(x$call$case.weights)) {
		# # cat("Case weights:")
		# # dput(x$call$case.weights)
		# # cat("\n")
	# # }
# }

# {
    # cat("\nCall:", deparse(x$call, 0.8 * getOption("width")), "\n", sep="\n")
    # cat("Parameters:\n")
    # cat("   SVM-Type: ", c("C-classification",
                           # "nu-classification",
                           # "one-classification",
                           # "eps-regression",
                           # "nu-regression")[x$type+1], "\n")
    # cat(" SVM-Kernel: ", c("linear",
                           # "polynomial",
                           # "radial",
                           # "sigmoid")[x$kernel+1], "\n")
    # if (x$type==0 || x$type==3 || x$type==4)
        # cat("       cost: ", x$cost, "\n")
    # if (x$kernel==1)
        # cat("     degree: ", x$degree, "\n")
    # cat("      gamma: ", x$gamma, "\n")
    # if (x$kernel==1 || x$kernel==3)
        # cat("     coef.0: ", x$coef0, "\n")
    # if (x$type==1 || x$type==2 || x$type==4)
        # cat("         nu: ", x$nu, "\n")
    # if (x$type==3) {
        # cat("    epsilon: ", x$epsilon, "\n\n")
        # if (x$compprob)
            # cat("Sigma: ", x$sigma, "\n\n")
    # }

    # cat("\nNumber of Support Vectors: ", x$tot.nSV)
    # cat("\n\n")

# }



# @rdname wsvm
# @export

# summary.wsvm <-
# function(object, ...)
    # structure(object, class=c("summary.wsvm", "summary.svm"))



# @rdname wsvm
# @export

# print.summary.wsvm <-
# function (x, ...)
# {
	# NextMethod(x, ...)

    # # print.wsvm(x)
    # # if (x$type<2) {
        # # cat(" (", x$nSV, ")\n\n")
        # # cat("\nNumber of Classes: ", x$nclasses, "\n\n")
        # # cat("Levels:", if(is.numeric(x$levels)) "(as integer)", "\n", x$levels)
    # # }
    # # cat("\n\n")
    # # if (x$type==2) cat("\nNumber of Classes: 1\n\n\n")

    # # if ("MSE" %in% names(x)) {
        # # cat(length (x$MSE), "-fold cross-validation on training data:\n\n", sep="")
        # # cat("Total Mean Squared Error:", x$tot.MSE, "\n")
        # # cat("Squared Correlation Coefficient:", x$scorrcoef, "\n")
        # # cat("Mean Squared Errors:\n", x$MSE, "\n\n")
    # # }
    # # if ("accuracies" %in% names(x)) {
        # # cat(length (x$accuracies), "-fold cross-validation on training data:\n\n", sep="")
        # # cat("Total Accuracy:", x$tot.accuracy, "\n")
        # # cat("Single Accuracies:\n", x$accuracies, "\n\n")
    # # }
    # # cat("\n\n")
# }




scale.data.frame <- e1071:::scale.data.frame
# scale.data.frame <-
# function(x, center = TRUE, scale = TRUE)
# {
    # i <- sapply(x, is.numeric)
    # if (ncol(x[, i, drop = FALSE])) {
        # x[, i] <- tmp <- scale.default(x[, i, drop = FALSE], na.omit(center), na.omit(scale))
        # if(center || !is.logical(center))
            # attr(x, "scaled:center")[i] <- attr(tmp, "scaled:center")
        # if(scale || !is.logical(scale))
            # attr(x, "scaled:scale")[i]  <- attr(tmp, "scaled:scale")
    # }
    # x
# }



#  Generates a scatter plot of the input data of a \code{wsvm} fit for
#  classification models by highlighting the classes and support
#  vectors. Optionally, draws a filled contour plot of the class regions.
#
# @title Plot SVM Objects
#
#  @param x An object of class \code{wsvm}.
#  @param data data to visualize. Should be the same used for fitting.
#  @param formula formula selecting the visualized two dimensions. Only
#    needed if more than two input variables are used.
#  @param fill Switch indicating whether a contour plot for the class
#    regions should be added.
#  @param grid Granularity for the contour plot.
#  @param slice A list of named values for the dimensions held
#    constant (only needed if more than two variables are
#    used). The defaults for unspecified dimensions are 0 (for numeric
#    variables) and the first level (for factors). Factor levels can
#    either be specified as factors or character vectors of length 1.
# @param symbolPalette Color palette used for the class the data points and support
#    vectors belong to.
#  @param svSymbol Symbol used for support vectors.
#  @param dataSymbol Symbol used for data points (other than support vectors).
#  @param \dots additional graphics parameters passed to
#    \code{filled.contour} and \code{plot}.
#
# @author David Meyer\cr
#  \email{David.Meyer@@R-project.org}
#
# @seealso \code{\link{wsvm}}.
#
#
# @keywords neural classif nonlinear
#
# @rdname plot.wsvm
#
# @export

#\examples{
### a simple example
#data(cats, package = "MASS")
#m <- wsvm(Sex~., data = cats)
#plot(m, cats)
#
### more than two variables: fix 2 dimensions
#data(iris)
#m2 <- wsvm(Species~., data = iris)
#plot(m2, iris, Petal.Width ~ Petal.Length,
#     slice = list(Sepal.Width = 3, Sepal.Length = 4))
#
### plot with custom symbols and colors
#plot(m, cats, svSymbol = 1, dataSymbol = 2, symbolPalette = rainbow(4),
#color.palette = terrain.colors)

# plot.wsvm <-
# function(x, data, formula = NULL, fill = TRUE,
         # grid = 50, slice = list(), symbolPalette = palette(),
         # svSymbol = "x", dataSymbol = "o", ...)
# {
    # if (x$type < 3) {
        # if (is.null(formula) && ncol(data) == 3) {
            # formula <- formula(delete.response(terms(x)))
            # formula[2:3] <- formula[[2]][2:3]
        # }
        # if (is.null(formula))
            # stop("missing formula.")
        # if (fill) {
            # sub <- model.frame(formula, data)
            # xr <- seq(min(sub[, 2]), max(sub[, 2]), length = grid)
            # yr <- seq(min(sub[, 1]), max(sub[, 1]), length = grid)
            # l <- length(slice)
            # if (l < ncol(data) - 3) {
                # slnames <- names(slice)
                # slice <- c(slice, rep(list(0), ncol(data) - 3 -
                                      # l))
                # names <- labels(delete.response(terms(x)))
                # names(slice) <- c(slnames, names[!names %in%
                                                 # c(colnames(sub), slnames)])
            # }
            # for (i in names(which(sapply(data, is.factor))))
                # if (!is.factor(slice[[i]])) {
                    # levs <- levels(data[[i]])
                    # lev <- if (is.character(slice[[i]])) slice[[i]] else levs[1]
                    # fac <- factor(lev, levels = levs)
                    # if (is.na(fac))
                        # stop(paste("Level", dQuote(lev), "could not be found in factor", sQuote(i)))
                    # slice[[i]] <- fac
                # }

            # lis <- c(list(yr), list(xr), slice)
            # names(lis)[1:2] <- colnames(sub)
            # new <- expand.grid(lis)[, labels(terms(x))]
            # preds <- predict(x, new)
            # filled.contour(xr, yr,
                           # matrix(as.numeric(preds),
                                  # nrow = length(xr), byrow = TRUE),
                           # plot.axes = {
                               # axis(1)
                               # axis(2)
                               # colind <- as.numeric(model.response(model.frame(x, data)))
                               # dat1 <- data[-x$index,]
                               # dat2 <- data[x$index,]
                               # coltmp1 <- symbolPalette[colind[-x$index]]
                               # coltmp2 <- symbolPalette[colind[x$index]]
                               # points(formula, data = dat1, pch = dataSymbol, col = coltmp1)
                               # points(formula, data = dat2, pch = svSymbol, col = coltmp2)
                           # },
                           # levels = 1:(length(levels(preds)) + 1),
                           # key.axes = axis(4, 1:(length(levels(preds))) + 0.5,
                           # labels = levels(preds),
                           # las = 3),
                           # plot.title = title(main = "SVM classification plot",
                           # xlab = names(lis)[2], ylab = names(lis)[1]),
                           # ...)
        # }
        # else {
            # plot(formula, data = data, type = "n", ...)
            # colind <- as.numeric(model.response(model.frame(x,
                                                            # data)))
            # dat1 <- data[-x$index,]
            # dat2 <- data[x$index,]
            # coltmp1 <- symbolPalette[colind[-x$index]]
            # coltmp2 <- symbolPalette[colind[x$index]]
            # points(formula, data = dat1, pch = dataSymbol, col = coltmp1)
            # points(formula, data = dat2, pch = svSymbol, col = coltmp2)
            # invisible()
        # }
    # }
# }



#  This function exports an SVM object (trained by \code{wsvm}) to two
#  specified files. One is in the format that the
#  function 'svm\_load\_model' of libsvm can read. The other is for
#  scaling data, containing a data with centers and scales for all variables.
#
#  This function is useful when SVM models trained in R shall be used in 
#  other environments. The SVM model is saved in the standard format of
#  libsvm. The scaling data are written to a separate file because scaling
#  data are not included in the standard format of libsvm. The format
#  of the scaling data file is a n times 2 matrix: the n-th row
#  corresponds to the n-th dimension of the data, the columns being formed
#  of the corresponding mean and scale. If scaling information for the
#  dependent variable exists (in case of regression models), it is stored
#  in yet another file (1 times 2 matrix).
#
# @title Write SVM Object to File
#
#  @param object Object of class \code{"svm"}, created by \code{svm}.
#  @param svm.file filename to export the svm object to.
#  @param scale.file filename to export the scaling data of the
#    explanatory variables to.
#  @param yscale.file filename to export the scaling data of the dependent
#    variable to, if any.
#
# @author
#  Tomomi TAKASHINA (based on 'predict.svm' by David Meyer)
#  \email{t.takashina@@computer.org}
#
# @seealso \code{\link{wsvm}}, \code{\link[e1071]{svm}}.
#
#
# @keywords neural nonlinear classif

# examples
#data(iris)
#attach(iris)
#
### classification mode
## default with factor response:
#model <- svm (Species~., data=iris)
#
## export SVM object to file
#write.svm(model, svm.file = "iris-classifier.svm", scale.file = "iris-classifier.scale")
#
## read scale file
## the n-th row is corresponding to n-th dimension. The 1st column contains the
## center value, the 2nd column is the scale value.
#
#read.table("iris-classifier.scale")

# write.wsvm <-
# function (object, wsvm.file="Rdata.wsvm", scale.file = "Rdata.scale",
          # yscale.file = "Rdata.yscale")
# {

    # ret <- .C ("svmwrite",
               # ## model
               # as.double  (if (object$sparse) object$SV@ra else t(object$SV)),
               # as.integer (nrow(object$SV)), as.integer(ncol(object$SV)),
               # as.integer (if (object$sparse) object$SV@ia else 0),
               # as.integer (if (object$sparse) object$SV@ja else 0),
               # as.double  (as.vector(object$coefs)),
               # as.double  (object$rho),
               # as.double  (object$probA),
               # as.double  (object$probB),
               # as.integer (object$nclasses),
               # as.integer (object$tot.nSV),
               # as.integer (object$labels),
               # as.integer (object$nSV),
               # as.integer (object$sparse),

               # ## parameter
               # as.integer (object$type),
               # as.integer (object$kernel),
               # as.integer (object$degree),
               # as.double  (object$gamma),
               # as.double  (object$coef0),

               # ## filename
               # as.character(wsvm.file),

               # PACKAGE = "e1071"
               # )$ret

    # write.table(data.frame(center = object$x.scale$"scaled:center",
                           # scale  = object$x.scale$"scaled:scale"),
                # file=scale.file, col.names=FALSE, row.names=FALSE)

    # if (!is.null(object$y.scale))
        # write.table(data.frame(center = object$y.scale$"scaled:center",
                               # scale  = object$y.scale$"scaled:scale"),
                    # file=yscale.file, col.names=FALSE, row.names=FALSE)
# }
