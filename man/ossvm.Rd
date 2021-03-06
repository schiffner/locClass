% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ossvm.R
\name{ossvm}
\alias{ossvm}
\alias{ossvm.default}
\alias{ossvm.formula}
\title{Observation Specific Support Vector Machines}
\usage{
ossvm(x, ...)

\method{ossvm}{formula}(formula, data = NULL, ..., subset,
  na.action = na.omit, scale = TRUE)

\method{ossvm}{default}(x, y = NULL, scale = TRUE, type = NULL,
  kernel = "radial", degree = 3, gamma = if (is.vector(x)) 1 else
  1/ncol(x), coef0 = 0, cost = 1, nu = 0.5, wf = c("biweight", "cauchy",
  "cosine", "epanechnikov", "exponential", "gaussian", "optcosine",
  "rectangular", "triangular"), bw, k, nn.only = TRUE, class.weights = NULL,
  cachesize = 40, tolerance = 0.001, epsilon = 0.1, shrinking = TRUE,
  seed = 1L, ..., subset = NULL, na.action = na.omit)
}
\arguments{
\item{x}{(Required if no \code{formula} is given as principal argument.) A data matrix, a 
vector, or a sparse matrix (object of class \code{\link[Matrix]{Matrix}} provided by the 
\pkg{Matrix} package, or of class \code{\link[SparseM]{matrix.csr}} provided by the \pkg{SparseM} 
package, or of class \code{\link[slam]{simple_triplet_matrix}} provided by the \pkg{slam} package).}

\item{formula}{A symbolic description of the model to be fit.}

\item{data}{An optional data frame containing the variables in the model. By default
the variables are taken from the environment which \code{ossvm} is called from.}

\item{subset}{An index vector specifying the cases to be used in the training sample. 
(NOTE: If given, this argument must be named.)}

\item{na.action}{A function to specify the action to be taken if \code{NA}s are found. 
The default action is \code{na.omit}, which 
leads to rejection of cases with missing values on any required variable. An alternative 
is \code{na.fail}, which causes an 
error if \code{NA} cases are found. (NOTE: If given, this argument must be named.)}

\item{scale}{A logical vector indicating the variables to be scaled. If \code{scale} is of 
length 1, the value is recycled as many times as needed. Per default, 
data are scaled internally (both \code{x} and \code{y} variables) to zero mean and unit variance. 
The center and scale values are returned and used for later predictions.}

\item{y}{(Only if no \code{formula} is given as principal argument.) A response vector with 
one label for each row/component of \code{x}. Can be either a factor (for classification tasks) or 
a numeric vector (for regression).}

\item{type}{\code{ossvm} can be used as a classification machine, as a regression machine, or 
for novelty detection. Depending of whether \code{y} is a factor or not, 
the default setting for \code{type} is \code{C-classification} or \code{eps-regression}, 
respectively, but may be overwritten by setting an explicit value.
\cr
Valid options are:
\itemize{
\item \code{C-classification}
\item \code{nu-classification}
\item \code{one-classification} (for novelty detection)
\item \code{eps-regression}
\item \code{nu-regression}
}}

\item{kernel}{The kernel used in training and predicting. You might consider changing some of 
the following parameters, depending on the kernel type.
\describe{
  \item{linear:}{\eqn{u'v}{u'*v}}
  \item{polynomial:}{\eqn{(\gamma u'v + coef0)^{degree}}{(gamma*u'*v + coef0)^degree}}
  \item{radial basis:}{\eqn{\exp(-\gamma |u-v|^2)}{exp(-gamma*|u-v|^2)}}
  \item{sigmoid:}{\eqn{\tanh(\gamma u'v + coef0)}{tanh(gamma*u'*v + coef0)}}
  }}

\item{degree}{Parameter needed for kernel of type \code{polynomial} (default: 3).}

\item{gamma}{Parameter needed for all kernels except \code{linear} (default: 1/(data dimension)).}

\item{coef0}{Parameter needed for kernels of type \code{polynomial} and \code{sigmoid} (default: 0).}

\item{cost}{Cost of constraints violation (default: 1)--it is the \sQuote{C}-constant of the 
regularization term in the Lagrange formulation.}

\item{nu}{Parameter needed for \code{nu-classification}, \code{nu-regression}, and \code{one-classification}.}

\item{wf}{A window function which is used to calculate weights that are introduced into 
the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
For details see the documentation for \code{\link[=biweight]{wfs}}.}

\item{bw}{(Required only if \code{wf} is a string.) The bandwidth parameter of the 
window function. (See \code{\link[=biweight]{wfs}}.)}

\item{k}{(Required only if \code{wf} is a string.) The number of nearest neighbors of the decision 
boundary to be used in the fitting process. 
(See \code{\link[=biweight]{wfs}}.)}

\item{nn.only}{(Required only if \code{wf} is a string indicating a window function with 
infinite support and if \code{k} is specified.) Should
only the \code{k} nearest neighbors or all observations receive positive weights? 
(See \code{\link[=biweight]{wfs}}.)}

\item{class.weights}{A named vector of weights for the different classes, used for asymmetric class sizes. 
Not all factor levels have to be supplied (default weight: 1). 
All components have to be named.}

\item{cachesize}{Cache memory in MB (default 40).}

\item{tolerance}{Tolerance of termination criterion (default: 0.001).}

\item{epsilon}{epsilon in the insensitive-loss function (default: 0.1).}

\item{shrinking}{Option whether to use the shrinking-heuristics (default: \code{TRUE}).}

\item{seed}{Integer seed for libsvm (used for probability prediction models).}

\item{\dots}{Additional parameters for the low level fitting function \code{ossvm.default}.}
}
\value{
An object of class \code{"ossvm"}, a \code{list} containing all information about 
  the SVM model to be fitted, mainly for internal use. It includes:
  \item{call}{The (matched) function call.}
  \item{x}{The explanatory variables (already scaled if scaling was desired).}
  \item{y}{The response vector (appropriately modified).}
  \item{wf}{The window function used.}
  \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions 
     documented in \code{\link[=biweight]{wfs}}.) 
  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
	 \item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions 
 	documented in \code{\link[=biweight]{wfs}}.) 
  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
  \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of 
 	the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
 specified.) \code{TRUE} if only the \code{k} nearest neighbors receive a positive 
  weight, \code{FALSE} otherwise.}
  \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the 
 	local density of data points, \code{FALSE} if the bandwidth
  is fixed.}
  \item{variant}{(Only if \code{wf} is a string or one of the window functions documented in 
 	\code{\link[=biweight]{wfs}} is used, for internal use only). 
  An integer indicating which weighting scheme is implied by \code{bw}, \code{k} and \code{nn.only}.}
Moreover, information about the chosen SVM \code{type}, the \code{kernel} function and the SVM parameters is 
 included.
}
\description{
A localized version of Support Vector Machines.
}
\details{
This is a localized version of the Support Vector Machine where for each test observation
an individual SVM is fitted. Training observations are weighted according to their Euclidean
distance from the test observation.

The name of the window function (\code{wf}) can be specified as a character string.
In this case the window function is generated internally in \code{predict.ossvm}. Currently
supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
\code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
\code{"triangular"}.

Moreover, it is possible to generate the window functions mentioned above in advance 
(see \code{\link[=biweight]{wfs}}) and pass them to \code{ossvm}.

Any other function implementing a window function can also be used as \code{wf} argument.
This allows the user to try own window functions.
See help on \code{\link[=biweight]{wfs}} for details.

For multiclass-classification with \eqn{k} levels, \eqn{k>2}, \sQuote{libsvm} uses the 'one-against-one'-approach, 
in which \eqn{k(k-1)/2} binary classifiers are trained; the appropriate class is found by a voting 
scheme.

\sQuote{libsvm} internally uses a sparse data representation, which is also high-level supported by 
the package \pkg{SparseM}.

If the predictor variables include factors, the formula interface must be used to get a 
correct model matrix.

The probability model for classification fits a logistic distribution using maximum likelihood 
to the decision values of all binary classifiers, and computes the a-posteriori class 
probabilities for the multi-class problem using quadratic optimization. The probabilistic 
regression model assumes (zero-mean) laplace-distributed errors for the predictions, and 
estimates the scale parameter using maximum likelihood.
}
\examples{
fit <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 0.5, 
    kernel = "polynomial", coef0 = 0.5)
pred <- predict(fit)
mean(pred != iris$Species)

}
\seealso{
\code{\link{predict.ossvm}}.

Other observation_specific svm: \code{\link{predict.ossvm}}

Other observation_specific svm: \code{\link{predict.ossvm}}

Other observation_specific svm: \code{\link{predict.ossvm}}
}
\keyword{classif}
\keyword{regression}

