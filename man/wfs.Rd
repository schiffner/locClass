% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generatewf.R
\name{biweight}
\alias{biweight}
\alias{cauchy}
\alias{cosine}
\alias{epanechnikov}
\alias{exponential}
\alias{gaussian}
\alias{optcosine}
\alias{rectangular}
\alias{triangular}
\alias{wfs}
\title{Generation of Window Functions}
\usage{
biweight(bw, k)

cauchy(bw, k, nn.only = TRUE)

cosine(bw, k)

epanechnikov(bw, k)

exponential(bw, k, nn.only = TRUE)

gaussian(bw, k, nn.only = TRUE)

optcosine(bw, k)

rectangular(bw, k)

triangular(bw, k)
}
\arguments{
\item{bw}{The bandwidth parameter.}

\item{k}{The number of nearest neighbors.}

\item{nn.only}{(Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.}
}
\value{
Returns an object of class \code{"function"}. The resulting \code{function} implements the desired window function and depends on one
argument \code{x} that is usually some sort of distance (and assumed to be positive).
The returned function has several attributes, depending on which arguments are specified.
\describe{
  \item{\code{"name"}}{The name of the window function.}
  \item{\code{"bw"}}{(If the corresponding argument is given.) The chosen bandwidth.}
	 \item{\code{"k"}}{(If the corresponding argument is given.) The chosen number of nearest neighbors.}
  \item{\code{"nn.only"}}{(Logical. Only if \code{k} was specified.) \code{TRUE} if only the k nearest neighbors are used. 
    (\code{nn.only} is always \code{TRUE} except for window functions with infinite support.)}
  \item{\code{"adaptive"}}{(Logical.) \code{TRUE} in case of an adaptive bandwidth, \code{FALSE} if the bandwidth is fixed.}
}
}
\description{
The window functions generating functions that are used in various local classification methods.
}
\details{
These functions are used to initialize window functions that can be passed as
\code{wf} argument to various local classification methods.

If only \code{bw} is given a window function with fixed bandwidth is returned.

If only \code{k} is given a window function with \code{k} nearest neighbors bandwidth, i.e. adaptive to the local density of data points, is generated.
In case of window functions with infinite support, \code{"cauchy"}, \code{"exponential"} and \code{"gaussian"}, the argument \code{nn.only} is used to decide 
if only the \code{k} nearest neighbors or all observations receive positive weights.

If \code{bw} and \code{k} are both specified, a window function with fixed bandwidth is generated and all weights are set to zero except for
the \code{k} nearest neighbors.

Parts of the source code are based on the function \link[stats]{density} in package \pkg{stats}. Concerning the \code{"cosine"} and \code{"optcosine"} windows, it applies
the same as for \link[stats]{density}: \code{"cosine"} is smoother than \code{"optcosine"}, which is the usual 'cosine' kernel in the literature.
\code{"cosine"} is the version used by the S programming language.
}
\examples{
x <- seq(0,1,0.01)

## fixed bandwidth
gwf <- gaussian(bw = 1)
gwf
curve(gwf(x))

## adaptive bandwidth, only the 50 nearest neighbors receive positive weights
gwf <- gaussian(k = 50)
gwf
curve(gwf(x))

## adaptive bandwidth, all observations have positive weights
gwf <- gaussian(k = 50, nn.only = FALSE)
gwf
curve(gwf(x))

## fixed bandwidth, only the 100 nearest neighbors get positive weights
gwf <- gaussian(k = 50, bw = 1)
gwf
curve(gwf(x))

}
\seealso{
Documentation of various local classification methods, e.g. \code{\link{dalda}} or \code{\link{oslda}}, and \link[stats]{density}.
}

