# Copyright (C) 2011-2012 Julia Schiffner
# Copyright (C) 2004-2011 Friedrich Leisch and Bettina Gruen
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

#' Runs \code{\link[flexmix]{flexmix}} repeatedly for different cluster values and returns the result with highest likelihood value.
#'
#' @title Run \code{\link[flexmix]{flexmix}} Repeatedly
#' 
#' @param \dots Passed to \code{\link[flexmix]{flexmix}}.
#' @param cluster Matrix where columns are integer vectors with the initial cluster assignments of observations at the start of the EM algorithm.
#' @param verbose If \code{TRUE}, show progress information during computations.
#' @param drop If \code{TRUE} and k is of length 1, then a single flexmix object is returned instead of a "stepFlexmix" object.
#' @param unique If \code{TRUE}, then \code{unique()} is called on the result, see below.
#'
#' @return
#' An object of class \code{"stepFlexmix"} containing the best models with respect to the log likelihood for the different number of components in a slot if \code{length(k)>1}, else directly an object of class \code{"flexmix"}.
#' If \code{unique=FALSE}, then the resulting object contains one model per element of \code{k} (which is the number of clusters the EM algorithm started with). 
#' If \code{unique=TRUE}, then the result is resorted according to the number of clusters contained in the fitted models (which may be less than the number with which 
#' the EM algorithm started), and only the maximum likelihood solution for each number of fitted clusters is kept. This operation can also be done manually by 
#' calling \code{unique()} on objects of class \code{"stepFlexmix"}.
#' 
#' @import flexmix
#' @export
#' 
#' @rdname myStepFlexmix
#' @aliases myStepFlexmix 
#' 
#' @examples
#' library(locClassData)
#' data <- xor3Data(500)
#' model <- FLXMCLmultinom(trace = FALSE, decay = 0.1)
#' cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
#' fit <- myStepFlexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, 
#'     cluster = cluster, control = list(verb = 1, tolerance = 10^-1))
#' fit2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, 
#'     cluster = posterior(fit), control = list(verb = 1))
#' pred <- mypredict(fit2, aggregate = TRUE)
#' mean(max.col(pred[[1]]) != data$y)


myStepFlexmix <- function (..., cluster, verbose = FALSE, drop = TRUE, 
    unique = FALSE) 
{
    MYCALL <- match.call()
    MYCALL1 <- MYCALL
    nrep <- ncol(cluster)
    bestFlexmix <- function(..., cluster) {
        z = new("flexmix", logLik = -Inf)
        logLiks = rep(NA, length.out = nrep)
        for (m in seq_len(nrep)) {
            if (verbose) 
                cat(" *")
            x = try(flexmix(..., cluster = cluster[,m]))
            if (!is(x, "try-error")) {
                logLiks[m] <- logLik(x)
                if (logLik(x) > logLik(z)) 
                  z = x
            }
        }
        return(list(z = z, logLiks = logLiks))
    }
    z = list()
    # if (is.null(k)) {
        RET = bestFlexmix(..., cluster = cluster)
        z[[1]] <- RET$z
        logLiks <- as.matrix(RET$logLiks)
        z[[1]]@call <- MYCALL
        z[[1]]@control@nrep <- nrep
        names(z) <- as.character(z[[1]]@k)
        if (verbose) 
            cat("\n")
    # }
    # else {
        # k = as.integer(k)
        # logLiks <- matrix(nrow = length(k), ncol = nrep)
        # for (n in seq_along(k)) {
            # ns <- as.character(k[n])
            # if (verbose) 
                # cat(k[n], ":")
            # RET <- bestFlexmix(..., k = k[n])
            # z[[ns]] = RET$z
            # logLiks[n, ] <- RET$logLiks
            # MYCALL1[["k"]] <- as.numeric(k[n])
            # z[[ns]]@call <- MYCALL1
            # z[[ns]]@control@nrep <- nrep
            # if (verbose) 
                # cat("\n")
        # }
    # }
    logLiks <- logLiks[is.finite(sapply(z, logLik)), , drop = FALSE]
    z <- z[is.finite(sapply(z, logLik))]
    rownames(logLiks) <- apply(cluster, 2, function(x) length(unique(x)))
# print(logLiks)
    if (!length(z)) 
        stop("no convergence to a suitable mixture")
    if (drop & (length(z) == 1)) {
        return(z[[1]])
    }
    else {
        z <- return(new("stepFlexmix", models = z, k = as.integer(names(z)), 
            nrep = as.integer(nrep), logLiks = logLiks, call = MYCALL))
        if (unique) 
            z <- unique(z)
        return(z)
    }
}
