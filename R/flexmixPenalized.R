#' Internal functions not to be called by the user.
#'
#' @title Internal Functions
#'
#' @rdname FLXdeterminePostunscaled
# @aliases FLXdeterminePostunscaled,FLXMCLsvm-method
#' @import flexmix
#' @export
#'
#' @include FLXMCL.R FLXMCLsvm.R

setMethod("FLXdeterminePostunscaled", signature(model = "FLXMCLsvm"), function(model, components, ...) {
	ll <- lapply(components, function(x) x@logLik(model@x, model@y))
	lpost <- matrix(sapply(ll, function(x) return(x[["lpost"]])), nrow = nrow(model@y))
	reg <- sum(sapply(ll, function(x) return(x[["reg"]])))
# print(reg)
	return(list(lpost = lpost, reg =reg))
})



#' @rdname FLXdeterminePostunscaled
# @aliases FLXdeterminePostunscaled,FLXMCLmultinom-method
#' @import flexmix
#' @export

setMethod("FLXdeterminePostunscaled", signature(model = "FLXMCLmultinom"), function(model, components, ...) {
	ll <- lapply(components, function(x) x@logLik(model@x, model@y))
	lpost <- matrix(sapply(ll, function(x) return(x[["lpost"]])), nrow = nrow(model@y))
	reg <- sum(sapply(ll, function(x) return(x[["reg"]])))
# print(reg)
	return(list(lpost = lpost, reg = reg))
})



#' @title Internal Functions
#'
#' @rdname FLXdeterminePostunscaled
# @aliases FLXdeterminePostunscaled,FLXMCLnnet-method
#' @import flexmix
#' @export

setMethod("FLXdeterminePostunscaled", signature(model = "FLXMCLnnet"), function(model, components, ...) {
	ll <- lapply(components, function(x) x@logLik(model@x, model@y))
	lpost <- matrix(sapply(ll, function(x) return(x[["lpost"]])), nrow = nrow(model@y))
	reg <- sum(sapply(ll, function(x) return(x[["reg"]])))
# print(reg)
	return(list(lpost = lpost, reg = reg))
})



#' @rdname flexmixPenalized
# @aliases flexmix,formula,ANY,ANY,ANY,FLXMCLsvm-method
#' @import flexmix
#' @export

setMethod("flexmix",
          signature(formula = "formula", model="FLXMCLsvm"),
function(formula, data=list(), k=NULL, cluster=NULL, 
         model=NULL, concomitant=NULL, control=NULL, weights=NULL)
{
  mycall = match.call()
  z <- flexmixPenalized(formula=formula, data=data, k=k, cluster=cluster, 
               model=list(model), concomitant=concomitant,
               control=control, weights=weights)
  z@call <- mycall
  z
})



#' @rdname flexmixPenalized
# @aliases flexmix,formula,ANY,ANY,ANY,FLXMCLmultinom-method
#' @import flexmix
#' @export

setMethod("flexmix",
          signature(formula = "formula", model="FLXMCLmultinom"),
function(formula, data=list(), k=NULL, cluster=NULL, 
         model=NULL, concomitant=NULL, control=NULL, weights=NULL)
{
  mycall = match.call()
  z <- flexmixPenalized(formula=formula, data=data, k=k, cluster=cluster, 
               model=list(model), concomitant=concomitant,
               control=control, weights=weights)
  z@call <- mycall
  z
})



#' @rdname flexmixPenalized
# @aliases flexmix,formula,ANY,ANY,ANY,FLXMCLnnet-method
#' @import flexmix
#' @export

setMethod("flexmix",
          signature(formula = "formula", model="FLXMCLnnet"),
function(formula, data=list(), k=NULL, cluster=NULL, 
         model=NULL, concomitant=NULL, control=NULL, weights=NULL)
{
  mycall = match.call()
  z <- flexmixPenalized(formula=formula, data=data, k=k, cluster=cluster, 
               model=list(model), concomitant=concomitant,
               control=control, weights=weights)
  z@call <- mycall
  z
})



#' Extension of function \code{\link[flexmix]{flexmix}} in package \pkg{flexmix} that allows to fit mixtures of penalized classifiers. 
#' Not to be called by the user.
#'
#' @title Mixtures of Penalized Classification Methods
#'
# @param formula A symbolic description of the model to be fit. The general form is \code{y~x|g} where \code{y} is the response, \code{x} the set of predictors and \code{g} an optional grouping factor for repeated measurements.
# @param data An optional data frame containing the variables in the model.
# @param k Number of clusters (not needed if \code{cluster} is specified).
# @param cluster Either a matrix with \code{k} columns of initial cluster membership probabilities for each observation; or a factor or integer vector with the initial cluster assignments of observations at the start of the EM algorithm. Default is random assignment into \code{k} clusters.
# @param concomitant Object of class \code{FLXP}. Default is the object returned by calling \code{\link[flexmix]{FLXPconstant}}.
# @param control Object of class \code{FLXcontrol} or a named list.
# @param weights An optional vector of replication weights to be used in the fitting process. Should be \code{NULL}, an integer vector or a formula.
#
#' @import flexmix
#' @export
#'
#' @rdname flexmixPenalized

setGeneric("flexmixPenalized",
           function(formula, data=list(), k=NULL,
                    cluster=NULL, model=NULL, concomitant=NULL, control=NULL,
                    weights = NULL)
           standardGeneric("flexmixPenalized"))



#' @rdname flexmixPenalized
# @aliases flexmixPenalized,formula,ANY,ANY,ANY,list-method
#' @import flexmix
#' @export
#'
# @usage \S4method{flexmixPenalized}{formula,ANY,ANY,ANY,list}(formula, data=list(), k=NULL, cluster=NULL,
#         model=NULL, concomitant=NULL, control=NULL, weights=NULL)

setMethod("flexmixPenalized",
          signature(formula = "formula", model="list"),
function(formula, data=list(), k=NULL, cluster=NULL,
         model=NULL, concomitant=NULL, control=NULL, weights=NULL)
{
    mycall = match.call()
    control = as(control, "FLXcontrol")
    if (!is(concomitant, "FLXP")) concomitant <- FLXPconstant()
    
    groups <- flexmix:::.FLXgetGrouping(formula, data)
    model <- lapply(model, FLXcheckComponent, k, cluster)
    k <- unique(unlist(sapply(model, FLXgetK, k)))
    if (length(k) > 1) stop("number of clusters not specified correctly")

    model <- lapply(model, FLXgetModelmatrix, data, formula)
    
    groups$groupfirst <-
        if (length(groups$group)) groupFirst(groups$group)
        else rep(TRUE, FLXgetObs(model[[1]]))
    
    if (is(weights, "formula")) {
      weights <- model.frame(weights, data = data, na.action = NULL)[,1]
    }
    ## check if the weights are integer
    ## if non-integer weights are wanted modifications e.g.
    ## for classify != weighted and
    ## plot,flexmix,missing-method are needed
    if (!is.null(weights) & !identical(weights, as.integer(weights)))
      stop("only integer weights allowed")
    ## if weights and grouping is specified the weights within each
    ## group need to be the same
    if (!is.null(weights) & length(groups$group)>0) {
      unequal <- tapply(weights, groups$group, function(x) length(unique(x)) > 1)
      if (any(unequal)) stop("identical weights within groups needed")
    }
    
    postunscaled <- flexmix:::initPosteriors(k, cluster, FLXgetObs(model[[1]]), groups)
    if (ncol(postunscaled) == 1L)
      concomitant <- FLXPconstant()
  
    concomitant <- FLXgetModelmatrix(concomitant, data = data,
                                     groups = groups)

    z <- FLXfitPenalized(model=model, concomitant=concomitant, control=control,
                postunscaled=postunscaled, groups=groups, weights = weights)
    
    z@formula = formula
    z@call = mycall
    z@k0 = as.integer(k)
    z
})



#' Extension of function \code{\link[flexmix]{FLXfit}} in package \pkg{flexmix} that allows to fit mixtures of penalized classifiers.
#' Not to be called by the user.
#'
#' @title Mixtures of Penalized Classifiers
#'
# @param model List of \code{FLXM} objects where at least one is a \code{FLXMCLsvm}, \code{FLXMCLmultinom}, or \code{FLXMCLnnet} object.
# @param concomitant Object of class \code{FLXP}.
# @param control Object of class \code{FLXcontrol}.
# @param weights A numeric vector of weights to be used in the fitting process.
# @param postunscaled Initial a-posteriori probabilities of the observations at the start of the EM algorithm.
# @param groups List with components \code{group} which is a factor with optional grouping of observations and \code{groupfirst} which is a logical vector for the first observation of each group.
#'
#' @import flexmix
#' @export
#'
#' @rdname FLXfitPenalized

setGeneric("FLXfitPenalized",
           function(model, concomitant, control,
                    postunscaled=NULL, groups, weights)
           standardGeneric("FLXfitPenalized"))



#' @rdname FLXfitPenalized
# @aliases FLXfitPenalized,list-method
#' @import flexmix
#' @export
#'
# @usage \S4method{FLXfitPenalized}{list}(model, concomitant, control, postunscaled=NULL, groups, weights)

setMethod("FLXfitPenalized", signature(model="list"),
function(model, concomitant, control, postunscaled=NULL, groups, weights)
{
  ### initialize
  k <- ncol(postunscaled)
  N <- nrow(postunscaled)
  control <- flexmix:::allweighted(model, control, weights)
  if(control@verbose>0)
    cat("Classification:", control@classify, "\n")
  if (control@classify=="random") iter.rm <- 0
  group <- groups$group
  groupfirst <- groups$groupfirst

  if(length(group)>0) postunscaled <- groupPosteriors(postunscaled, group)

  logpostunscaled <- log(postunscaled)
  postscaled <- exp(logpostunscaled - flexmix:::log_row_sums(logpostunscaled))
  
  llh <- -Inf
  if (control@classify=="random") llh.max <- -Inf
  converged <- FALSE
  components <- NULL
  ### EM
  for(iter in seq_len(control@iter.max)) {
      ### M-Step
      postscaled = flexmix:::.FLXgetOK(postscaled, control, weights)
      prior <- if (is.null(weights))
        flexmix:::ungroupPriors(concomitant@fit(concomitant@x, postscaled[groupfirst,,drop=FALSE]),
                      group, groupfirst)
      else flexmix:::ungroupPriors(concomitant@fit(concomitant@x, (postscaled/weights)[groupfirst & weights > 0,,drop=FALSE], weights[groupfirst & weights > 0]),
                         group, groupfirst)
      # Check min.prior
      nok <- if (nrow(prior) == 1) which(prior < control@minprior) else {
               if (is.null(weights)) which(colMeans(prior[groupfirst,]) < control@minprior)
               else which(colSums(prior[groupfirst,] * weights[groupfirst])/sum(weights[groupfirst]) < control@minprior)
             }
      if(length(nok)) {
        if(control@verbose>0)
          cat("*** Removing", length(nok), "component(s) ***\n")
        prior <- prior[,-nok,drop=FALSE]
        prior <- prior/rowSums(prior)
        postscaled <- postscaled[,-nok,drop=FALSE]
        postscaled[rowSums(postscaled) == 0,] <- if (nrow(prior) > 1) prior[rowSums(postscaled) == 0,]
                                                 else prior[rep(1, sum(rowSums(postscaled) == 0)),]
        postscaled <- postscaled/rowSums(postscaled)
        if (!is.null(weights)) postscaled <- postscaled * weights
        k <- ncol(prior)
        if (k == 0) stop("all components removed")
        if (control@classify=="random") {
          llh.max <- -Inf
          iter.rm <- iter
        }
        model <- lapply(model, FLXremoveComponent, nok)
      }
      components <- lapply(seq_along(model), function(i) FLXmstep(model[[i]], postscaled, components[[i]]))
      postunscaled <- matrix(0, nrow = N, ncol = k)
      reg <- 0
      for (n in seq_along(model)) {
      	helper <- FLXdeterminePostunscaled(model[[n]], components[[n]])
        postunscaled <- postunscaled + helper$lpost							# posterior
        reg <- reg + helper$reg												# regularization term, skalar
      }
 # cat("postscaled\n")
 # print(head(postunscaled))
 # cat("reg\n")
 # print(head(reg))
 # cat("prior\n")
 # print(colSums(prior))
      if(length(group)>0) {
        postunscaled <- groupPosteriors(postunscaled, group)
        #reg <- groupPosteriors(reg, group)									# necessary ???
      }
      ### E-Step
      ## Code changed thanks to Nicolas Picard
      ## to avoid problems with small likelihoods
      postunscaled <- if (nrow(prior) > 1) postunscaled + log(prior)
                         else sweep(postunscaled, 2, log(prior), "+")
      logpostunscaled <- postunscaled
      postunscaled <- exp(postunscaled)
      postscaled <- exp(logpostunscaled - flexmix:::log_row_sums(logpostunscaled))
      ##<FIXME>: wenn eine beobachtung in allen Komonenten extrem
      ## kleine postunscaled-werte hat, ist exp(-postunscaled)
      ## numerisch Null, und damit postscaled NaN
      ## log(rowSums(postunscaled)) ist -Inf
      ##</FIXME>
      if (any(is.nan(postscaled))) {
        index <- which(as.logical(rowSums(is.nan(postscaled))))
        postscaled[index,] <- if(nrow(prior)==1) rep(prior, each = length(index)) else prior[index,]
        postunscaled[index,] <- .Machine$double.xmin
      }
      ### check convergence
      llh.old <- llh
      llh <- if (is.null(weights)) sum(flexmix:::log_row_sums(logpostunscaled[groupfirst,,drop=FALSE])) + reg # + rowSums(reg[groupfirst,,drop=FALSE]))
             else sum(flexmix:::log_row_sums(logpostunscaled[groupfirst,,drop=FALSE])*weights[groupfirst]) + reg #rowSums(reg[groupfirst,,drop=FALSE]))
      # llh <- if (is.null(weights)) sum(log_row_sums(logpostunscaled[groupfirst,,drop=FALSE]))
             # else sum(log_row_sums(logpostunscaled[groupfirst,,drop=FALSE])*weights[groupfirst])
      if(is.na(llh) | is.infinite(llh))
        stop(paste(formatC(iter, width=4),
                   "Log-likelihood:", llh))
      if (abs(llh-llh.old)/(abs(llh)+0.1) < control@tolerance){
        if(control@verbose>0){
          flexmix:::printIter(iter, llh)
          cat("converged\n")
        }
        converged <- TRUE
        break
      }
      if (control@classify=="random") {
        if (llh.max < llh) {
          components.max <- components
          prior.max <- prior
          postscaled.max <- postscaled
          postunscaled.max <- postunscaled
          llh.max <- llh
        }
      }
      if(control@verbose && (iter%%control@verbose==0))
        flexmix:::printIter(iter, llh)
    }
  ### Construct return object
  if (control@classify=="random") {
    components <- components.max
    prior <- prior.max
    postscaled <- postscaled.max
    postunscaled <- postunscaled.max
    llh <- llh.max
    iter <- control@iter.max - iter.rm
  }

  components <- lapply(seq_len(k), function(i) lapply(components, function(x) x[[i]]))
  names(components) <- paste("Comp", seq_len(k), sep=".") 
  cluster <- max.col(postscaled)
  size <-  if (is.null(weights)) tabulate(cluster, nbins=k) else tabulate(rep(cluster, weights), nbins=k)
  names(size) <- seq_len(k)
  concomitant <- flexmix:::FLXfillConcomitant(concomitant, postscaled[groupfirst,,drop=FALSE], weights[groupfirst])
  df <- concomitant@df(concomitant@x, k) + sum(sapply(components, sapply, slot, "df"))
  control@nrep <- 1
  prior <- if (is.null(weights)) colMeans(postscaled[groupfirst,,drop=FALSE])
           else colSums(postscaled[groupfirst,,drop=FALSE] * weights[groupfirst])/sum(weights[groupfirst])

  retval <- new("flexmix", model=model, prior=prior,
                posterior=list(scaled=postscaled,
                  unscaled=postunscaled),
                weights = weights,
                iter=iter, cluster=cluster, size = size,
                logLik=llh, components=components,
                concomitant=concomitant,
                control=control, df=df, group=group, k=as(k, "integer"),
                converged=converged)
  retval
})
