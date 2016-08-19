#### flexmix
### FLXdist
## FLXM (component specific models)
## FLXcomponent (functions to determine component specific log-liklihoods and predictions, component-specific parameters)
## FLXP concomitant variable model
### FLXcontrol


#' @title Class "FLXMCL"
#'
#' @description Model specification for mixtures of classifiers.
#'
#' @details Objects can be created by calls of the form \code{new("FLXMCL", ...)},
#'   typically inside driver functions like \code{\link{FLXMCLlda}} or
#'   \code{\link{FLXMCLmultinom}}. 
#' Class FLXMCL extends class \code{\link[flexmix]{FLXM-class}} from package \pkg{flexmix} directly
#' and has exactly the same slots.
#'
#' @rdname FLXMCL
#'
# @aliases FLXMCL-class FLXMCLconstant-class FLXMCLlda-class FLXMCLqda-class FLXMCLmultinom-class FLXMCLnnet-class FLXMCLsvm-class
#'
#' @family mixtures
#' 
#' @import flexmix
#'
#' @export
#' 
# @section Slots:
#  \describe{
#    \item{\code{fit}:}{Function returning an \code{FLXcomponent} object.}
#    \item{\code{defineComponent}:}{Expression to determine the
#      \code{FLXcomponent} object given the parameters.}
#    \item{\code{weighted}:}{Logical indicating whether \code{fit} can do
#    weighted likelihood maximization.}
#    \item{\code{name}:}{Character string used in print methods.}
#    \item{\code{formula}:}{Formula describing the model.}
#    \item{\code{fullformula}:}{Resulting formula from updating the model
#      formula with the formula specified in the call to \code{flexmix}.}
#    \item{\code{x}:}{Model matrix.}
#    \item{\code{y}:}{Model response.}
#    \item{\code{terms}, \code{xlevels}, \code{contrasts}:}{Additional
#      information for model matrix.}
#    \item{\code{preproc.x}:}{Function for preprocessing matrix \code{x}
#      before the EM algorithm starts, by default the identity function.}
#    \item{\code{preproc.y}:}{Function for preprocessing matrix \code{y}
#      before the EM algorithm starts, by default the identity function.}
#  }
#'
#' @keywords classes
#'
#' @seealso \code{\link[flexmix]{FLXM-class}}.

setClass("FLXMCL", contains = "FLXM")



#' Extract fitted values from a flexmix object.
#'
#' @title Extract Fitted Values
#'
#' @param object An object of class "flexmix".
#' @param drop Logical. If \code{TRUE} the function tries to
#'    simplify the return object by combining lists of length 1 into
#'    matrices.
#' @param aggregate Logical. If \code{TRUE} the fitted values for
#'    each model aggregated over the components are returned.
#' @param \dots Currently unused.
#'
#' @rdname myfitted
#'
#' @family mixtures
#'
#' @import flexmix
#' @export

setGeneric("myfitted",
	function(object, drop = TRUE, aggregate = FALSE, ...)
	standardGeneric("myfitted"))

# setMethod("fitted", signature(object="flexmix"),
# function(object, drop=TRUE, aggregate = FALSE, ...)
# {
    # x<- list()
    # for(m in seq_along(object@model)) {
      # comp <- lapply(object@components, "[[", m)
      # x[[m]] <- fitted(object@model[[m]], comp, ...)
    # }
    # if (aggregate) {
      # group <- group(object)
      # prior_weights <- determinePrior(object@prior, object@concomitant, group)[as.integer(group),]
      # z <- lapply(x, function(z) matrix(rowSums(do.call("cbind", z) * prior_weights),
                                        # nrow = nrow(z[[1]])))
      # if(drop && all(lapply(z, ncol)==1)){
        # z <- sapply(z, unlist)
      # }
    # }
    # else {
      # z <- list()
      # for (k in seq_len(object@k)) {
        # z[[k]] <- do.call("cbind", lapply(x, "[[", k))
      # }
      # names(z) <- paste("Comp", seq_len(object@k), sep=".")
      # if(drop && all(lapply(z, ncol)==1)){
        # z <- sapply(z, unlist)
      # }
    # }
    # z
# })


#' @rdname myfitted
#'
#' @family mixtures
#'
#' @import flexmix
#' @export

setMethod("myfitted", signature(object = "flexmix"), function (object, drop = TRUE, aggregate = FALSE, ...){
	if (aggregate & inherits(object@model[[1]], "FLXMCL")) {
		if (length(object@model) > 1)
			stop ("currently not supported")
		else {
    		x <- list()
    		for (m in seq_along(object@model)) {
      			comp <- lapply(object@components, "[[", m)
      			x[[m]] <- fitted(object@model[[m]], comp, ...)
    		}
      		group <- group(object)
      		prior_weights <- determinePrior(object@prior, object@concomitant, group)[as.integer(group),]
			if (any(is.na(prior_weights)))
				warning("NAs in 'prior_weights'")
			if (is.null(dim(prior_weights)))
				prior_weights <- matrix(prior_weights, ncol = length(prior_weights), dimnames = list(NULL, names(prior_weights)))
        	# if (class(object@model[[1]]) == "FLXMCLsvm") {
        		# z <- lapply(x, function(z)
        		    # list(decision = matrix(rowSums(matrix(sapply(seq_len(object@k), 
                		# function(K) z[[K]][["decision"]] * prior_weights[,K]), ncol = object@k)), 
                		# nrow = nrow(z[[1]][["decision"]])),
                	# posterior = matrix(rowSums(matrix(sapply(seq_len(object@k), 
                		# function(K) z[[K]][["posterior"]] * prior_weights[,K]), ncol = object@k)), 
                		# nrow = nrow(z[[1]][["posterior"]]))))
        	# } else {
       			z <- lapply(x, function(z)
        		    matrix(rowSums(matrix(sapply(seq_len(object@k), 
                	function(K) z[[K]] * prior_weights[,K]), ncol = object@k)), 
                	nrow = nrow(z[[1]])))
        	# }        	
         	for (m in seq_along(object@model)) {
         		# if (class(object@model[[m]]) == "FLXMCLsvm") {
					# #for (j in seq_along(z[[m]]))
						# dimnames(z[[m]]$decision) <- dimnames(x[[m]][[1]]$decision)
						# dimnames(z[[m]]$posterior) <- dimnames(x[[m]][[1]]$posterior)
				# } else
	         		dimnames(z[[m]]) <- dimnames(x[[m]][[1]])
         	}
            # z <- lapply(x, function(z) matrix(rowSums(matrix(sapply(seq_len(object@k), 
                # function(K) z[[K]] * object@prior[K]), ncol = object@k)), 
                # nrow = nrow(z[[1]])))
            if (drop && all(lapply(z, ncol) == 1)) {
                z <- sapply(z, unlist)
            }
        	return(z)
        }
	} else
		return(fitted(object, drop, aggregate, ...))
})



#' Predict values for a FLXdist object.
#'
#' @title Predict Values
#'
#' @param object An object of class "FLXdist".
#' @param newdata Dataframe containing new data.
#' @param aggregate Logical. If \code{TRUE} the predicted values for
#'    each model aggregated over the components are returned.
#' @param \dots Passed to the \code{predict}-method of the model class.
#'
#' @rdname mypredict
#'
#' @family mixtures
#'
#' @import flexmix
#' @export

# setMethod("predict", signature(object="FLXdist"),
# function(object, newdata=list(), aggregate=FALSE, ...){
    # if (missing(newdata)) return(fitted(object, aggregate=aggregate, drop=FALSE))
    # x = list()
    # for(m in seq_along(object@model)) {
      # comp <- lapply(object@components, "[[", m)
      # x[[m]] <- predict(object@model[[m]], newdata, comp, ...)
    # }
    # if (aggregate) {
      # prior_weights <- prior(object, newdata)
      # z <- lapply(x, function(z) matrix(rowSums(do.call("cbind", z) * prior_weights), nrow = nrow(z[[1]])))
    # }
    # else {
      # z <- list()
      # for (k in seq_len(object@k)) {
        # z[[k]] <- do.call("cbind", lapply(x, "[[", k))
      # }
      # names(z) <- paste("Comp", seq_len(object@k), sep=".")
    # }
    # z
# })


setGeneric("mypredict",
	function(object, newdata = list(), aggregate = FALSE, ...)
	standardGeneric("mypredict"))



#' @rdname mypredict
#'
#' @family mixtures
#'
#' @import flexmix
#' @export

setMethod("mypredict", signature(object = "FLXdist"), function (object, newdata = list(), aggregate = FALSE, ...){
	if (aggregate & inherits(object@model[[1]], "FLXMCL")) {
		if (length(object@model) > 1)
			stop ("currently not supported")
		else {
    		if (missing(newdata)) return(myfitted(object, aggregate = aggregate, drop = FALSE))
    		x = list()
    		for(m in seq_along(object@model)) {
      			comp <- lapply(object@components, "[[", m)
      			x[[m]] <- predict(object@model[[m]], newdata, comp, ...)
    		}
			prior_weights <- prior(object, newdata)
			if (any(is.na(prior_weights)))
				warning("NAs in 'prior_weights'")
			if (is.null(dim(prior_weights)))
				prior_weights <- matrix(prior_weights, ncol = length(prior_weights), dimnames = list(NULL, names(prior_weights)))
        	# if (class(object@model[[1]]) == "FLXMCLsvm") {
        		# z <- lapply(x, function(z)
        		    # list(decision = matrix(rowSums(matrix(sapply(seq_len(object@k), 
                		# function(K) z[[K]][["decision"]] * prior_weights[,K]), ncol = object@k)), 
                		# nrow = nrow(z[[1]][["decision"]])),
                	# posterior = matrix(rowSums(matrix(sapply(seq_len(object@k), 
                		# function(K) z[[K]][["posterior"]] * prior_weights[,K]), ncol = object@k)), 
                		# nrow = nrow(z[[1]][["posterior"]]))))
        	# } else {
        		z <- lapply(x, function(z)
        		    matrix(rowSums(matrix(sapply(seq_len(object@k), 
                	function(K) z[[K]] * prior_weights[,K]), ncol = object@k)), 
                	nrow = nrow(z[[1]])))
        	# }        	
         	for (m in seq_along(object@model)) {
         		# if (class(object@model[[m]]) == "FLXMCLsvm") {
					# dimnames(z[[m]]$decision) <- dimnames(x[[m]][[1]]$decision)
					# dimnames(z[[m]]$posterior) <- dimnames(x[[m]][[1]]$posterior)
				# } else         		
	         		dimnames(z[[m]]) <- dimnames(x[[m]][[1]])     		
         	}
            # z <- lapply(x, function(z) matrix(rowSums(matrix(sapply(seq_len(object@k), 
                # function(K) return(z[[K]] * post[,K])), ncol = object@k)), 
                # nrow = nrow(z[[1]])))
         	# for (m in seq_along(object@model))
         		# colnames(z[[m]]) <- colnames(x[[m]][[1]])
       		return(z)
		}
	} else {
		if (missing(newdata)) {
			return(fitted(object, aggregate = aggregate, drop = FALSE))
		}
		else
			return(predict(object, newdata, aggregate = aggregate, ...))
	}	
})
