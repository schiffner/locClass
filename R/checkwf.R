# @param wf A string. The name of the window function. 
# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the \code{k} nearest neighbors or 
#   all observations receive positive weights? 
# @param n The number of training observations.
# @param \dots Unused.
#
#' @noRd

checkwf <- function(wf = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", 
	"gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only = TRUE, n, ...) {
    wf <- match.arg(wf)
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k)) 
				stop("'k' must be numeric of length > 0")
   			if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      		 }
			if (k <= 0)
   				stop("'k' must be positive")
			if (k + 1 > n)
				stop("'k + 1' is larger than 'n'")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			variant <- 3
			# window functions with infinite support are cut depending on nn.only
			if (wf %in% c("cauchy", "exponential", "gaussian")) {
				if(!nn.only)
					variant <- 4
			} else if (wf == "rectangular") {
				if (k == n) {
					warning("nonlocal solution")
					variant <- 0
				}	
			}
			adaptive <- TRUE
			bw <- NULL
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
   	 		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			if (!missing(nn.only))
				warning("argument 'nn.only' is ignored")
			nn.only <- NULL
			## window functions with fixed bandwidth
			variant <- 1
			k <- NULL
		} else {			# bw and k given -> fixed bandwidth with nn.only
			if (!missing(nn.only))
				if (!nn.only)
					stop("if 'bw' and 'k' are given argument 'nn.only' must be TRUE")
			# checks on k
			if ((!is.numeric(k)) || !length(k))
   	   	 		stop("'k' must be numeric of length > 0")
   		 	if (length(k) > 1) {
      			k <- k[1]
       			warning("only first element of 'k' used")
   		 	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (k > n)
    			stop("'k' is larger than 'n'")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
			variant <- 2
		}
		adaptive <- FALSE
	}
	return(list(wf = wf, bw = bw, k = k, nn.only = nn.only, adaptive = adaptive, variant = variant))
}
