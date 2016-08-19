# @param wf A string. The name of the window function. Default is \code{"biweight"}.
# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
# @param /dots Unused.
#
#' @noRd

generatewf <- function(wf = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian",
	"optcosine", "rectangular", "triangular"), bw, k, nn.only = TRUE, n, ...) {
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
			if (k+1 > n)
				stop("'k' is larger than 'n - 1'")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
			wfunc <- switch(wf,
        		biweight = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
            		weights <- numeric(length(ax))
    	        	weights[ind] <- 15/16 * (1 - (ax[ind]/bw)^2)^2/bw
    	        	weights
					###
            		# ax <- abs(x)
            		# sax <- sort(ax)
            		# bw <- sax[k] + 1e-06
        			# ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
        		},
        		cauchy = if (nn.only) {
	        		function(x) {
						if (any(x < 0))
							stop("'x' must be positive")
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
						ind <- ax < bw
						untied <- sum(ind)
						if (untied > k) {
							indTied <- ax == sax[k]
							s <- sample(which(indTied), size = untied - k)
							ind[s] <- FALSE
						}
						weights <- numeric(length(ax))
						weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
						weights
						###
	            		# ax <- abs(x)
    	        		# sax <- sort(ax)
        	    		# bw <- sax[k] + 1e-06
						# ind <- ax < bw
            			# weights <- numeric(length(ax))
    	        		# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        	    		# weights
        			}
        		} else {
        			function(x) {
						if (any(x < 0))
							stop("'x' must be positive")
	            		ax <- abs(x)
    	        		sax <- sort(ax)
        	    		bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight

        				1/(pi * (1 + (ax/bw)^2) * bw)        				
						###
	            		# ax <- abs(x)
    	        		# sax <- sort(ax)
        	    		# bw <- sax[k] + 1e-06
        				# 1/(pi * (1 + (ax/bw)^2) * bw)        				
        			}
        		},
      		  	cosine = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
					weights <- numeric(length(ax))
					weights[ind] <- (1 + cos(pi * ax[ind]/bw))/(2 * bw)
					weights
            		###
            		# ax <- abs(x)
   	        		# sax <- sort(ax)
       	    		# bw <- sax[k] + 1e-06
            		# ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
            	},
       		 	epanechnikov = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
					weights <- numeric(length(ax))
					weights[ind] <- 3/4 * (1 - (ax[ind]/bw)^2)/bw
					weights
					###
            		# ax <- abs(x)
   	        		# sax <- sort(ax)
       	    		# bw <- sax[k] + 1e-06
            		# ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
        		},
        		exponential = if (nn.only) {
        			function(x) {
						if (any(x < 0))
							stop("'x' must be positive")
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
						ind <- ax < bw
						untied <- sum(ind)
						if (untied > k) {
							indTied <- ax == sax[k]
							s <- sample(which(indTied), size = untied - k)
							ind[s] <- FALSE
						}
            			weights <- numeric(length(ax))
        				weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        				weights
						###
	            		# ax <- abs(x)
   		        		# sax <- sort(ax)
       		    		# bw <- sax[k] + 1e-06
						# ind <- ax < bw
            			# weights <- numeric(length(ax))
        				# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        				# weights
        			}
        		} else {
        			function(x) {
						if (any(x < 0))
							stop("'x' must be positive")
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
        				0.5 * exp(-ax/bw)/bw
						###
	            		# ax <- abs(x)
   		        		# sax <- sort(ax)
       		    		# bw <- sax[k] + 1e-06
        				# 0.5 * exp(-ax/bw)/bw
        			}
        		},
        		gaussian = if (nn.only) { ###
        			function(x) {
						if (any(x < 0))
							stop("'x' must be positive")
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
						ind <- ax < bw
						untied <- sum(ind)
						if (untied > k) {
							indTied <- ax == sax[k]
							s <- sample(which(indTied), size = untied - k)
							ind[s] <- FALSE
						}
            			weights <- numeric(length(ax))
        				weights[ind] <- dnorm(x[ind], sd = bw)
        				weights
        				###
	            		# ax <- abs(x)
   		        		# sax <- sort(ax)
       		    		# bw <- sax[k] + 1e-06
						# ind <- ax < bw
            			# weights <- numeric(length(ax))
    	        		# weights[ind] <- dnorm(x[ind], sd = bw)
    	        		# weights
        			}
        		} else {
        			function(x) {
						if (any(x < 0))
							stop("'x' must be positive")
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
        				dnorm(x, sd = bw)
	      		  		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
        	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        				# dnorm(x, sd = bw)
        				# bw <- sort(abs(x))[k+1] + .Machine$double.eps
        				# dnorm(x, sd = bw)
        			}
        		},
        		optcosine = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
       				weights[ind] <- pi/4 * cos(pi * x[ind]/(2 * bw))/bw
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# bw <- sax[k] + 1e-06
         	  	 	# ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
         	  	},
        		rectangular = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
       				weights[ind] <- 0.5/bw
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# bw <- sax[k] + 1e-06
            		# ifelse(ax < bw, 0.5/bw, 0)
            	},
       		 	triangular = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
       				weights[ind] <- (1 - ax[ind]/bw)/bw
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# bw <- sax[k] + 1e-06
           		 	# ifelse(ax < bw, (1 - ax/bw)/bw, 0)
        		})
        	if (wf %in% c("cauchy", "exponential", "gaussian"))
        		attributes(wfunc) <- list(name = wf, k = k, nn.only = nn.only, adaptive = TRUE)
			else {
        		if (!missing(nn.only))
        			warning("argument 'nn.only' is ignored")
        		attributes(wfunc) <- list(name = wf, k = k, nn.only = TRUE, adaptive = TRUE)
        	}
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
			## window functions with fixed bandwidth
			wfunc <- switch(wf,
        		biweight = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
            		ifelse(abs(x) < bw, 15/16 * (1 - (x/bw)^2)^2/bw, 0)
            	},
        		cauchy = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")        			
        			1/(pi * (1 + (x/bw)^2) * bw)
        		},
      		  	cosine = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")      		  		
            		ifelse(abs(x) < bw, (1 + cos(pi * x/bw))/(2 * bw), 0)
            	},
       		 	epanechnikov = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
            		ifelse(abs(x) < bw, 3/4 * (1 - (x/bw)^2)/bw, 0)
            	},
        		exponential = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
        			0.5 * exp(-abs(x)/bw)/bw
        		},
        		gaussian = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")        			
        			dnorm(x, sd = bw)
        		},
        		optcosine = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")        			
         	  	 	ifelse(abs(x) < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
         	  	 },
        		rectangular = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
            		ifelse(abs(x) < bw, 0.5/bw, 0)
            	},
       		 	triangular = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
            		ax <- abs(x)
           		 	ifelse(ax < bw, (1 - ax/bw)/bw, 0)
        		})
        	attributes(wfunc) <- list(name = wf, bw = bw, adaptive = FALSE)
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
    		wfunc <- switch(wf,
        		biweight = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
       				weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
        	    	# weights
        		},
        		cauchy = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
       				weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
       				weights
					###
            		# ax <- x^2
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        	    	# weights
        		},
      		  	cosine = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
        	    	# weights
            	},
       		 	epanechnikov = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
        	    	# weights
        		},
        		exponential = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        	    	# weights
        		},
        		gaussian = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- dnorm(x[ind], sd = bw)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- dnorm(x[ind], sd = bw)
        	    	# weights
        		},
        		optcosine = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
        	    	# weights
         	  	},
        		rectangular = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
        	    	# weights
            	},
       		 	triangular = function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < knnbw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
       				weights
					###
            		# ax <- abs(x)
	        		# sax <- sort(ax)
   		    		# knnbw <- sax[k] + 1e-06
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
        	    	# weights
        		})
        		attributes(wfunc) <- list(name = wf, bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(wfunc)
}

#' The window functions generating functions that are used in various local classification methods.
#'
#' These functions are used to initialize window functions that can be passed as
#' \code{wf} argument to various local classification methods.
#'
#' If only \code{bw} is given a window function with fixed bandwidth is returned.
#'
#' If only \code{k} is given a window function with \code{k} nearest neighbors bandwidth, i.e. adaptive to the local density of data points, is generated.
#' In case of window functions with infinite support, \code{"cauchy"}, \code{"exponential"} and \code{"gaussian"}, the argument \code{nn.only} is used to decide 
#' if only the \code{k} nearest neighbors or all observations receive positive weights.
#'
#' If \code{bw} and \code{k} are both specified, a window function with fixed bandwidth is generated and all weights are set to zero except for
#' the \code{k} nearest neighbors.
#' 
#' Parts of the source code are based on the function \link[stats]{density} in package \pkg{stats}. Concerning the \code{"cosine"} and \code{"optcosine"} windows, it applies
#' the same as for \link[stats]{density}: \code{"cosine"} is smoother than \code{"optcosine"}, which is the usual 'cosine' kernel in the literature.
#' \code{"cosine"} is the version used by the S programming language.
#'
#' @title Generation of Window Functions
#'
#' @param bw The bandwidth parameter.
#' @param k The number of nearest neighbors.
#' @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#'
#' @return Returns an object of class \code{"function"}. The resulting \code{function} implements the desired window function and depends on one
#' argument \code{x} that is usually some sort of distance (and assumed to be positive).
#' The returned function has several attributes, depending on which arguments are specified.
#' \describe{
#'   \item{\code{"name"}}{The name of the window function.}
#'   \item{\code{"bw"}}{(If the corresponding argument is given.) The chosen bandwidth.}
#' 	 \item{\code{"k"}}{(If the corresponding argument is given.) The chosen number of nearest neighbors.}
#'   \item{\code{"nn.only"}}{(Logical. Only if \code{k} was specified.) \code{TRUE} if only the k nearest neighbors are used. 
#'     (\code{nn.only} is always \code{TRUE} except for window functions with infinite support.)}
#'   \item{\code{"adaptive"}}{(Logical.) \code{TRUE} in case of an adaptive bandwidth, \code{FALSE} if the bandwidth is fixed.}
#'	}
#'
#'
#' @seealso Documentation of various local classification methods, e.g. \code{\link{dalda}} or \code{\link{oslda}}, and \link[stats]{density}.
#'
#' @family weights
#'
#' @examples
#' x <- seq(0,1,0.01)
#' 
#' ## fixed bandwidth
#' gwf <- gaussian(bw = 1)
#' gwf
#' curve(gwf(x))
#'
#' ## adaptive bandwidth, only the 50 nearest neighbors receive positive weights
#' gwf <- gaussian(k = 50)
#' gwf
#' curve(gwf(x))
#'
#' ## adaptive bandwidth, all observations have positive weights
#' gwf <- gaussian(k = 50, nn.only = FALSE)
#' gwf
#' curve(gwf(x))
#' 
#' ## fixed bandwidth, only the 100 nearest neighbors get positive weights
#' gwf <- gaussian(k = 50, bw = 1)
#' gwf
#' curve(gwf(x))
#'
#' @rdname wfs
#'
#' @aliases wfs biweight cauchy cosine epanechnikov exponential gaussian optcosine rectangular triangular
#'
#' @export biweight cauchy cosine epanechnikov exponential gaussian optcosine rectangular triangular
#'

biweight <- function(bw, k) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
      		bi <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		   	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < bw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
            	weights[ind] <- 15/16 * (1 - (ax[ind]/bw)^2)^2/bw
            	weights
				###
            	# ax <- abs(x)
            	# sax <- sort(ax)
            	# bw <- sax[k] + 1e-06
        		# ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
   			}
		    attributes(bi) <- list(name = "biweight", k = k, nn.only = TRUE, adaptive = TRUE)
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
			## window functions with fixed bandwidth
			bi = function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
            	ifelse(abs(x) < bw, 15/16 * (1 - (x/bw)^2)^2/bw, 0)
            }
		    attributes(bi) <- list(name = "biweight", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
        	bi <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		   	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
       			weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
       			weights
				###
            	# ax <- abs(x)
            	# sax <- sort(ax)
            	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
    	        # weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
        	    # weights
        	}
        	attributes(bi) <- list(name = "biweight", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(bi)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#'
#' @family weights
#'
#' @rdname wfs

cauchy <- function(bw, k, nn.only = TRUE) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
        	if (nn.only) {
        		cau <- function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
		            ax <- abs(x)
   			        sax <- sort(ax)
   	    		   	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
   	        		weights <- numeric(length(ax))
   	        		weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
   	    	     	weights
					###
	            	# ax <- abs(x)
    	        	# sax <- sort(ax)
        	    	# bw <- sax[k] + 1e-06
					# ind <- ax < bw
        	    	# weights <- numeric(length(ax))
   	        		# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
       	    		# weights
        		}
        	} else {
        		cau <- function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
    	        	sax <- sort(ax)
        	    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
        			1/(pi * (1 + (ax/bw)^2) * bw)        				
					###
	            	# ax <- abs(x)
    	        	# sax <- sort(ax)
        	    	# bw <- sax[k] + 1e-06
					# 1/(pi * (1 + (ax/bw)^2) * bw)
        		}
        	}
			attributes(cau) <- list(name = "cauchy", k = k, nn.only = nn.only, adaptive = TRUE)
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
			## window functions with fixed bandwidth
			cau <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
				1/(pi * (1 + (x/bw)^2) * bw)
			}
			attributes(cau) <- list(name = "cauchy", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
			cau <- function(x) { 
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		   	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
       			weights <- numeric(length(ax))
       			weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
  				weights
				###
            	# ax <- abs(x)
   	        	# sax <- sort(ax)
       	    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
       	    	# weights <- numeric(length(ax))
	       		# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        		# weights
			}
			attributes(cau) <- list(name = "cauchy", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(cau)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @family weights
#'
#' @rdname wfs

cosine <- function(bw, k) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
    		cosi <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		   	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < bw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
				weights <- numeric(length(ax))
				weights[ind] <- (1 + cos(pi * ax[ind]/bw))/(2 * bw)
				weights
    			###
            	# ax <- abs(x)
   	        	# sax <- sort(ax)
       	    	# bw <- sax[k] + 1e-06
            	# ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
    		}    
    		attributes(cosi) <- list(name = "cosine", k = k, nn.only = TRUE, adaptive = TRUE)
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
			## window functions with fixed bandwidth
    		cosi <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
        		ifelse(abs(x) < bw, (1 + cos(pi * x/bw))/(2 * bw), 0)
        	}
    		attributes(cosi) <- list(name = "cosine", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		cosi <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		       	sax <- sort(ax)
       		   	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
         		weights <- numeric(length(ax))
    	       	weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
       			weights
 				###
            	# ax <- abs(x)
   	        	# sax <- sort(ax)
       	    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
       	    	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
        		# weights
    		}    
    		attributes(cosi) <- list(name = "cosine", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(cosi)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @family weights
#'
#' @rdname wfs

epanechnikov <- function(bw, k) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
    		epan <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < bw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
				weights <- numeric(length(ax))
				weights[ind] <- 3/4 * (1 - (ax[ind]/bw)^2)/bw
				weights
				###
            	# ax <- abs(x)
   	        	# sax <- sort(ax)
       	    	# bw <- sax[k] + 1e-06
        		# ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
    		}
    		attributes(epan) <- list(name = "epanechnikov", k = k, nn.only = TRUE, adaptive = TRUE)
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
			## window functions with fixed bandwidth
    		epan <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
        		ifelse(abs(x) < bw, 3/4 * (1 - (x/bw)^2)/bw, 0)
        	}
    		attributes(epan) <- list(name = "epanechnikov", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		epan <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		   	knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
       			weights <- numeric(length(ax))
    	       	weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
       			weights
				###
            	# ax <- abs(x)
   	        	# sax <- sort(ax)
       	    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
       	    	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
        		# weights
    		}
    		attributes(epan) <- list(name = "epanechnikov", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(epan)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#
#' @family weights
#'
#' @rdname wfs

exponential <- function(bw, k, nn.only = TRUE) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
        	if (nn.only) {
        		expo <- function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
           			weights <- numeric(length(ax))
       				weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
       				weights
					###
	           		# ax <- abs(x)
   	        		# sax <- sort(ax)
   		    		# bw <- sax[k] + 1e-06
					# ind <- ax < bw
           			# weights <- numeric(length(ax))
       				# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
       				# weights
        		}
        	} else {
        		expo <- function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
        			0.5 * exp(-ax/bw)/bw
					###
	            	# ax <- abs(x)
   		        	# sax <- sort(ax)
       		    	# bw <- sax[k] + 1e-06
        			# 0.5 * exp(-ax/bw)/bw
        		}
        	}
			attributes(expo) <- list(name = "exponential", k = k, nn.only = nn.only, adaptive = TRUE)
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
			## window functions with fixed bandwidth
			expo <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
        		0.5 * exp(-abs(x)/bw)/bw
        	}
			attributes(expo) <- list(name = "exponential", bw = bw, adaptive = FALSE)
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
			expo <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
    	        weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
       			weights
				###
	            # ax <- abs(x)
   		        # sax <- sort(ax)
       		    # knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
           		# weights <- numeric(length(ax))
        		# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        		# weights	
			}
			attributes(expo) <- list(name = "exponential", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(expo)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#
#' @family weights
#'
#' @rdname wfs

gaussian <- function(bw, k, nn.only = TRUE) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
        	if (nn.only) {
        		gauss <- function(x) {###
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
					ind <- ax < bw
					untied <- sum(ind)
					if (untied > k) {
						indTied <- ax == sax[k]
						s <- sample(which(indTied), size = untied - k)
						ind[s] <- FALSE
					}
            		weights <- numeric(length(ax))
        			weights[ind] <- dnorm(x[ind], sd = bw)
        			weights
        			###
	            	# ax <- abs(x)
   		        	# sax <- sort(ax)
       		    	# bw <- sax[k] + 1e-06
					# ind <- ax < bw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- dnorm(x[ind], sd = bw)
    	        	# weights
        		}
        	} else {
        		gauss <- function(x) {
					if (any(x < 0))
						stop("'x' must be positive")
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
        			dnorm(x, sd = bw)
					###
	      		  	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
        	    	# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        			# dnorm(x, sd = bw)
        			# bw <- sort(abs(x))[k+1] + .Machine$double.eps
        			# dnorm(x, sd = bw)
        		}
        	}
    		attributes(gauss) <- list(name = "gaussian", k = k, nn.only = nn.only, adaptive = TRUE)
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
			## window functions with fixed bandwidth
    		gauss <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
    			dnorm(x, sd = bw)
    		}
			attributes(gauss) <- list(name = "gaussian", bw = bw, adaptive = FALSE)
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
        	gauss <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
    	       	weights[ind] <- dnorm(x[ind], sd = bw)
       			weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
    	       	# weights[ind] <- dnorm(x[ind], sd = bw)
        	   	# weights
        	}
			attributes(gauss) <- list(name = "gaussian", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(gauss)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @family weights
#'
#' @rdname wfs

optcosine <- function(bw, k) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
		    optcos <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		   	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < bw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
       			weights[ind] <- pi/4 * cos(pi * x[ind]/(2 * bw))/bw
       			weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# bw <- sax[k] + 1e-06
         	  	# ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
    		}    
    		attributes(optcos) <- list(name = "optcosine", k = k, nn.only = TRUE, adaptive = TRUE)
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
			## window functions with fixed bandwidth
		    optcos <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
        		ifelse(abs(x) < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
        	}
    		attributes(optcos) <- list(name = "optcosine", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
		    optcos <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
    	        weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
       			weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
        		# weights
    		}    
    		attributes(optcos) <- list(name = "optcosine", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(optcos)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @family weights
#'
#' @rdname wfs

rectangular <- function(bw, k) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
    		rect <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		       	sax <- sort(ax)
       		   	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < bw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
       			weights[ind] <- 0.5/bw
       			weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# bw <- sax[k] + 1e-06
            	# ifelse(ax < bw, 0.5/bw, 0)
    		}    
    		attributes(rect) <- list(name = "rectangular", k = k, nn.only = TRUE, adaptive = TRUE)
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
			## window functions with fixed bandwidth
    		rect <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
        		ifelse(abs(x) < bw, 0.5/bw, 0)
        	}
    		attributes(rect) <- list(name = "rectangular", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		rect <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
    	        weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
       			weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
        		# weights
    		}    
    		attributes(rect) <- list(name = "rectangular", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(rect)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @family weights
#'
#' @rdname wfs

triangular <- function(bw, k) {
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
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
		    triangle <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		       	sax <- sort(ax)
       		   	bw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < bw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
           		weights <- numeric(length(ax))
       			weights[ind] <- (1 - ax[ind]/bw)/bw
       			weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# bw <- sax[k] + 1e-06
       		 	# ifelse(ax < bw, (1 - ax/bw)/bw, 0)
    		}
    		attributes(triangle) <- list(name = "triangular", k = k, nn.only = TRUE, adaptive = TRUE)
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
			## window functions with fixed bandwidth
		    triangle <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
       			ax <- abs(x)
        		ifelse(ax < bw, (1 - ax/bw)/bw, 0)
    		}
    		attributes(triangle) <- list(name = "triangular", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
		    triangle <- function(x) {
				if (any(x < 0))
					stop("'x' must be positive")
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    knnbw <- sax[k]/(1 - .Machine$double.neg.eps)   ## => sax[k]/bw = 1 - .Machine$double.neg.eps < 1 => sax[k] gets a small positiv weight
				ind <- ax < knnbw
				untied <- sum(ind)
				if (untied > k) {
					indTied <- ax == sax[k]
					s <- sample(which(indTied), size = untied - k)
					ind[s] <- FALSE
				}
       			weights <- numeric(length(ax))
            	weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
   				weights
				###
            	# ax <- abs(x)
	        	# sax <- sort(ax)
   		    	# knnbw <- sax[k] + 1e-06
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
        		# weights
    		}
    		attributes(triangle) <- list(name = "triangular", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(triangle)
}
