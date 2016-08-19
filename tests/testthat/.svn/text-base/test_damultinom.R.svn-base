context("damultinom")

test_that("damultinom: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(damultinom(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10, trace = FALSE))
	# wrong class
	# expect_error(damultinom(formula = iris, data = iris, wf = "gaussian", bw = 10, trace = FALSE))	#??? Sepal.Length becomes response
	# expect_error(damultinom(iris, data = iris, wf = "gaussian", bw = 10, trace = FALSE))			#??? Sepal.Length becomes response
	# target variable also in x
	expect_error(damultinom(iris$Species ~ iris, wf = "gaussian", bw = 10, trace = FALSE))			# 
	expect_warning(damultinom(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10, trace = FALSE))	## warning, Species on RHS removed
	## itr
	expect_that(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = -5, trace = FALSE), throws_error("'itr' must be >= 1"))
	expect_that(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = 0, trace = FALSE), throws_error("'itr' must be >= 1"))
})


test_that("damultinom throws a warning if grouping variable is numeric", {
	data(iris)
	# formula, data          
	expect_that(damultinom(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10, trace = FALSE), gives_warning("response variable was coerced to a factor"))
	# grouping, x
	expect_that(damultinom(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris, wf = "gaussian", bw = 10, trace = FALSE), gives_warning("response variable was coerced to a factor"))
})


test_that("dalda: training data from only one class", {
	data(iris)
	expect_that(damultinom(Species ~ ., data = iris, bw = 2, subset = 1:50, trace = FALSE), throws_error("need two or more classes to fit a damultinom model"))
})


test_that("damultinom: one training observation", {
	data(iris)
	# one training observation
	expect_that(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1, trace = FALSE), throws_error("need two or more classes to fit a damultinom model"))
	# one training observation in one predictor variable
	expect_that(damultinom(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = 1, trace = FALSE), throws_error("need two or more classes to fit a damultinom model"))
})


test_that("damultinom: initial weighting works correctly", {
	data(iris)
	## check if weighted solution with initial weights = 1 equals unweighted solution
	fit1 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, trace = FALSE)
	fit2 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(1,150), trace = FALSE)
	expect_equal(fit1[-16],fit2[-16])
	## returned weights	
	a <- rep(1,150)
	names(a) <- 1:150
	expect_equal(fit1$weights[[1]], a)
	expect_equal(fit1$weights, fit2$weights)
	## weights and subsetting
	# formula, data
	expect_that(fit <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 11:60, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	a <- rep(1,50)
	names(a) <- 11:60
	expect_equal(fit$weights[[1]], a)
	# formula, data, weights
	a <- rep(1:3,50)[11:60]
	a <- a/sum(a) * length(a)
	names(a) <- 11:60
	expect_that(fit <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(1:3, 50), subset = 11:60, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit$weights[[1]], a)
	## wrong specification of weights argument
	# weights in a matrix
	weight <- matrix(seq(1:150), nrow = 50)
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = weight, trace = FALSE))
	# weights < 0
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(-5, 150), trace = FALSE))
	# weights true/false
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = TRUE, trace = FALSE))
})


test_that("damultinom breaks out of for-loop if only one class is left", {
	expect_that(fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 30, trace = FALSE), gives_warning("for at least one class all weights are zero"))
	expect_equal(fit$itr, 3)
	expect_equal(length(fit$weights), 4)
	expect_that(fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 2, trace = FALSE), gives_warning("for at least one class all weights are zero"))
	expect_that(fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 2, trace = FALSE), gives_warning("training data from only one class, breaking out of iterative procedure")) ###
	expect_equal(fit$itr, 1)
	expect_equal(length(fit$weights), 2)
})
#sapply(fit$weights, function(x) return(list(sum(x[1:50]), sum(x[51:100]), sum(x[101:150]))))


test_that("damultinom: subsetting works", {
	data(iris)
	# formula, data
	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-16],fit2[-16])
	a <- rep(1,80)
	names(a) <- 1:80
	expect_equal(fit1$weights[[1]], a)
	# formula, data, weights
	expect_that(fit1 <- damultinom(Species ~ ., data = iris, weights = rep(1:3, each = 50), wf = "gaussian", bw = 2, subset = 1:80, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = iris[1:80,], weights = rep(1:3, each = 50)[1:80], wf = "gaussian", bw = 2, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-16],fit2[-16])
	a <- rep(80, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	b <- rep(1:3, each = 50)[1:80]
	b <- b/sum(b) * length(b)
	names(b) <- 1:80
	expect_equal(fit1$weights[[1]], b)
	# wrong specification of subset argument
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,], trace = FALSE))
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE, trace = FALSE))
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0, trace = FALSE))
	expect_error(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50, trace = FALSE))
})


test_that("damultinom: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50), trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	### NA in grouping
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50), trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	
	### NA in weights
	weights <- rep(1:3,50)
	weights[1:10] <- NA
	## formula, data, weights
	# na.fail
	expect_that(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, weights = weights, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_that(fit2 <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50), trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(fit1[-c(16, 31)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
})


test_that("damultinom: try all weight functions", {
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, trace = FALSE)    
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = gaussian(0.5), trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 50, trace = FALSE)    
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5, k = 50), trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
  	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 50), trace = FALSE)
  	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50), trace = FALSE)  
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = triangular(5, k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = biweight(5, k = 50), trace = FALSE)
  	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = optcosine(5, k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
  	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = cosine(5, k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
})


test_that("damultinom: local solution with rectangular window function and large bw and global solution coincide", {
  	library(nnet)
  	fit1 <- multinom(formula = Species ~ ., data = iris, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = rectangular(20), trace = FALSE)
	expect_equal(fit1[-c(16,18)], fit2[-c(16:24,26)])
	expect_equivalent(as.vector(fit1$weights), fit2$weights[[1]])
})


test_that("damultinom: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5, trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- damultinom(Species ~ ., data = iris, wf = gaussian(0.5), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])

	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5, trace = FALSE), gives_warning("argument 'bw' is ignored"))	
	fit2 <- damultinom(Species ~ ., data = iris, wf = gaussian(0.5), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30, trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- damultinom(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30, trace = FALSE), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- damultinom(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, trace = FALSE), gives_warning("argument 'bw' is ignored"))
	fit2 <- damultinom(Species ~ ., data = iris, wf = function(x) exp(-x), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# bw, k missing
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = gaussian(), trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = gaussian(), k = 10, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(damultinom(Species ~ ., data = iris, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5, trace = FALSE), throws_error("'bw' must be positive"))
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50, trace = FALSE), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(damultinom(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris)), trace = FALSE), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50, trace = FALSE), throws_error("'k' must be positive"))

	# k too small
	# expect_error(damultinom(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005, trace = FALSE))

	# k too large
	expect_that(damultinom(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50, trace = FALSE), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(damultinom(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris)), trace = FALSE), gives_warning("only first element of 'k' used"))
})


test_that("damultinom: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = rectangular(bw = 5), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "rectangular", k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = rectangular(k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	is.null(fit1$bw)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only not needed
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE, trace = FALSE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, function(x) sum(x > 0)), a)

	# adaptive bw, only knn
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = gaussian(k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	# adaptive bw, all obs
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, trace = FALSE)
	fit2 <- damultinom(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50), trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})


#=================================================================================================================
context("predict.damultinom")

test_that("predict.damultinom works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)  
  	pred <- predict(fit, newdata = iris[-ran,])
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])  	
})


test_that("predict.dalr: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- damultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
})


test_that("predict.damultinom works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100, trace = FALSE), gives_warning("group ‘virginica’ is empty"))
	expect_equal(ncol(fit$fitted.values), 1)
	expect_equal(ncol(fit$residuals), 1)
	expect_true(fit$entropy)
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(pred$class), 3)
	expect_equal(ncol(pred$posterior), 2)
})


test_that("predict.damultinom works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- damultinom(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
	expect_equal(fit$coefnames, c("(Intercept)", "Petal.Width"))
	pred <- predict(fit, newdata = iris[-ran,])
})


test_that("predict.damultinom works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1) 
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) <- "5"
	expect_equal(pred$class, a)
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred$class, a)
})	


test_that("predict.damultinom works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- damultinom(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
})

   
test_that("predict.damultinom: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran, trace = FALSE)
  	pred <- predict(fit, newdata = irisna)
	expect_equal(all(is.na(pred$class[1:17])), TRUE)
	expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
})


test_that("predict.damultinom: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- damultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
}) 

#=================================================================================================================

## eqivalence global and local with large bandwidth
# options(contrasts = c("contr.treatment", "contr.poly"))
# library(MASS)
# example(birthwt)
# (bwt.mu <- multinom(low ~ ., bwt))
# # Not run: Call:
# dg <- multinom(formula = low ~ ., data = bwt)
# d <- damultinom(formula = low ~ ., data = bwt, wf = "rectangular", bw = 10)


## plot
# irisscale <- data.frame(scale(iris[,1:4]), Species = iris$Species)
# d <- damultinom(formula = Species ~ Sepal.Length + Sepal.Width, data = irisscale, wf = "gaussian", bw = 0.5)
# plot(irisscale[,1:2], cex = d$weights[[4]], col = iris$Species)

# x <- seq(-3,3.5,0.05)
# y <- seq(-3,3,0.05)
# iris.grid <- expand.grid(Sepal.Length = x, Sepal.Width = y)
# pred <- predict(d, newdata = iris.grid)

# contour(x = x, y = y, z = matrix(pred$posterior[,1], length(x)))
# points(irisscale[,1:2], cex = d$weights[[4]], col = iris$Species)

# contour(x = x, y = y, z = matrix(pred$posterior[,2], length(x)))
# points(irisscale[,1:2], cex = d$weights[[4]], col = iris$Species)

# contour(x = x, y = y, z = matrix(pred$posterior[,3], length(x)))
# points(irisscale[,1:2], cex = d$weights[[4]], col = iris$Species)

