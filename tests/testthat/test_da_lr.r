context("dalr")

test_that("dalr: misspecified arguments", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	# wrong variable names
	expect_error(dalr(formula = Species ~ V1, data = iris2, wf = "gaussian", bw = 0.5))
	# wrong class
	expect_error(dalr(formula = iris2, data = iris2, wf = "gaussian", bw = 10))
	expect_error(dalr(iris2, data = iris2, wf = "gaussian", bw = 10))
	# target variable also in x
	#expect_error(dalr(Y = iris2$Species, X = iris2, wf = "gaussian", bw = 10))      ## system singular
	expect_warning(dalr(Species ~ Species + Petal.Width, data = iris2, wf = "gaussian", bw = 10))           ## warning, Species on RHS removed
	# missing x
	expect_error(dalr(Y = iris2$Species, wf = "gaussian", bw = 10))
	## itr
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, itr = -5))
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, itr = 0))
	## wrong method argument
	# missing quotes
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, method = ML))
	# method as vector
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, method = c("ML","unbiased")))
})


test_that("dalr throws a warning if grouping variable is numeric", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	# formula, data
	expect_that(dalr(formula = Sepal.Length ~ ., data = iris2, wf = "gaussian", bw = 10), gives_warning("'Y' was coerced to a factor"))
	expect_error(dalr(formula = Petal.Width ~ ., data = iris2, wf = "gaussian", bw = 10))  ## system singular
	# y, x
	expect_that(dalr(Y = iris2[,1], X = iris2[,-1], wf = "gaussian", bw = 10), gives_warning("'Y' was coerced to a factor"))
	expect_error(dalr(Y = iris2[,4], X = iris2[,-1], wf = "gaussian", bw = 10))     ## system singular
	expect_warning(dalr(Y = iris2$Petal.Width, X = iris2[,-5], wf = "gaussian", bw = 10))
})


test_that("dalr works if only one predictor variable is given", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	expect_that(fit <- dalr(Species ~ Petal.Width, data = iris2, wf = "gaussian", bw = 5), gives_warning("non-integer #successes in a binomial glm!"))
#	expect_equal(ncol(fit$means), 1)	
#	expect_equal(dim(fit$cov), rep(1, 2))	
})


test_that("dalr: detectig singular covariance matrix works", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	# one training observation
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 1))            ## system singular	
	# one training observation in one predictor variable
	expect_error(dalr(Species ~ Petal.Width, data = iris2, wf = "gaussian", bw = 1, subset = 1))   ## system singular
})


test_that("dalr: initial weighting works correctly", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))                 #rownames(iris2) <- 1:100
	## check if weighted solution with initial weights = 1 equals unweighted solution
	fit1 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2)
	fit2 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, weights = rep(1,100))
	expect_equal(fit1[-c(21,22)],fit2[-c(21,22)])
	## returned weights	
	a <- rep(1,100)
  	names(a) <- 51:150
  	expect_equal(fit1$prior.weights[[1]], a)
	expect_equal(fit1$prior.weights, fit2$prior.weights)
	## weights and subsetting
	# formula, data
	fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = 11:60)
	a <- rep(1,50)
  	names(a) <- 61:110
  	expect_equal(fit$prior.weights[[1]], a)                  
	# formula, data, weights
	# a <- rep(1:2,50)[11:60]                                           
	# a <- a/sum(a) * length(a)
	# names(a) <- 61:110
	# fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, weights = rep(1:2, 50), subset = 11:60)      
	# expect_equal(fit$prior.weights[[1]], a)                          
	# x, y
	a <- rep(1,50)
  	names(a) <- 61:110
  	fit <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 2, subset = 11:60)
	expect_equal(fit$prior.weights[[1]], a)	              
	# x, y, weights
	# a <- rep(1:2,50)[11:60]                                           
	# a <- a/sum(a) * length(a)
	# names(a) <- 61:110
	# fit <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 2, weights = rep(1:2, 50), subset = 11:60)   
	# expect_equal(fit$prior.weights[[1]], a)
	## wrong specification of weights argument
	# weights in a matrix
	weight <- matrix(seq(1:100), nrow = 50)
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, weights = weight))
	# weights < 0
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, weights = rep(-5, 100)))
	# weights true/false
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, weights = TRUE))
})



#test_that("dalr breaks out of for-loop if only one class is left", {
#  data(iris)
#  iris2 <- iris[c(51:150),]
#  iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
#  expect_that(fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 1:50), gives_warning(c("group virginica is empty", "Warnings about non-integer #successes in a binomial glm are expected")))
#	expect_equal(fit$itr, 1)
#	expect_equal(length(fit$weights), 1)
#})

test_that("dalr: subsetting works", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	# formula, data
	a <- rep(1,80)
  	names(a) <- 51:130
  	fit1 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = 1:80)         
	fit2 <- dalr(Species ~ ., data = iris2[1:80,], wf = "gaussian", bw = 2)
	expect_equal(fit1[-c(21,22,25)],fit2[-c(21,22,25)])                                          
	expect_equal(fit1$prior.weights[[1]], a)
	# formula, data, weights
	# fit1 <- dalr(Species ~ ., data = iris2, weights = rep(1:2, each = 50), wf = "gaussian", bw = 2, subset = 1:80)
	# fit2 <- dalr(Species ~ ., data = iris2[1:80,], weights = rep(1:2, each = 50)[1:80], wf = "gaussian", bw = 2)
	# expect_equal(fit1[-c(21,22,25)],fit2[-c(21,22,25)])
	# a <- rep(80, 4)
	# names(a) <- 0:3
	# expect_equal(sapply(fit1$prior.weights, length), a)
	# b <- rep(1:2, each = 50)[1:80]
	# b <- b/sum(b) * length(b)
	# expect_equal(fit1$prior.weights[[1]], b)
  	# x, y
	a <- rep(1,80)
	names(a) <- 51:130
  	fit1 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 2, subset = 1:80)
	fit2 <- dalr(Y = iris2$Species[1:80], X = iris2[1:80,-5], wf = "gaussian", bw = 2)
	expect_equal(fit1[-c(21,22,25)],fit2[-c(21,22,25)])
	expect_equal(fit1$prior.weights[[1]], a)
	# x, y, weights
	# fit1 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 2, weights = rep(1:3, each = 50), subset = 1:80)
	# fit2 <- dalr(Y = iris2$Species[1:80], X = iris2[1:80,-5], wf = "gaussian", bw = 2, weights = rep(1:3, each = 50)[1:80])
	# expect_equal(fit1[-c(21,22,25)],fit2[-c(21,22,25)])
	# a <- rep(80, 4)     
	# names(a) <- 0:3
	# expect_equal(sapply(fit1$prior.weights, length), a)
	# b <- rep(1:3, each = 50)[1:80]
	# b <- b/sum(b) * length(b)
	# expect_equal(fit1$prior.weights[[1]], b)
	# wrong specification of subset argument
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = iris[1:10,]))
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = FALSE))
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 0))
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = -10:50))
})

test_that("dalr: NA handling works correctly", {
	### NA in x
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	irisna <- iris2
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_error(dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(21, 22, 23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## formula, data, weights
	# na.fail
	expect_error(dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:2, 50))
	expect_equal(fit1[-c(21, 22, 23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
  ## x, y
	# na.fail
	expect_error(dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 2, subset = 6:60, na.action = na.omit)      
	fit2 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 2, subset = 11:60)                           
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	
	## x, y, weights
	# na.fail
	expect_error(dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.omit)
	fit2 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:2, 50))
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	
	### NA in y
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
  irisna <- iris2
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_error(dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## formula, data, weights
	# na.fail
	expect_error(dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:2, 50))
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## x, y
	# na.fail
	expect_error(dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## x, grouping, weights
	# na.fail
	expect_error(dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:2, 50), na.action = na.omit)
	fit2 <- dalr(Y = irisna$Species, X = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:2, 50))
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
  
	### NA in weights
	weights <- rep(1:2,50)
	weights[1:10] <- NA
	## formula, data, weights
	# na.fail
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 11:60, weights = weights)
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## x, y, weights
	# na.fail
	expect_error(dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit)
	fit2 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = weights)
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## formula, data, weights
	# na.fail
	expect_error(dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:2, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:2, 50), na.action = na.omit)
	fit2 <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:2, 50))
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## x, y
	# na.fail
	expect_error(dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit)
	fit2 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
	## x, y, weights
	# na.fail
	expect_error(dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = subset, weights = rep(1:2, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = subset, weights = rep(1:2, 50), na.action = na.omit)
	fit2 <- dalr(Y = iris2$Species, X = iris2[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:2, 50))
	expect_equal(fit1[-c(21,22,23)], fit2[-c(21,22,23)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, length), a)
})

test_that("dalr: try all weight functions", {
	data(iris)
  iris2 <- iris[c(51:150),]
  iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 0.5)    
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = gaussian(0.5))    
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 0.5)    
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = gaussian(0.5))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:20,23:40)], fit4[c(1:20,23:40)])
	
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 0.5, k = 30)    
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = gaussian(bw = 0.5, k = 30))    
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 0.5, k = 30)    
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = gaussian(0.5, 30))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)
	
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "epanechnikov", bw = 5, k = 30)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = epanechnikov(bw = 5, k = 30))
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "epanechnikov", bw = 5, k = 30)
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = epanechnikov(5, 30))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "rectangular", bw = 5, k = 30)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = rectangular(bw = 5, k = 30))
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "rectangular", bw = 5, k = 30)
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = rectangular(5, 30))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "triangular", bw = 5, k = 30)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = triangular(5, k = 30))
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "triangular", bw = 5, k = 30)
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = triangular(5, 30))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "biweight", bw = 5, k = 30)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = biweight(5, k = 30))
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "biweight", bw = 5, k = 30)
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = biweight(5, 30))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "optcosine", bw = 5, k = 30)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = optcosine(5, k = 30))
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "optcosine", bw = 5, k = 30)
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = optcosine(5, 30))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "cosine", bw = 5, k = 30)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = cosine(5, k = 30))
	fit3 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "cosine", bw = 5, k = 30)
	fit4 <- dalr(X = iris2[,-5], Y = iris2$Species, wf = cosine(5, 30))    
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit3[-c(21,22)], fit4[-c(21,22)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)
})


test_that("dalr: local solution with rectangular window function and large bw and global solution coincide", {
  data(iris)
  iris2 <- iris[c(51:150),]
  iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	fit1 <- glm(formula = Species ~ ., data = iris2, family = binomial())
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = rectangular(20))
	expect_equal(fit1[-c(14,15,21,22,28)], fit2[-c(14,15,21,22,28,31:40)])
	expect_equal(fit1$prior.weights, fit2$prior.weights[[1]])
})


test_that("dalr: arguments related to weighting misspecified", {
	data(iris)
  iris2 <- iris[c(51:150),]
  iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
  # bw, k not required
	expect_that(fit1 <- dalr(Species ~ ., data = iris2, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning("argument 'k' is ignored")) 
  expect_that(fit1 <- dalr(Species ~ ., data = iris2, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning("argument 'bw' is ignored"))   
	fit2 <- dalr(Species ~ ., data = iris2, wf = gaussian(0.5))                                                
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])

	expect_that(fit1 <- dalr(Species ~ ., data = iris2, wf = gaussian(0.5), bw = 0.5), gives_warning("argument 'bw' is ignored"))	
	fit2 <- dalr(Species ~ ., data = iris2, wf = gaussian(0.5))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- dalr(Species ~ ., data = iris2, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning("argument 'k' is ignored"))
  expect_that(fit1 <- dalr(Species ~ ., data = iris2, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning("argument 'bw' is ignored"))
	expect_that(fit2 <- dalr(Species ~ ., data = iris2, wf = function(x) exp(-x), k = 30), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- dalr(Species ~ ., data = iris2, wf = function(x) exp(-x), bw = 0.5), gives_warning("argument 'bw' is ignored"))
	fit2 <- dalr(Species ~ ., data = iris2, wf = function(x) exp(-x))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = gaussian)) ## error because length(weights) and nrow(x) are different

	# bw, k missing
	expect_that(dalr(formula = Species ~ ., data = iris2, wf = gaussian()), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(dalr(formula = Species ~ ., data = iris2, wf = gaussian(), k = 10), throws_error("either 'bw' or 'k' have to be specified"))
	expect_error(dalr(Species ~ ., data = iris2))
	
	# bw < 0
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = -5))
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = "cosine", k = 10, bw = -50))
	
	# bw vector
	expect_that(dalr(formula = Species ~., data = iris2, wf = "gaussian", bw = rep(1, nrow(iris))), gives_warning("only first element of 'bw' used"))

	# k < 0
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = "gaussian", k =-7, bw = 50))

	# k too small
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = "gaussian", k = 5, bw = 0.005))

	# k too large
	expect_error(dalr(formula = Species ~ ., data = iris2, k = 250, wf = "gaussian", bw = 50))

	# k vector
	expect_that(dalr(formula = Species ~., data = iris2, wf = "gaussian", k = rep(50, nrow(iris))), gives_warning("only first element of 'k' used"))
})


test_that("dalr: weighting schemes work", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
  ## wf with finite support
	# fixed bw
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "rectangular", bw = 5)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = rectangular(bw = 5))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "rectangular", k = 50)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = rectangular(k = 50))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	is.null(fit1$bw)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "rectangular", bw = 5, k = 50)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = rectangular(bw = 5, k = 50))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only not needed
	expect_that(dalr(formula = Species ~ ., data = iris2, wf = "rectangular", bw = 5, nn.only = TRUE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE))
	
	## wf with infinite support
	# fixed bw
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 0.5)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = gaussian(bw = 0.5))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)
	a <- rep(100, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, function(x) sum(x > 0)), a)

	# adaptive bw, only knn
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", k = 50)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = gaussian(k = 50))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)

	# adaptive bw, all obs
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", k = 50, nn.only = FALSE)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = gaussian(k = 50, nn.only = FALSE))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)
	a <- rep(100, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$prior.weights, function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 1, k = 50)
	fit2 <- dalr(formula = Species ~ ., data = iris2, wf = gaussian(bw = 1, k = 50))
	expect_equal(fit1[-c(21,22)], fit2[-c(21,22)])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$prior.weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_error(dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE))
})	


#=================================================================================================================
context("predict.dalr")

test_that("predict.dalr works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
  	ran <- sample(1:100,60)
	## formula, data
	fit <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris2)[ran])  	
  	expect_equal(names(pred$class), rownames(iris2)[ran])  	
	## formula, data, newdata
	fit <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit, newdata = iris2[-ran,])
  	expect_equal(rownames(pred$posterior), rownames(iris2)[-ran])  	
  	expect_equal(names(pred$class), rownames(iris2)[-ran])  	
	## Y, x
	fit <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris2)[ran])  	
  	expect_equal(names(pred$class), rownames(iris2)[ran])  	
	## Y, x, newdata
	fit <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit, newdata = iris2[-ran,-5])          ### To Do !!!
  	expect_equal(rownames(pred$posterior), rownames(iris2)[-ran])  	
  	expect_equal(names(pred$class), rownames(iris2)[-ran])  	
})


test_that("predict.dalr: retrieving training data works", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	## no subset
	# formula, data
	fit <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 2)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris2)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 2)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris2[,-5])
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- dalr(formula = Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = ran)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris2[ran,])
  	expect_equal(pred1, pred2)
	# y, x
	fit <- dalr(X = iris2[,-5], Y = iris2$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris2[ran,-5])
  	expect_equal(pred1, pred2)
})


#test_that("predict.dalr works with missing classes in the training data", {
#	data(iris)
#	iris2 <- iris[c(51:150),]
#	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
#	ran <- sample(51:150,60)
#	expect_that(fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 10, subset = 51:100), gives_warning("group versicolor is empty"))
#	expect_equal(length(fit$prior), 2)
#	a <- rep(50, 2)
#	names(a) <- names(fit$counts)
#	expect_equal(fit$counts, a)
#	expect_equal(fit$N, 100)
#	expect_equal(nrow(fit$means), 2)
#	pred <- predict(fit, newdata = iris2[-ran,])
#	expect_equal(nlevels(pred$class), 3)
#	expect_equal(ncol(pred$posterior), 2)
#	# a <- rep(0,50)
#	# names(a) <- rownames(pred$posterior)
#	# expect_equal(pred$posterior[,3], a)
#})

	
test_that("predict.dalr works with one single predictor variable", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	ran <- sample(1:100,60)
	fit <- dalr(Species ~ Petal.Width, data = iris2, wf = "gaussian", bw = 2, subset = ran)
#	expect_equal(ncol(fit$means), 1)
#	expect_equal(dim(fit$cov), rep(1, 2))
	predict(fit, newdata = iris2[-ran,])
})


test_that("predict.dalr works with one single test observation", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
  ran <- sample(1:100,60)
	fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit, newdata = iris2[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 2))
	a <- factor("versicolor", levels = c("versicolor", "virginica"))
	names(a) = "55"
	expect_equal(pred$class, a)
	pred <- predict(fit, newdata = iris2[58,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 2))
	a <- factor("virginica", levels = c("versicolor", "virginica"))
	names(a) = "108"
	expect_equal(pred$class, a)
})	


test_that("predict.dalr works with one single predictor variable and one single test observation", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	ran <- sample(1:100,60)
	fit <- dalr(Species ~ Petal.Width, data = iris2, wf = "gaussian", bw = 2, subset = ran)
#	expect_equal(ncol(fit$means), 1)
#	expect_equal(dim(fit$cov), rep(1, 2))
	pred <- predict(fit, newdata = iris2[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 2))
})

   
test_that("predict.dalr: NA handling in newdata works", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
  	ran <- sample(1:100,60)
	irisna <- iris2
	irisna[1:17,c(1,3)] <- NA
	fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 50, subset = ran)
	pred <- predict(fit, newdata = irisna)
	expect_equal(all(is.na(pred$class[1:17])), TRUE)
	expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
})


test_that("predict.dalr: misspecified arguments", {
	data(iris)
	iris2 <- iris[c(51:150),]
	iris2$Species <- factor(iris2$Species, levels = c("versicolor", "virginica"))
	ran <- sample(1:100,60)
	fit <- dalr(Species ~ ., data = iris2, wf = "gaussian", bw = 2, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
    # errors in prior
#    expect_error(predict(fit, prior = rep(2,length(levels(iris2$Species))), newdata = iris2[-ran,]))
#    expect_error(predict(fit, prior = TRUE, newdata = iris2[-ran,]))
#    expect_error(predict(fit, prior = 0.6, newdata = iris2[-ran,]))
})  	



#=================================================================================================================

# test.dalr <- function() {
	# data(iris)
	
	# ## number of classes larger than 2
	# checkException(dalr(Species ~ ., data = iris, wf = "gaussian",bw = 50, thr = 0.3))

	# iris <- iris[1:100,]
	# iris$Species <- factor(iris$Species, levels = c("setosa", "versicolor"))

	# # only 1 class
	# #dalr(Species ~ ., data = iris, wf = "gaussian",bw = 50, thr = 0.3, subset = 1:50) ## warning
	
	# ## formula
	# # wrong variable names
	# checkException(dalr(Species ~ V1, data = iris, wf = "gaussian",bw = 50, thr = 0.3))

	# # numeric grouping variable
	# checkException(dalr(formula = Petal.Width ~ ., data = iris, wf = "gaussian", bw = 50, thr = 0.3))

	# # wrong class
	# checkException(dalr(formula = iris, data = iris))

	# ## data.frame/matrix
	# # numeric grouping variable/number of classes not 2
	# checkException(dalr(X = iris[,-1], Y = iris[,1], bw = 50))         

	# # target variable also in x
	# checkException(dalr(X = iris, Y = iris$Species, wf = "gaussian", bw = 50 ))         ## todo: works, but should not

	# # missing x
	# checkException(dalr(Y = iris$Species))

	# ## subset
	# # wrong class
	# checkException(dalr(Species ~ ., data = iris, bw = 5, subset = iris[1:10,]))
	# checkException(dalr(Species ~ ., data = iris, bw = 5, subset = FALSE))
	# # nonsensical indices
	# checkException(dalr(Species ~ ., data = iris, bw = 5, subset = -10:50))

	# ## na.action
	# irisna <- iris
	# irisna[1:10,c(1,3)] <- NA	
	# # default na.omit
	# dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 0.5)
	# # na.fail
	# checkException(dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 1, na.action = na.fail))
	# # check if na.omit works correctly
	# fit1 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 1, na.action = na.omit)
	# fit2 <- dalr(Species ~ ., data = irisna, wf = "gaussian", bw = 1, subset = 11:100)
	# all.equal(fit1[-c(21:23)], fit2[-c(21:22)])
	# all.equal(fit1[[21]][1:5], fit2[[21]][1:5])
	# all.equal(attributes(fit1[[21]])[1:4], attributes(fit2[[21]])[1:4])
	
	# # one predictor variable
	# dalr(Species ~ Petal.Width, data = iris, k = 60)
	
	# # one training observation -> one class
	# checkException(dalr(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1)) ### glm.fit: Indizierung außerhalb der Grenzen???
	
	# # one training observation in one predictor variable
	# checkException(dalr(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = 1))  ### glm.fit: Indizierung außerhalb der Grenzen???

	# # itr
	# checkException(dalr(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = -5))
	# checkException(dalr(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = 0))
	
	# # bw not necessary
	# #dalr(Species ~ ., data = iris, bw = 0.5, k = 30)
	# fit1 <- dalr(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5)  ## warning
	# fit2 <- dalr(Species ~ ., data = iris, wf = gaussian(0.5), k = 30)
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5)          ## warning
	# fit2 <- dalr(Species ~ ., data = iris, wf = gaussian(0.5))
	# all.equal(fit1[-22], fit2[-22])
	# #fit1$k == nrow(iris)

	# fit1 <- dalr(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30)  ## warning
	# fit2 <- dalr(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30)
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5)  ## warning
	# fit2 <- dalr(Species ~ ., data = iris, wf = function(x) exp(-x))
	# all.equal(fit1[-22], fit2[-22])
	# #fit1$k == nrow(iris)

	# # missing quotes
	# checkException(dalr(formula = Species ~ ., data = iris, wf = gaussian, bw = 50)) ### todo: error message not understandable

	# # bw missing
	# checkException(dalr(formula = Species ~ ., data = iris, wf = "gaussian"))
	# #checkException(dalr(formula = Species ~ ., data = iris, wf = "gaussian", k = 50))
	# checkException(dalr(formula = Species ~ ., data = iris, wf = gaussian()))
	# checkException(dalr(formula = Species ~ ., data = iris, wf = gaussian(), k = 10))
	# # bw < 0
	# checkException(dalr(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5))
	# checkException(dalr(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50))
	# # bw vector
	# dalr(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris)))      ## warning
	
	# # k missing
	# #dalr(Species ~ ., data = iris) ## warning
	# #dalr(Species ~ ., data = iris, bw = 0.5) ## warning

	# # k < 0
	# checkException(dalr(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50))
	# # k too large
	# checkException(dalr(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50))
	# # k vector
	# dalr(formula = Species ~., data = iris, wf = "rectangular", k = rep(50, nrow(iris)))          ## warning

	# # try all available weight functions
	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)    
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = gaussian(0.5))    
	# all.equal(fit1[-22], fit2[-22])
	# #fit1$k == nrow(iris)
	
	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 30)    
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5, k = 30))    
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 0.5, k = 30)
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 0.5, k = 30))
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "rectangular", bw = 0.5, k = 30)
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = rectangular(bw = 0.5, k = 30))
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "triangular", bw = 0.5, k = 30)
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = triangular(bw = 0.5, k = 30))
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "biweight", bw = 0.5, k = 30)
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = biweight(bw = 0.5, k = 30))
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "optcosine", bw = 0.5, k = 30)
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = optcosine(bw = 0.5, k = 30))
	# all.equal(fit1[-22], fit2[-22])

	# fit1 <- dalr(formula = Species ~ ., data = iris, wf = "cosine", bw = 0.5, k = 30)
	# fit2 <- dalr(formula = Species ~ ., data = iris, wf = cosine(bw = 1, k = 30), bw = 0.5)
	# all.equal(fit1[-22], fit2[-22])

	# dalr(formula = Species ~ ., data = iris, wf = "none", k = 30)
	# dalr(formula = Species ~ ., data = iris, k = 30)
	
	# # individual weight functions
	# dalr(Species ~ ., data = iris, wf = function(x) exp(-x))
	# dalr(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30)
	
	# # wrong weight functions
	# checkException(dalr(Species ~ ., data = iris, wf = TRUE))
	# checkException(dalr(Species ~ ., data = iris, wf = rep(-5, 100)))
	# checkException(dalr(Species ~ ., data = iris, wf = "iris"))
	  
	# ## check if glm with family="binomial" equals dalr
	# l1 <- glm(Species ~ ., data = iris, family = "binomial", x = FALSE, y = FALSE)
	# l2 <- dalr(Species ~ ., data = iris, wf = "none", k = 100, x = FALSE, y = FALSE)
	# checkEquals(l1[-c(21,27)],l2[-c(21,27, 30:35)])                                
	
	# l1 <- glm(Species ~ ., data = iris, family = "binomial", x = TRUE, y = FALSE)
	# l2 <- dalr(Species ~ ., data = iris, wf = "none", k = 100, x = TRUE, y = FALSE)
	# checkEquals(l1[-c(22, 28)],l2[-c(22,28, 31:36)])                                    

	# l1 <- glm(Species ~ ., data = iris, family = "binomial", x = FALSE, y = TRUE)
	# l2 <- dalr(Species ~ ., data = iris, wf = "none", k = 100, x = FALSE, y = TRUE)
	# checkEquals(l1[-c(22, 28)],l2[-c(22,28, 31:36)])                                    

	# l1 <- glm(Species ~ ., data = iris, family = "binomial", x = TRUE, y = TRUE)
	# l2 <- dalr(Species ~ ., data = iris, wf = "none", k = 100, x = TRUE, y = TRUE)
	# checkEquals(l1[-c(23, 29)],l2[-c(23,29, 32:37)])       
	
	# ## initial weights
	# fit <- dalr(Species ~ ., data = iris, wf = "gaussian", k = 70, weights = rep(1:2, 50))
	# fit$weights   
	# fit$prior.weights                                  

# }


# test.predict.dalr <- function(){
	
	# data(iris)
	
	# iris <- iris[1:100,]
	# iris$Species <- factor(iris$Species, levels = c("setosa", "versicolor"))

	# ran <- sample(1:100,50)

	# fit <- dalr(formula = Species ~ ., data = iris, k = 50, subset = ran)  
  	# predict(fit, newdata= iris[-ran,])
  	
  	# # missing classes
	# fit <- dalr(Species ~ ., data = iris, k = 50, subset = 1:50) ## warning
	# p <- predict(fit, newdata = iris[-ran,])
	# nlevels(p$class) == 2
	# ncol(p$posterior) == 1
	# ## levels verschwunden???

  	
  	  
  	# # one predictor variable
	# fit <- dalr(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = ran)
	# predict(fit, newdata = iris[-ran,])
	
	# # one predictor variable and one test observation
	# fit <- dalr(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = ran)
	# predict(fit, newdata = iris[5,])
	
    # # one test observation
  	# predict(fit, newdata = iris[5,])
  	# predict(fit, newdata = iris[58,])
  
  	# # errors in newdata
  	# checkException(predict(fit, newdata = TRUE))
  	# checkException(predict(fit, newdata = -50:50)) 
  
    # # try se.fit and dispersion          
	# fit <- dalr(formula = Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran)  
	# predict(fit, newdata = iris[10,], se.fit = TRUE)
    # predict(fit, newdata = iris[10,], se.fit = TRUE, dispersion = 20)

	# ## todo: test further arguments to predict
	
	# # NA in newdata
	# irisna <- iris
	# irisna[1:17,c(1,3)] <- NA
  	# fit <- dalr(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran)
	# predict(fit, newdata = irisna)  ### todo: warning if NAs in newdata
# }
