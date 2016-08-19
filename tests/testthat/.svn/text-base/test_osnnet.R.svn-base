context("osnnet")


class.ind <- function(cl) {
    n <- length(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}


test_that("osnnet: reps argument works", {
	## formula, data
	fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 2, size = 2, trace = FALSE)
	expect_equal(fit$reps, 1)
	predict(fit)
	predict(fit, newdata = iris[1,])
	fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 2, size = 2, trace = FALSE, reps = 3)
	expect_equal(fit$reps, 3)
	predict(fit)
	predict(fit, newdata = iris[1,])
	## x, y	
	fit <- osnnet(y = class.ind(iris$Species), x = as.matrix(iris[,-5]), wf = "gaussian", bw = 2, size = 2, trace = FALSE)
	expect_equal(fit$reps, 1)
	predict(fit)
	predict(fit, newdata = iris[1,-5])
	fit <- osnnet(y = class.ind(iris$Species), x = as.matrix(iris[,-5]), wf = "gaussian", bw = 2, size = 2, trace = FALSE, reps = 3)
	expect_equal(fit$reps, 3)
	predict(fit)
	predict(fit, newdata = iris[1,-5])
})


test_that("osnnet: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(osnnet(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	# wrong class
	expect_error(osnnet(formula = iris, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	#expect_error(osnnet(iris, data = iris, wf = "gaussian", bw = 10, size = 2,  trace = FALSE))
	# target variable also in x
	fit <- osnnet(y = class.ind(iris$Species), x = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE) ## todo!!!
	expect_warning(predict(fit))
	expect_warning(osnnet(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))           ## warning, Species on RHS removed
	# missing x
	expect_error(osnnet(y = class.ind(iris$Species), wf = "gaussian", bw = 10, size = 2, trace = FALSE))
})


# test_that("osnnet throws a warning if y variable is numeric", {
	# data(iris)
	# # formula, data
	# expect_that(osnnet(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10, size = 2,  trace = FALSE), gives_warning("'y' was coerced to a factor"))
	# # y, x
	# expect_that(osnnet(y = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10, size = 2,  trace = FALSE), gives_warning("'y' was coerced to a factor"))
# })


test_that("osnnet works if only one predictor variable is given", {
	data(iris)
	fit <- osnnet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5, size = 2,  trace = FALSE)
	predict(fit)
})


test_that("osnnet: training data from only one class", {
	data(iris)
	expect_that(osnnet(Species ~ ., data = iris, bw = 2, subset = 1:50, size = 2, trace = FALSE), throws_error("'softmax = TRUE' requires at least two response categories"))
	expect_that(osnnet(Species ~ ., data = iris, bw = 2, subset = 1, size = 2, trace = FALSE), throws_error("'softmax = TRUE' requires at least two response categories"))
	expect_that(osnnet(y = class.ind(iris$Species)[1:50,1], x = iris[1:50,-5], bw = 2, softmax = TRUE, size = 2, trace = FALSE), throws_error("'softmax = TRUE' requires at least two response categories"))
	expect_that(osnnet(y = class.ind(iris$Species)[1,1], x = iris[1,-5], bw = 2, softmax = TRUE, size = 2, trace = FALSE), throws_error("'softmax = TRUE' requires at least two response categories"))
	#expect_that(osnnet(y = class.ind(iris$Species)[1:50,1], x = iris[1:50,-5], bw = 2, entopy = TRUE, size = 2, trace = FALSE), throws_error("'softmax = TRUE' requires at least two response categories"))
	#expect_that(osnnet(y = class.ind(iris$Species)[1,1], x = iris[1,-5], bw = 2, entropy = TRUE, size = 2, trace = FALSE), throws_error("'softmax = TRUE' requires at least two response categories")) ## works...
})


test_that("osnnet: subsetting works", {
	data(iris)
	# formula, data
	expect_that(fit1 <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osnnet(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-28],fit2[-28])
	# x, y
#	expect_that(fit1 <- osnnet(y = class.ind(class.ind(iris$Species)), x = iris[,-5], wf = "gaussian", bw = 2, subset = 1:80, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
#	expect_that(fit2 <- osnnet(y = class.ind(class.ind(iris$Species))[1:80],, x = iris[1:80,-5], wf = "gaussian", bw = 2, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
#	expect_equal(fit1[-27],fit2[-27])
	# wrong specification of subset argument
	expect_error(osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,], size = 2, trace = FALSE))
	expect_error(fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE, size = 2, trace = FALSE))
	expect_error(fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0, size = 2, trace = FALSE))	
	expect_error(osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50, size = 2, trace = FALSE))
})


test_that("osnnet: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(osnnet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- osnnet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osnnet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(28, 31)], fit2[-28])

	## x, y
	# na.fail
	# expect_that(osnnet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# # check if na.omit works correctly
	# expect_that(fit1 <- osnnet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	# expect_that(fit2 <- osnnet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	# expect_equal(fit1[-c(28, 31)], fit2[-28])
	
	### NA in y
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(osnnet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- osnnet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osnnet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(28, 31)], fit2[-28])
	## x, y
	# # na.fail
	# expect_that(osnnet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# # check if na.omit works correctly
	# expect_that(fit1 <- osnnet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	# expect_that(fit2 <- osnnet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	# expect_equal(fit1[-c(28, 31)], fit2[-28])

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(28,31)], fit2[-28])
	## x, y
	# na.fail
	# expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# # check if na.omit works correctly
	# expect_that(fit1 <- osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	# expect_that(fit2 <- osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	# expect_equal(fit1[-c(28,31)], fit2[-28])
})


test_that("osnnet: try all weight functions", {
	Wts = runif(n = 19, -0.5, 0.5)

	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5, Wts = Wts, size = 2, trace = FALSE)    
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = gaussian(5), Wts = Wts, size = 2, trace = FALSE)    
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "gaussian", bw = 5, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = gaussian(5), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4) ## rownames fehlen bei matrix method
		
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5, k = 30, Wts = Wts, size = 2, trace = FALSE)    
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = gaussian(bw = 5, k = 30), Wts = Wts, size = 2, trace = FALSE)    
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "gaussian", bw = 5, k = 30, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = gaussian(5, 30), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)
	
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 30, Wts = Wts, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 30), Wts = Wts, size = 2, trace = FALSE)
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "epanechnikov", softmax = TRUE, Wts = Wts, bw = 5, k = 30, size = 2, trace = FALSE)
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = epanechnikov(5, 30), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 30, Wts = Wts, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 30), Wts = Wts, size = 2, trace = FALSE)
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "rectangular", bw = 5, k = 30, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = rectangular(5, 30), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 30, Wts = Wts, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = triangular(5, k = 30), Wts = Wts, size = 2, trace = FALSE)
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "triangular", bw = 5, k = 30, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = triangular(5, 30), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, Wts = Wts, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = biweight(5), Wts = Wts, size = 2, trace = FALSE)
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "biweight", bw = 5, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = biweight(5), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 30, Wts = Wts, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = optcosine(5, k = 30), Wts = Wts, size = 2, trace = FALSE)
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "optcosine", bw = 5, k = 30, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = optcosine(5, 30), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30, Wts = Wts, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = cosine(5, k = 30), Wts = Wts, size = 2, trace = FALSE)
	fit3 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "cosine", bw = 5, k = 30, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	fit4 <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = cosine(5, 30), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit3[-c(22,28)], fit4[-c(22,28)])
	expect_equal(fit2[-c(1:2,28:31)], fit4[-c(1:2,28)])
	expect_equivalent(fit2[1], fit4[1])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1)
	set.seed(120)
	pred2 <- predict(fit2)
	set.seed(120)
	pred3 <- predict(fit3)
	set.seed(120)
	pred4 <- predict(fit4)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)
})


test_that("osnnet: local solution with rectangular window function and large bw and global solution coincide", {
	library(nnet)
	Wts = runif(n = 19, -0.5, 0.5)	
	## formula
	fit1 <- nnet(formula = Species ~ ., data = iris, Wts = Wts, size = 2, trace = FALSE)
	pred1 <- predict(fit1)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = rectangular(8), Wts = Wts, size = 2, trace = FALSE)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)
	## matrix
	fit3 <- nnet(y = class.ind(iris$Species), x = iris[,-5], softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	pred3 <- predict(fit3)
	fit4 <- osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = rectangular(8), softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
	pred4 <- predict(fit4)
	expect_equivalent(pred3, pred4)
})


test_that("osnnet: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- osnnet(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5, size = 2, trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- osnnet(Species ~ ., data = iris, wf = gaussian(0.5), size = 2, trace = FALSE)
	expect_equal(fit1[-28], fit2[-28])

	expect_that(fit1 <- osnnet(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5, size = 2, trace = FALSE), gives_warning("argument 'bw' is ignored"))	
	fit2 <- osnnet(Species ~ ., data = iris, wf = gaussian(0.5), size = 2, trace = FALSE)
	expect_equal(fit1[-28], fit2[-28])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- osnnet(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30, size = 2, trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- osnnet(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30, size = 2, trace = FALSE), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-28], fit2[-28])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- osnnet(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, size = 2, trace = FALSE), gives_warning("argument 'bw' is ignored"))
	fit2 <- osnnet(Species ~ ., data = iris, wf = function(x) exp(-x), size = 2, trace = FALSE)
	expect_equal(fit1[-28], fit2[-28])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	fit <- osnnet(formula = Species ~ ., data = iris, wf = gaussian, size = 2, trace = FALSE) ## error because length(weights) and nrow(x) are different
	expect_error(predict(fit))
	
	# bw, k missing
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = gaussian(), size = 2, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = gaussian(), k = 10, size = 2, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(osnnet(Species ~ ., data = iris, size = 2, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5, size = 2, trace = FALSE), throws_error("'bw' must be positive"))
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50, size = 2, trace = FALSE), throws_error("'bw' must be positive"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = -5, softmax = TRUE, size = 2, trace = FALSE), throws_error("'bw' must be positive"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "cosine", k = 10, bw = -50, softmax = TRUE, size = 2, trace = FALSE), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(osnnet(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris)), size = 2, trace = FALSE), gives_warning("only first element of 'bw' used"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = rep(1, nrow(iris)), softmax = TRUE, size = 2, trace = FALSE), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50, size = 2, trace = FALSE), throws_error("'k' must be positive"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", k =-7, bw = 50, softmax = TRUE, size = 2, trace = FALSE), throws_error("'k' must be positive"))

	# k too small
	#fit <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005, size = 2, trace = FALSE)
	#expect_equal(length(is.na(predict(fit)$class)), 150)

	# k too large
	expect_that(osnnet(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50, size = 2, trace = FALSE), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(osnnet(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris)), size = 2, trace = FALSE), gives_warning("only first element of 'k' used"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", k = rep(50, nrow(iris)), softmax = TRUE, size = 2, trace = FALSE), gives_warning("only first element of 'k' used"))
})


test_that("osnnet: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = rectangular(bw = 5), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "rectangular", k = 50, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = rectangular(k = 50), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	
	# nn.only not needed
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE, size = 2, trace = FALSE), gives_warning("argument 'nn.only' is ignored"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "rectangular", bw = 5, nn.only = TRUE, softmax = TRUE, size = 2, trace = FALSE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE, size = 2, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "rectangular", bw = 5, k = 50, nn.only = FALSE, softmax = TRUE, size = 2, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = gaussian(k = 50), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)

	# adaptive bw, all obs
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, size = 2, trace = FALSE)
	fit2 <- osnnet(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50), size = 2, trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE, size = 2, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	expect_that(osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = 1, k = 50, nn.only = FALSE, softmax = TRUE, size = 2, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})	


#=================================================================================================================
context("predict.osnnet")

test_that("predict.osnnet works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
  	pred <- predict(fit)
  	expect_equal(rownames(pred), rownames(iris)[ran])  	
  	pred <- predict(fit, type = "class")
  	expect_equal(names(pred), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)  
  	pred <- predict(fit, newdata = iris[-ran,])
  	expect_equal(rownames(pred), rownames(iris)[-ran])  	  	
	## y, x
	fit <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "gaussian", bw = 2, softmax = TRUE, size = 2, trace = FALSE)  
  	pred <- predict(fit)
  	expect_equal(rownames(pred), rownames(iris))
  	pred <- predict(fit, type = "class")
  	expect_equal(names(pred), rownames(iris))  	
	## y, x, newdata
	fit <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "gaussian", bw = 2, softmax = TRUE, size = 2, trace = FALSE)  
  	pred <- predict(fit, newdata = iris[-ran,-5])
  	expect_equal(rownames(pred), rownames(iris)[-ran])  	
  	pred <- predict(fit, newdata = iris[-ran,-5], type = "class")
  	expect_equal(names(pred), rownames(iris)[-ran])
})


test_that("predict.osnnet: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	Wts = runif(n = 19, -0.5, 0.5)	
	fit <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, Wts = Wts, size = 2, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
  	pred1 <- predict(fit, type = "class")
  	pred2 <- predict(fit, newdata = iris, type = "class")
  	expect_equal(pred1, pred2)
	# y, x
	fit <- osnnet(x = iris[,-5], y = class.ind(iris$Species), wf = "gaussian", bw = 2, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[,-5]) ## no names
  	expect_equal(pred1, pred2)
  	pred1 <- predict(fit, type = "class")
  	pred2 <- predict(fit, newdata = iris[,-5], type = "class") ## no names
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- osnnet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, Wts = Wts, size = 2, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
  	pred1 <- predict(fit, type = "class")
  	pred2 <- predict(fit, newdata = iris[ran,], type = "class")
  	expect_equal(pred1, pred2)
	# y, x
	fit <- osnnet(x = iris[ran,-5], y = class.ind(iris$Species)[ran,], wf = "gaussian", bw = 2, softmax = TRUE, Wts = Wts, size = 2, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred1, pred2)
  	pred1 <- predict(fit, type = "class")
  	pred2 <- predict(fit, newdata = iris[ran,-5], type = "class")
  	expect_equal(pred1, pred2)
})


test_that("predict.osnnet works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(ncol(fit$y), 2)
	expect_equal(fit$n[3], 2)
	pred <- predict(fit)
	expect_equal(ncol(pred), 2)
	pred <- predict(fit, type = "class")
	expect_equal(nlevels(pred), 3)
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(ncol(pred), 2)
	pred <- predict(fit, newdata = iris[-ran,], type = "class")
	expect_equal(nlevels(pred), 3)	
})


test_that("predict.osnnet works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osnnet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
	expect_equal(ncol(fit$x), 1)
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.osnnet works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(dim(pred), c(1, 3))
  	pred <- predict(fit, newdata = iris[5,], type = "class")
	expect_equal(length(pred), 1)
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred, a)
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(dim(pred), c(1, 3))
	pred <- predict(fit, newdata = iris[58,], type = "class")
	expect_equal(length(pred), 1)
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred, a)
})	


test_that("predict.osnnet works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osnnet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
	expect_equal(ncol(fit$x), 1)
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(dim(pred), c(1, 3))
	pred <- predict(fit, newdata = iris[5,], type = "class")
	expect_equal(length(pred), 1)
})

   
test_that("predict.osnnet: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	## formula
	fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran, size = 2, trace = FALSE)
	pred <- predict(fit, newdata = irisna)
	expect_equal(nrow(pred), 133)	
	pred <- predict(fit, newdata = irisna, type = "class")
	expect_equal(length(pred), 133)
	## matrix
	fit <- osnnet(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = 50, softmax = TRUE, size = 2, trace = FALSE)
	expect_that(pred <- predict(fit, newdata = irisna), throws_error("missing values in 'x'"))
	expect_that(pred <- predict(fit, newdata = irisna, type = "class"), throws_error("missing values in 'x'"))
})


test_that("predict.osnnet: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osnnet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})  	


#=================================================================================================================


## fixed bandwidth
# res <- osnnet(Species ~ ., data = iris, wf = "biweight", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- osnnet(Species ~ ., data = iris, wf = biweight(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "cauchy", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = cauchy(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "cosine", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = cosine(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "epanechnikov", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = epanechnikov(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "exponential", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = exponential(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "gaussian", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = gaussian(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "optcosine", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = optcosine(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "rectangular", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = rectangular(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "triangular", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = triangular(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)



# ## fixed bandwidth, knn
# res <- osnnet(Species ~ ., data = iris, wf = "biweight", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- osnnet(Species ~ ., data = iris, wf = biweight(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "cauchy", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = cauchy(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "cosine", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = cosine(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "exponential", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = exponential(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "gaussian", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = gaussian(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = optcosine(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = rectangular(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "triangular", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = triangular(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# ## adaptive bandwidth, knn only
# res <- locnnet(Species ~ ., data = iris, wf = "biweight", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = biweight(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "cauchy", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = cauchy(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "cosine", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = cosine(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "epanechnikov", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = eoanechnikov(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "exponential", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = exponential(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "gaussian", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = gaussian(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "optcosine", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = optcosine(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "rectangular", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = rectangular(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "triangular", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = triangular(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)


# ## adaptive bandwidth, all obs
# res <- locnnet(Species ~ ., data = iris, wf = "exponential", k = 100, nn.only = FALSE)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = exponential(k = 100, nn.only = FALSE))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- locnnet(Species ~ ., data = iris, wf = "gaussian", k = 100, nn.only = FALSE)
# pred1 <- predict(res, newdata = iris[1,])
# res <- locnnet(Species ~ ., data = iris, wf = gaussian(k = 100, nn.only = FALSE))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)
