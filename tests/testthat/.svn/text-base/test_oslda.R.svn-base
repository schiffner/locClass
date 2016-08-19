context("oslda")

test_that("oslda: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(oslda(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10))
	# wrong class
	expect_error(oslda(formula = iris, data = iris, wf = "gaussian", bw = 10))
	#expect_error(oslda(iris, data = iris, wf = "gaussian", bw = 10))
	# target variable also in x
	fit <- oslda(grouping = iris$Species, x = iris, wf = "gaussian", bw = 10) ## todo!!!
	expect_warning(predict(fit))
	expect_warning(oslda(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10))           ## warning, Species on RHS removed
	# missing x
	expect_error(oslda(grouping = iris$Species, wf = "gaussian", bw = 10))
	## wrong method argument
	# missing quotes
	expect_error(oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, method = ML))
	# method as vector
	expect_error(oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, method = c("ML","unbiased")))
})


test_that("oslda throws a warning if grouping variable is numeric", {
	data(iris)
	# formula, data
	expect_that(oslda(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10), gives_warning("'grouping' was coerced to a factor"))
	# grouping, x
	expect_that(oslda(grouping = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10), gives_warning("'grouping' was coerced to a factor"))
})


test_that("oslda works if only one predictor variable is given", {
	data(iris)
	fit <- oslda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5)
	predict(fit)
})


test_that("oslda: training data from only one class", {
	data(iris)
	expect_that(oslda(Species ~ ., data = iris, bw = 2, subset = 1:50), throws_error("training data from only one group given"))
	expect_that(oslda(Species ~ ., data = iris, bw = 2, subset = 1), throws_error("training data from only one group given"))
	expect_that(oslda(grouping = iris$Species, x = iris[,-5], bw = 2, subset = 1:50), throws_error("training data from only one group given"))
	expect_that(oslda(grouping = iris$Species, x = iris[,-5], bw = 2, subset = 1), throws_error("training data from only one group given"))
})


test_that("oslda: subsetting works", {
	data(iris)
	# formula, data
	expect_that(fit1 <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2), gives_warning("group virginica is empty"))
	expect_equal(fit1[-13],fit2[-13])
	expect_equal(fit1$N, 80)
	# x, grouping
	expect_that(fit1 <- oslda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(grouping = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2), gives_warning("group virginica is empty"))
	expect_equal(fit1[-13],fit2[-13])
	expect_equal(fit1$N, 80)
	# wrong specification of subset argument
	expect_error(oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,]))
	## todo
	# expect_error(fit <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE)) #???
	# expect_error(fit <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0)) #???
	##
	expect_error(oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50))
})


test_that("oslda: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(oslda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- oslda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(13, 16)], fit2[-13])

	## x, grouping
	# na.fail
	expect_that(oslda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- oslda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-13], fit2[-13])
	
	### NA in grouping
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(oslda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- oslda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(13, 16)], fit2[-13])
	## x, grouping
	# na.fail
	expect_that(oslda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- oslda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-13], fit2[-13])

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(13, 16)], fit2[-13])
	## x, grouping
	# na.fail
	expect_that(oslda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- oslda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- oslda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-13], fit2[-13])
})


test_that("oslda: try all weight functions", {
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5)    
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = gaussian(5))    
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 5)    
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = gaussian(5))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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
		
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5, k = 30)    
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = gaussian(bw = 5, k = 30))    
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 5, k = 30)    
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = gaussian(5, 30))
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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
	
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 30)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 30))
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "epanechnikov", bw = 5, k = 30)
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = epanechnikov(5, 30))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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

	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 30)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 30))
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "rectangular", bw = 5, k = 30)
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = rectangular(5, 30))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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

	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 30)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = triangular(5, k = 30))
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "triangular", bw = 5, k = 30)
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = triangular(5, 30))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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

	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "biweight", bw = 5)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = biweight(5))
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "biweight", bw = 5)
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = biweight(5))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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

	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 30)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = optcosine(5, k = 30))
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "optcosine", bw = 5, k = 30)
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = optcosine(5, 30))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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

	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = cosine(5, k = 30))
	fit3 <- oslda(x = iris[,-5], grouping = iris$Species, wf = "cosine", bw = 5, k = 30)
	fit4 <- oslda(x = iris[,-5], grouping = iris$Species, wf = cosine(5, 30))    
	expect_equal(fit1[-c(6, 13)], fit2[-c(6, 13)])
	expect_equal(fit3[-c(6, 13)], fit4[-c(6, 13)])
	expect_equal(fit2[-c(2,13:15)], fit4[-c(2,13:14)])
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


test_that("oslda: local solution with rectangular window function and large bw and global solution coincide", {
	library(MASS)
	# unbiased
	fit1 <- wlda(formula = Species ~ ., data = iris, method = "unbiased")
	pred1 <- predict(fit1)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = rectangular(20), method = "unbiased")
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)
	fit3 <- lda(Species ~ ., data = iris)
	pred3 <- predict(fit3, newdata = iris)
	names(pred3$class) <- names(pred2$class)
	expect_equal(pred2$class, pred3$class)
	expect_equal(pred2$posterior, pred3$posterior)
	
	# ML
	fit1 <- wlda(formula = Species ~ ., data = iris, method = "ML")
	pred1 <- predict(fit1)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = rectangular(20), method = "ML")
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)
	fit3 <- lda(Species ~ ., data = iris, method = "mle")
	pred3 <- predict(fit3, newdata = iris)
	names(pred3$class) <- names(pred2$class)
	expect_equal(pred2$class, pred3$class)
	expect_equal(pred2$posterior, pred3$posterior)
})


test_that("oslda: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- oslda(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- oslda(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-13], fit2[-13])

	expect_that(fit1 <- oslda(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5), gives_warning("argument 'bw' is ignored"))	
	fit2 <- oslda(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-13], fit2[-13])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- oslda(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- oslda(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-13], fit2[-13])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- oslda(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5), gives_warning("argument 'bw' is ignored"))
	fit2 <- oslda(Species ~ ., data = iris, wf = function(x) exp(-x))
	expect_equal(fit1[-13], fit2[-13])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	fit <- oslda(formula = Species ~ ., data = iris, wf = gaussian) ## error because length(weights) and nrow(x) are different
	expect_error(predict(fit))
	
	# bw, k missing
	expect_that(oslda(formula = Species ~ ., data = iris, wf = gaussian()), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(oslda(formula = Species ~ ., data = iris, wf = gaussian(), k = 10), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(oslda(Species ~ ., data = iris), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5), throws_error("'bw' must be positive"))
	expect_that(oslda(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(oslda(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris))), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(oslda(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50), throws_error("'k' must be positive"))

	# k too small
	#fit <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005)
	#expect_equal(length(is.na(predict(fit)$class)), 150)

	# k too large
	expect_that(oslda(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(oslda(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris))), gives_warning("only first element of 'k' used"))
})


test_that("oslda: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "rectangular", k = 50)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = rectangular(k = 50))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	
	# nn.only not needed
	expect_that(oslda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(oslda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = gaussian(k = 50))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)

	# adaptive bw, all obs
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50)
	fit2 <- oslda(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50))
	expect_equal(fit1[-c(6,13)], fit2[-c(6,13)])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})	


#=================================================================================================================
context("predict.oslda")

test_that("predict.oslda works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit)
  	expect_equal(names(pred$class), rownames(iris)[ran])  	
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit, newdata = iris[-ran,])
  	expect_equal(names(pred$class), rownames(iris)[-ran])  	
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])  	
	## grouping, x
	fit <- oslda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit)
  	expect_equal(names(pred$class), rownames(iris)[ran])  	
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## grouping, x, newdata
	fit <- oslda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit, newdata = iris[-ran,-5])
  	expect_equal(names(pred$class), rownames(iris)[-ran])  	
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])  	
})


test_that("predict.oslda: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- oslda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[,-5])
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- oslda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
	# y, x
	fit <- oslda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred1, pred2)
})


test_that("predict.oslda works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100), gives_warning("group virginica is empty"))
	expect_equal(length(fit$counts), 2)
	a <- rep(50, 2)
	names(a) <- names(fit$counts)
	expect_equal(fit$counts, a)
	expect_equal(fit$N, 100)
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(pred$class), 3)
	expect_equal(ncol(pred$posterior), 2)
})


test_that("predict.oslda works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- oslda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
	expect_equal(ncol(fit$x), 1)
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.oslda works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred$class, a)
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred$class, a)
})	


test_that("predict.oslda works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- oslda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
	expect_equal(ncol(fit$x), 1)
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
})

   
test_that("predict.oslda: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran)
	expect_that(pred <- predict(fit, newdata = irisna), gives_warning("NAs in test observation 1"))
	expect_equal(all(is.na(pred$class[1:17])), TRUE)
	expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
})


test_that("predict.oslda: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- oslda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})  	

#=================================================================================================================

## fixed bandwidth
# res <- oslda(Species ~ ., data = iris, wf = "biweight", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- oslda(Species ~ ., data = iris, wf = biweight(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "cauchy", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = cauchy(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "cosine", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = cosine(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "epanechnikov", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = epanechnikov(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "exponential", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = exponential(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "gaussian", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = gaussian(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "optcosine", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = optcosine(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "rectangular", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = rectangular(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "triangular", bw = 5)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = triangular(bw = 5))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)



# ## fixed bandwidth, knn
# res <- oslda(Species ~ ., data = iris, wf = "biweight", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- oslda(Species ~ ., data = iris, wf = biweight(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "cauchy", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = cauchy(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "cosine", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = cosine(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "exponential", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = exponential(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "gaussian", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = gaussian(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = optcosine(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = rectangular(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "triangular", bw = 5, k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = triangular(bw = 5, k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# ## adaptive bandwidth, knn only
# res <- loclda(Species ~ ., data = iris, wf = "biweight", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = biweight(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "cauchy", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = cauchy(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "cosine", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = cosine(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "epanechnikov", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = eoanechnikov(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "exponential", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = exponential(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "gaussian", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = gaussian(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "optcosine", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = optcosine(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "rectangular", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = rectangular(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "triangular", k = 20)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = triangular(k = 20))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)


# ## adaptive bandwidth, all obs
# res <- loclda(Species ~ ., data = iris, wf = "exponential", k = 100, nn.only = FALSE)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = exponential(k = 100, nn.only = FALSE))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)

# res <- loclda(Species ~ ., data = iris, wf = "gaussian", k = 100, nn.only = FALSE)
# pred1 <- predict(res, newdata = iris[1,])
# res <- loclda(Species ~ ., data = iris, wf = gaussian(k = 100, nn.only = FALSE))
# pred2 <- predict(res, newdata = iris[1,])
# all.equal(pred1, pred2)
