context("osmultinom")

test_that("osmultinom: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(osmultinom(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10, trace = FALSE))
	# wrong class
	# FIXME
	# expect_error(osmultinom(formula = iris, data = iris, wf = "gaussian", bw = 10, trace = FALSE))	# target variable is Sepal.Length
	# expect_error(osmultinom(iris, data = iris, wf = "gaussian", bw = 10,  trace = FALSE))				# target variable is Sepal.Length
	# target variable also in x
	expect_warning(osmultinom(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10, trace = FALSE))           ## warning, Species on RHS removed
})


# test_that("osmultinom throws a warning if y variable is numeric", {
	# data(iris)
	# # formula, data
	# expect_that(osmultinom(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10,   trace = FALSE), gives_warning("'y' was coerced to a factor"))
	# # y, x
	# expect_that(osmultinom(y = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10,   trace = FALSE), gives_warning("'y' was coerced to a factor"))
# })


test_that("osmultinom works if only one predictor variable is given", {
	data(iris)
	fit <- osmultinom(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5, trace = FALSE)
	predict(fit)
})


test_that("osmultinom: training data from only one class", {
	data(iris)
	expect_that(osmultinom(Species ~ ., data = iris, bw = 2, subset = 1:50,  trace = FALSE), throws_error("need two or more classes to fit a multinom model"))
	expect_that(osmultinom(Species ~ ., data = iris, bw = 2, subset = 1,  trace = FALSE), throws_error("need two or more classes to fit a multinom model"))
	iris2 <- iris[1:100,]
	iris2$Species <- factor(iris2$Species, levels = unique(iris2$Species))
	expect_that(osmultinom(Species ~ ., data = iris2, bw = 2, subset = 1:50,  trace = FALSE), throws_error("need two or more classes to fit a multinom model"))
	expect_that(osmultinom(Species ~ ., data = iris2, bw = 2, subset = 1,  trace = FALSE), throws_error("need two or more classes to fit a multinom model"))
})


test_that("osmultinom: subsetting works", {
	data(iris)
	# formula, data
	expect_that(fit1 <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osmultinom(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-28],fit2[-28])
	# wrong specification of subset argument
	expect_error(osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,],  trace = FALSE))
	expect_error(fit <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE,  trace = FALSE))
	expect_error(fit <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0,  trace = FALSE))	
	expect_error(osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50,  trace = FALSE))
})


test_that("osmultinom: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(osmultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail,  trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- osmultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osmultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(28,34)], fit2[-28])
	
	### NA in y
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(osmultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail,  trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- osmultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osmultinom(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(28,34)], fit2[-28])

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail,  trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_that(fit2 <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(28,34)], fit2[-28])
})


test_that("osmultinom: try all weight functions", {
	Wts = runif(n = 18, -0.5, 0.5)

	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5, Wts = Wts,  trace = FALSE)    
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian(5), Wts = Wts,  trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)
		
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5, k = 30, Wts = Wts,  trace = FALSE)    
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian(bw = 5, k = 30), Wts = Wts,  trace = FALSE)    
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)
	
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 30, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 30), Wts = Wts,  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)

	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 30, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 30), Wts = Wts,  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)

	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 30, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = triangular(5, k = 30), Wts = Wts,  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)

	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = biweight(5), Wts = Wts,  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)

	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 30, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = optcosine(5, k = 30), Wts = Wts,  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)

	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = cosine(5, k = 30), Wts = Wts,  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	expect_equal(pred1, pred2)
})


test_that("osmultinom: local solution with rectangular window function and large bw and global solution coincide", {
	library(nnet)
	Wts = runif(n = 18, -0.5, 0.5)	
	## formula
	fit1 <- multinom(formula = Species ~ ., data = iris, Wts = Wts,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = rectangular(20), Wts = Wts,  trace = FALSE)
	pred1 <- predict(fit1, type = "probs")
	pred2 <- predict(fit2, type = "probs")
	expect_equal(pred1, pred2)
	pred1 <- predict(fit1, type = "class")
	pred2 <- predict(fit2, type = "class")
	expect_equivalent(pred1, pred2)	
})


test_that("osmultinom: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- osmultinom(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5,  trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- osmultinom(Species ~ ., data = iris, wf = gaussian(0.5),  trace = FALSE)
	expect_equal(fit1[-28], fit2[-28])

	expect_that(fit1 <- osmultinom(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5,  trace = FALSE), gives_warning("argument 'bw' is ignored"))	
	fit2 <- osmultinom(Species ~ ., data = iris, wf = gaussian(0.5),  trace = FALSE)
	expect_equal(fit1[-28], fit2[-28])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- osmultinom(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30,  trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- osmultinom(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30,  trace = FALSE), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-28], fit2[-28])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- osmultinom(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5,  trace = FALSE), gives_warning("argument 'bw' is ignored"))
	fit2 <- osmultinom(Species ~ ., data = iris, wf = function(x) exp(-x),  trace = FALSE)
	expect_equal(fit1[-28], fit2[-28])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	fit <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian,  trace = FALSE) ## error because length(weights) and nrow(x) are different
	expect_error(predict(fit))
	
	# bw, k missing
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = gaussian(),  trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = gaussian(), k = 10,  trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(osmultinom(Species ~ ., data = iris,  trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5,  trace = FALSE), throws_error("'bw' must be positive"))
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50,  trace = FALSE), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(osmultinom(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris)),  trace = FALSE), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50,  trace = FALSE), throws_error("'k' must be positive"))

	# k too small
	#fit <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005,  trace = FALSE)
	#expect_equal(length(is.na(predict(fit)$class)), 150)

	# k too large
	expect_that(osmultinom(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50,  trace = FALSE), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(osmultinom(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris)),  trace = FALSE), gives_warning("only first element of 'k' used"))
})


test_that("osmultinom: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = rectangular(bw = 5),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "rectangular", k = 50,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = rectangular(k = 50),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	
	# nn.only not needed
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE,  trace = FALSE), gives_warning("argument 'nn.only' is ignored"))
	# expect_that(osmultinom(y = class.ind(iris$Species), x = iris[,-5], wf = "rectangular", bw = 5, nn.only = TRUE, softmax = TRUE,  trace = FALSE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE,  trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	# expect_that(osmultinom(y = class.ind(iris$Species), x = iris[,-5], wf = "rectangular", bw = 5, k = 50, nn.only = FALSE, softmax = TRUE,  trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", k = 50,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian(k = 50),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)

	# adaptive bw, all obs
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50,  trace = FALSE)
	fit2 <- osmultinom(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50),  trace = FALSE)
	expect_equal(fit1[-c(22,28)], fit2[-c(22,28)])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE,  trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	# expect_that(osmultinom(y = class.ind(iris$Species), x = iris[,-5], wf = "gaussian", bw = 1, k = 50, nn.only = FALSE, softmax = TRUE,  trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})	


#=================================================================================================================
context("predict.osmultinom")

test_that("predict.osmultinom works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran,  trace = FALSE)
  	pred <- predict(fit, type = "probs")
  	expect_equal(rownames(pred), rownames(iris)[ran])  	
  	pred <- predict(fit, type = "class")
  	expect_equal(names(pred), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran,  trace = FALSE)  
  	pred <- predict(fit, newdata = iris[-ran,], type = "probs")
  	expect_equal(rownames(pred), rownames(iris)[-ran])  	  	
  	pred <- predict(fit, newdata = iris[-ran,], type = "class")
  	expect_equal(names(pred), rownames(iris)[-ran])  	
})


test_that("predict.osmultinom: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
  	pred1 <- predict(fit, type = "probs")
  	pred2 <- predict(fit, newdata = iris, type = "probs")
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- osmultinom(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
  	pred1 <- predict(fit, type = "probs")
  	pred2 <- predict(fit, newdata = iris[ran,], type = "probs")
  	expect_equal(pred1, pred2)
})


test_that("predict.osmultinom works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100,  trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(ncol(fit$y), 1)
	expect_equal(fit$n[3], 1)
	expect_equal(fit$lev, c("setosa", "versicolor", "virginica"))
	expect_equal(fit$lev1, c("setosa", "versicolor"))
	pred <- predict(fit, type = "probs")
	expect_equal(ncol(pred), 1)
	pred <- predict(fit, type = "class")
	expect_equal(nlevels(pred), 3)
	pred <- predict(fit, newdata = iris[-ran,], type = "probs")
	expect_equal(ncol(pred), 1)
	pred <- predict(fit, newdata = iris[-ran,], type = "class")
	expect_equal(nlevels(pred), 3)
})


test_that("predict.osmultinom works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osmultinom(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran,  trace = FALSE)
	expect_equal(ncol(fit$x), 2) # + intercept
	expect_equal(fit$coefnames, c("(Intercept)", "Petal.Width"))
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.osmultinom works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran,  trace = FALSE)
  	pred <- predict(fit, newdata = iris[5,], type = "probs")
	expect_equal(dim(pred), c(1, 3)) ## drop
  	pred <- predict(fit, newdata = iris[5,], type = "class")
	expect_equal(length(pred), 1)
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred, a)
	pred <- predict(fit, newdata = iris[58,], type = "probs")
	expect_equal(dim(pred), c(1, 3)) ##
	pred <- predict(fit, newdata = iris[58,], type = "class")
	expect_equal(length(pred), 1)
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred, a)
})	


test_that("predict.osmultinom works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osmultinom(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran,  trace = FALSE)
	expect_equal(ncol(fit$x), 2)
	expect_equal(fit$coefnames, c("(Intercept)", "Petal.Width"))
	pred <- predict(fit, newdata = iris[5,], type = "probs")
	expect_equal(dim(pred), c(1, 3))
	pred <- predict(fit, newdata = iris[5,], type = "class")
	expect_equal(length(pred), 1)
})

   
test_that("predict.osmultinom: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran,  trace = FALSE)
	pred <- predict(fit, newdata = irisna, type = "probs")
	expect_equal(all(is.na(pred[1:17,])), TRUE)	
	pred <- predict(fit, newdata = irisna, type = "class")
	expect_equal(all(is.na(pred[1:17])), TRUE)
})


test_that("predict.osmultinom: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- osmultinom(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran,  trace = FALSE)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})  


