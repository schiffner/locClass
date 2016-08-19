context("dannet")

class.ind <- function(cl) {
    n <- length(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}


test_that("dannet: reps argument works", {
	## formula, data
	fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, size = 2, trace = FALSE)
	expect_equal(fit$reps, 1)
	predict(fit)
	predict(fit, newdata = iris[1,])
	fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, size = 2, trace = FALSE, reps = 3)
	expect_equal(fit$reps, 3)
	predict(fit)
	predict(fit, newdata = iris[1,])
	## x, y	
	fit <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, size = 2, trace = FALSE)
	expect_equal(fit$reps, 1)
	predict(fit)
	predict(fit, newdata = iris[1,-5])
	fit <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, size = 2, trace = FALSE, reps = 3)
	expect_equal(fit$reps, 3)
	predict(fit)
	predict(fit, newdata = iris[1,-5])
})


test_that("dannet: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(dannet(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	# wrong class
	expect_error(dannet(formula = iris, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	expect_error(dannet(iris, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	# target variable also in x
  	#expect_error(dannet(y = iris$Species, x = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	expect_warning(dannet(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10, size = 2, trace = FALSE))           ## warning, Species on RHS removed
	# missing x
	expect_error(dannet(y = iris$Species, wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	## itr
	expect_that(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = -5, size = 2, trace = FALSE), throws_error("'itr' must be >= 1"))
	expect_that(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = 0, size = 2, trace = FALSE), throws_error("'itr' must be >= 1"))
})


test_that("dannet throws a warning if grouping variable is numeric", {
	data(iris)
	# formula, data
	expect_that(dannet(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", size = 2, bw = 10, trace = FALSE), gives_warning("response was coerced to a factor"))
	# y, x
	#expect_error(dannet(y = class.ind(as.numeric(iris$Species)), x = iris[,-5], wf = "gaussian", bw = 10, size = 2, trace = FALSE))
	expect_that(dannet(y = as.numeric(iris$Species), x = iris[,-5], wf = "gaussian", bw = 10, size = 2, trace = FALSE), gives_warning("'y' was coerced to a factor"))
	y <- class.ind(iris$Species)
	y[1,1] <- 2
	expect_that(dannet(y = y, x = iris[,-5], wf = "gaussian", bw = 10, size = 2, trace = FALSE), throws_error("only factors are allowed as reponse"))
})


test_that("dannet: training data from only one class", {
	data(iris)
	expect_that(dannet(Species ~ ., data = iris, bw = 2, subset = 1:50), throws_error("training data from only one class given"))
	expect_that(dannet(y = iris$Species, x = iris[,-5], bw = 2, subset = 1:50), throws_error("training data from only one class given"))
})


test_that("dannet works if only one predictor variable is given", {
	data(iris)
	fit <- dannet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5, size = 2, trace = FALSE)
	expect_equal(fit$coefnames, "Petal.Width")	
})


test_that("dannet: one training observation", {
	data(iris)
	# one training observation
	expect_that(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, size=10, subset = 1, trace = FALSE), throws_error("training data from only one class given"))
	# one training observation in one predictor variable
	expect_that(dannet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, size=10, subset = 1, trace = FALSE), throws_error("training data from only one class given"))
})


test_that("dannet: initial weighting works correctly", {
	data(iris)
	## check if weighted solution with initial weights = 1 equals unweighted solution
	set.seed(120)
  	fit1 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, size = 2, trace = FALSE)
	set.seed(120)
	fit2 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(1,150), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16]) 
	## returned weights	
	a <- rep(1,150)
	names(a) <- 1:150
	expect_equal(fit1$weights[[1]], a)
	expect_equal(fit1$weights, fit2$weights)
	## weights and subsetting
	# formula, data
	a <- rep(1,50)
	names(a) <- 11:60
	expect_that(fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit$weights[[1]], a)
	# formula, data, weights
	a <- rep(1:3,50)[11:60]
	a <- a/sum(a) * length(a)
	names(a) <- 11:60
	expect_that(fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(1:3, 50), subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit$weights[[1]], a)
	# x, y
	a <- rep(1,150)
	names(a) <- 1:150
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, size = 2, trace = FALSE)
	expect_equal(fit$weights[[1]], a)	
	# x, y, weights
	a <- rep(1:3,50)
	a <- a/sum(a) * length(a)
	names(a) <- 1:150
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, weights = rep(1:3, 50), size = 2, trace = FALSE)
	expect_equal(fit$weights[[1]], a)
	## wrong specification of weights argument
	# weights in a matrix
	weight <- matrix(seq(1:150), nrow = 50)
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = weight, size = 2, trace = FALSE))
	# weights < 0
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(-5, 150), size = 2, trace = FALSE))
	# weights true/false
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = TRUE, size = 2, trace = FALSE))
})


test_that("dannet breaks out of for-loop if only one class is left", {
	set.seed(120)
	expect_that(fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 60, size = 2, trace = FALSE), gives_warning("for at least one class all weights are zero"))
	expect_equal(fit1$itr, 3)
	expect_equal(length(fit1$weights), 4)
	# break out
	set.seed(120)
	expect_that(fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 2, size = 2, trace = FALSE), gives_warning("training data from only one class, breaking out of iterative procedure"))
	expect_equal(fit1$itr, 0)
	expect_equal(length(fit1$weights), 1)
})
#sapply(fit1$weights, function(x) return(list(sum(x[1:50]),sum(x[51:100]),sum(x[101:150]))))


test_that("dannet: subsetting works", {
	data(iris)
	# formula, data
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16],fit2[-16])
	a <- rep(1,80)
	names(a) <- 1:80
	expect_equal(fit1$weights[[1]], a)
	# formula, data, weights
	set.seed(120)
	expect_that(fit1 <- dannet(Species ~ ., data = iris, weights = rep(1:3, each = 50), wf = "gaussian", bw = 2, subset = 1:80, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
	expect_that(fit2 <- dannet(Species ~ ., data = iris[1:80,], weights = rep(1:3, each = 50)[1:80], wf = "gaussian", bw = 2, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16],fit2[-16])
	a <- rep(80, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	b <- rep(1:3, each = 50)[1:80]
	b <- b/sum(b) * length(b)
	names(b) <- 1:80
	expect_equal(fit1$weights[[1]], b)
	# y, x
	set.seed(120)
  	expect_that(fit1 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, subset = 1:80, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(y = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16],fit2[-16])
	a <- rep(1,80)
	names(a) <- 1:80
	expect_equal(fit1$weights[[1]], a)
	# y, x, weights
	set.seed(120)
	expect_that(fit1 <- dannet(y = iris$Species, x = iris[,-5], weights = rep(1:3, each = 50), wf = "gaussian", bw = 2, subset = 1:80, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
	expect_that(fit2 <- dannet(y = iris$Species[1:80], x = iris[1:80,-5], weights = rep(1:3, each = 50)[1:80], wf = "gaussian", bw = 2, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16],fit2[-16])
	a <- rep(80, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	b <- rep(1:3, each = 50)[1:80]
	b <- b/sum(b) * length(b)
	names(b) <- 1:80
	expect_equal(fit1$weights[[1]], b)
	# for dannet.default no subset argument available
	# wrong specification of subset argument
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,], size = 2, trace = FALSE))
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE, size = 2, trace = FALSE))
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0, size = 2, trace = FALSE))
	expect_error(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50, size = 2, trace = FALSE))
})


test_that("dannet: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
	expect_that(fit1 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16, 28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, size = 2, weights = rep(1:3, 50), trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16, 28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y
	# na.fail
	expect_that(dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
	expect_that(fit1 <- dannet(y = iris$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(y = iris$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y, weights
	# na.fail
	expect_error(dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, weights = rep(1:3, 50), trace = FALSE))
	# for dannet.default no na.action argument available
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(y = iris$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(y = iris$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, weights = rep(1:3, 50), trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	
	### NA in y
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16, 28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
	expect_that(fit1 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, size = 2, weights = rep(1:3, 50), trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16, 28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y
	# na.fail
	expect_that(dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, size = 2, subset = 6:60, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y, weights
	# na.fail
	expect_that(dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
	expect_that(fit1 <- dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = rep(1:3, 50), na.action = na.omit, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
	expect_that(fit2 <- dannet(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, weights = rep(1:3, 50), trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	### NA in weights
	weights <- rep(1:3,50)
	weights[1:10] <- NA
	## formula, data, weights
	# na.fail
	expect_that(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = weights, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, weights = weights, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16, 28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y, weights
	# na.fail
	expect_that(dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 6:60, size = 2, weights = weights, na.action = na.fail, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = weights, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16,28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50), size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(16, 28)], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y
	# na.fail
	expect_that(dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
	expect_that(fit1 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
	expect_that(fit2 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, y, weights
	# na.fail
	expect_that(dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.fail, size = 2, trace = FALSE), throws_error("missing values in object"))
	# check if na.omit works correctly
	set.seed(120)
	expect_that(fit1 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.omit, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	set.seed(120)
	expect_that(fit2 <- dannet(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50), size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit1[-16], fit2[-16])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
})


test_that("dannet: try all weight functions", {
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, size = 2, trace = FALSE)    
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = gaussian(0.5), size = 2, trace = FALSE)    
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 0.5, size = 2, trace = FALSE)    
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = gaussian(0.5), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	expect_equal(fit2[-c(16, 26, 28)], fit4[-c(16, 27)])
	
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 60, size = 2, trace = FALSE)    
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5, k = 60), size = 2, trace = FALSE)    
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 0.5, k = 60, size = 2, trace = FALSE)    
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = gaussian(0.5, 60), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	expect_equal(fit2[-c(16, 26, 28)], fit4[-c(16, 27)])
	a <- rep(60, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 30), size = 2, trace = FALSE)
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "epanechnikov", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = epanechnikov(5, 30), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	#expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

  	set.seed(120)
	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 30), size = 2, trace = FALSE)
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "rectangular", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = rectangular(5, 30), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	#expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = triangular(5, k = 30), size = 2, trace = FALSE)
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "triangular", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = triangular(5, 30), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	#expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = biweight(5, k = 30), size = 2, trace = FALSE)
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "biweight", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = biweight(5, 30), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	#expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = optcosine(5, k = 30), size = 2, trace = FALSE)
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "optcosine", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = optcosine(5, 30), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	#expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = cosine(5, k = 30), size = 2, trace = FALSE)
	set.seed(120)
  	fit3 <- dannet(x = iris[,-5], y = iris$Species, wf = "cosine", bw = 5, k = 30, size = 2, trace = FALSE)
	set.seed(120)
  	fit4 <- dannet(x = iris[,-5], y = iris$Species, wf = cosine(5, 30), size = 2, trace = FALSE)    
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit3[-16], fit4[-16])
	#expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
})


test_that("dannet: local solution with rectangular window function and large bw and global solution coincide", {
	library(nnet)
	Wts <- runif(19,-0.7,0.7)
	fit1 <- nnet(Species ~ ., data = iris, size = 2, Wts = Wts, trace = FALSE)
	fit2 <- dannet(Species ~ ., data = iris, size = 2, wf = "rectangular", bw = 10, Wts = Wts, trace = FALSE)
	expect_equal(fit1[-16], fit2[-c(16:25)])   
	a <- rep(1,150)
	names(a) <- 1:150
	expect_equal(fit2$weights[[1]], a)                                             
	expect_equal(fit2$weights[[2]], a)                                             
	expect_equal(fit2$weights[[3]], a)                                             
	expect_equal(fit2$weights[[4]], a)                                             
	expect_equal(predict(fit2)$posterior, predict(fit1))
})


test_that("dannet: arguments related to weighting misspecified", {
	# bw, k not required
	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5, size = 2, trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	set.seed(120)
  	fit2 <- dannet(Species ~ ., data = iris, wf = gaussian(0.5), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])                                

	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5, size = 2, trace = FALSE), gives_warning("argument 'bw' is ignored"))	
	set.seed(120)
  	fit2 <- dannet(Species ~ ., data = iris, wf = gaussian(0.5), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30, size = 2, trace = FALSE), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	set.seed(120)
  	expect_that(fit2 <- dannet(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30, size = 2, trace = FALSE), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	set.seed(120)
  	expect_that(fit1 <- dannet(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, size = 2, trace = FALSE), gives_warning("argument 'bw' is ignored"))
	set.seed(120)
  	fit2 <- dannet(Species ~ ., data = iris, wf = function(x) exp(-x), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	expect_error(dannet(formula = Species ~ ., data = iris, wf = gaussian, size = 2, trace = FALSE)) ## error because length(weights) and nrow(x) are different

	# bw, k missing
	expect_that(dannet(formula = Species ~ ., data = iris, wf = gaussian(), size = 2, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(dannet(formula = Species ~ ., data = iris, wf = gaussian(), k = 10, size = 2, trace = FALSE), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(dannet(Species ~ ., data = iris), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5, size = 2, trace = FALSE), throws_error("'bw' must be positive"))
	expect_that(dannet(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50, size = 2, trace = FALSE), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(dannet(formula = Species ~., data = iris, wf = "gaussian", size = 2, bw = rep(1, nrow(iris)), trace = FALSE), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(dannet(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50, size = 2, trace = FALSE), throws_error("'k' must be positive"))

	# k too small
	expect_error(dannet(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005, size = 2, trace = FALSE))

	# k too large
	expect_that(dannet(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50, size = 2, trace = FALSE), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(dannet(formula = Species ~., data = iris, wf = "gaussian", size = 2, k = rep(50, nrow(iris)), trace = FALSE), gives_warning("only first element of 'k' used"))
})


test_that("dannet: weighting schemes work", {
	## wf with finite support
	# fixed bw
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = rectangular(bw = 5), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "rectangular", k = 100, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = rectangular(k = 100), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	is.null(fit1$bw)
	expect_equal(fit1$k, 100)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)
	a <- rep(100, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a) ####

	# fixed bw, only knn
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 100, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 100), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 100)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	a <- rep(100, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only not needed
	expect_that(dannet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE, trace = FALSE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(dannet(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, function(x) sum(x > 0)), a)

	# adaptive bw, only knn
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", k = 100, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = gaussian(k = 100), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 100)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)
	a <- rep(100, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	# adaptive bw, all obs
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE, size = 2, trace = FALSE)
	set.seed(120)
  	fit2 <- dannet(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, function(x) sum(x > 0)), a)

	# fixed bw, only knn
	set.seed(120)
  	fit1 <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 100, size = 2, trace = FALSE)
	set.seed(120)
	fit2 <- dannet(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 100), size = 2, trace = FALSE)
	expect_equal(fit1[-16], fit2[-16])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 100)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	a <- rep(100, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE, size = 2, trace = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})	


#=================================================================================================================
context("predict.dannet")

test_that("predict.dannet works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)  
  	pred <-	predict(fit, newdata = iris[-ran,])
	## y, x
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)  
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## y, x, newdata
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)  
  	pred <- predict(fit, newdata = iris[-ran,-5])
})


test_that("predict.dannet: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, size = 2, trace = FALSE)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, size = 1, trace = FALSE)  
  	pred3 <- predict(fit)
  	pred4 <- predict(fit, newdata = iris[,-5])
  	expect_equal(pred3, pred4)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
  	pred5 <- predict(fit)
  	pred6 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred5, pred6)
	# y, x
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)  
  	pred7 <- predict(fit)
  	pred8 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred7, pred8)
})


test_that("predict.dannet works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100, size = 2, trace = FALSE), gives_warning("group virginica is empty"))
	expect_equal(fit$n[3], 2) ## 2 output units
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(pred$class), 3)
	expect_equal(ncol(pred$posterior), 2)
})


test_that("predict.dannet works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dannet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
	expect_equal(fit$n[1], 1)
	expect_equal(fit$coefnames, "Petal.Width")
	pred <- predict(fit, newdata = iris[-ran,])
})


test_that("predict.dannet works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
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


test_that("predict.dannet works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dannet(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
	expect_equal(fit$n[1], 1)
	expect_equal(fit$coefnames, "Petal.Width")
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
})

   
test_that("predict.dannet: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran, size = 2, trace = FALSE)
	#expect_warning(pred <- predict(fit, newdata = irisna))
	pred <- predict(fit, newdata = irisna)
	expect_equal(all(is.na(pred$class[1:17])), TRUE)
	expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
})


test_that("predict.dannet: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dannet(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})

#=================================================================================================================

context("(da)nnet-specific issues")


test_that("dannet: number of levels and entropy/softmax", {
	library(nnet)
	data(iris)	
	iris2 <- iris[1:100,]
	iris2$Species <- factor(iris2$Species, levels = levels(iris$Species)[1:2])
	
	## 3 classes, all levels present in the training data
	fit <- nnet(Species ~ ., data = iris, size = 2, trace = FALSE)
	expect_equal(length(fit$lev), 3)
	expect_equal(fit$entropy, FALSE)
	expect_equal(fit$softmax, TRUE)

	fit <- dannet(Species ~ ., data = iris, size = 2, bw = 5, trace = FALSE)
	expect_equal(length(fit$lev), 3)
	expect_equal(length(fit$lev1), 3)
	expect_equal(fit$entropy, FALSE)
	expect_equal(fit$softmax, TRUE)

	## 3 classes, 1 level missing in the training data
	expect_that(fit <- nnet(Species ~ ., data = iris, size = 2, trace = FALSE, subset = 1:100), gives_warning("group `virginica` is empty"))
	expect_equal(length(fit$lev), 3)
	expect_equal(fit$entropy, FALSE)
	expect_equal(fit$softmax, TRUE)

	expect_that(fit <- dannet(Species ~ ., data = iris, size = 2, bw = 5, trace = FALSE, subset = 1:100), gives_warning("group virginica is empty"))
	expect_equal(length(fit$lev), 3)
	expect_equal(length(fit$lev1), 2)
	expect_equal(fit$entropy, FALSE)
	expect_equal(fit$softmax, TRUE)

	## 3 classes, 2 levels missing in the training data
	expect_that(fit <- nnet(Species ~ ., data = iris, size = 2, trace = FALSE, subset = 1:50), throws_error("'softmax = TRUE' requires at least two response categories"))

	expect_that(fit <- dannet(Species ~ ., data = iris, size = 2, bw = 5, trace = FALSE, subset = 1:50), throws_error("training data from only one class given"))
	
	## 2 classes, all levels present in the training data
	fit <- nnet(Species ~ ., data = iris2, size = 2, trace = FALSE)
	expect_equal(length(fit$lev), 2)
	expect_equal(fit$entropy, TRUE)
	expect_equal(fit$softmax, FALSE)
	
	fit <- dannet(Species ~ ., data = iris2, size = 2, bw = 5, trace = FALSE)
	expect_equal(length(fit$lev), 2)
	expect_equal(length(fit$lev1), 2)
	expect_equal(fit$entropy, TRUE)
	expect_equal(fit$softmax, FALSE)

	## 2 classes, 1 level missing in the training data
	expect_that(fit <- nnet(Species ~ ., data = iris2, size = 2, trace = FALSE, subset = 1:50), gives_warning("group `versicolor` is empty"))
	expect_equal(length(fit$lev), 2)
	expect_equal(fit$entropy, TRUE)
	expect_equal(fit$softmax, FALSE)
	
	expect_that(fit <- dannet(Species ~ ., data = iris2, size = 2, bw = 5, trace = FALSE, subset = 1:50), throws_error("training data from only one class given"))	
	## not the same behavior
})


test_that("dannet: print.dannet works correctly with formula and data.frame interface", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- dannet(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)
	# print(fit)
	## y, x
	fit <- dannet(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = ran, size = 2, trace = FALSE)  
	# print(fit)
})


#=================================================================================================================

# ###
# ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
# targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
# samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
# ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
            # decay = 5e-4, maxit = 200)
# test.cl <- function(true, pred) {
    # true <- max.col(true)
    # cres <- max.col(pred)
    # table(true, cres)
# }
# test.cl(targets[-samp,], predict(ir1, ir[-samp,]))



# ir2 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
            # decay = 5e-4, maxit = 200, weights = c(rep(2,50), rep(0.1,100))[samp])
            
# options(contrasts = c("contr.treatment", "contr.poly"))
# library(MASS)
# example(birthwt)
# bwt.mu <- multinom(low ~ ., bwt)
# bwt.mu2 <- glm(low ~ ., bwt, family = "binomial")


# w <- rep(1:3, 189/3)
# bwt.mu <- multinom(low ~ ., bwt, weights = w)
# bwt.mu2 <- glm(low ~ ., bwt, family = "binomial", weights = w)
# r = 8
# mask <- c(FALSE, rep(TRUE, r))
# #fit <- nnet.default(X, Y, w, mask=mask, size=0, skip=TRUE,
# #                                entropy=TRUE, rang=0, ...)
# bwt.mu3 <- nnet(low ~ ., bwt, maks = mask, size = 0, skip = TRUE, weights = w)
# bwt.mu
# bwt.mu2
# bwt.mu3$wts
# bwt.mu3$value * 2

# predict(bwt.mu, type = "probs")
# predict(bwt.mu2, type = "response")
# predict(bwt.mu3)
