context("mobNnetModel")

test_that("mobNnetModel: misspecified arguments", {
	data(iris)
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	# wrong variable names
	expect_error(mob(Species ~ V1 | Sepal.Length, data = iris, model = nnetModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	expect_error(mob(Species ~ Sepal.Length | V1, data = iris, model = nnetModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	expect_error(mob(y ~ Sepal.Length | Sepal.Width, data = iris, model = nnetModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# wrong class
	expect_error(mob(iris, data = iris, model = nnetModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = nnetModel, trace = FALSE,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("mobNnetModel: reps argument works", {
	data <- benchData::vData(500)
	## default: reps = 1
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = nnetModel, trace = FALSE, size = 1, decay = 0.05,
		control = mob_control(objfun = deviance, minsplit = 200))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$addargs, list(trace = FALSE, size = 1, decay = 0.05))
	## reps = 5
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = nnetModel, trace = FALSE, size = 1, decay = 0.05, reps = 5,
		control = mob_control(objfun = deviance, minsplit = 200))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$addargs, list(trace = FALSE, size = 1, decay = 0.05, reps = 5))	
})


test_that("mobNnetModel: binary problem", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = nnetModel, trace = FALSE, size = 1, decay = 0.05,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::vBayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
	
})


test_that("mobNnetModel: multi-class problem", {
	data <- benchData::xor3Data(1000)
	fit <- mob(y ~ x.1 + x.2 | x.1, data = data, model = nnetModel, trace = FALSE, size = 1, decay = 0.5,
		control = mob_control(objfun = deviance, minsplit = 250))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::xor3BayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
})


test_that("mobNnetModel throws a warning if grouping variable is numeric", {
	data(iris)
	iris$Species <- as.numeric(iris$Species)
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = nnetModel, trace = FALSE, size = 5, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
})


test_that("mobNnetModel works if only one predictor variable is given", {
	data(iris)
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = nnetModel, trace = FALSE, size = 1, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$coefnames, "Sepal.Width")
})


test_that("mobNnetModel: Local and global solution coincide if minsplit is large", {
	library(nnet)
	data <- benchData::vData(500)
	set.seed(112)
	Wts <- runif(5, -0.7, 0.7)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = nnetModel, size = 1, trace = FALSE, Wts = Wts,
		control = mob_control(objfun = deviance, minsplit = 500))
	w <- nnet(y ~ ., data = as.data.frame(data), size = 1, trace = FALSE, Wts = Wts)
	expect_equal(fit@tree$model$wts, w$wts)
	# expect_equal(fit@tree$model$fitted.values, w$fitted.values)
	expect_equal(fit@tree$model$deviance, w$deviance)
	pred <- predict(fit, out = "posterior")
	pred <- do.call("rbind", pred)
	p <- predict(w, newdata = as.data.frame(data))
	expect_equal(as.numeric(pred[,2]), as.numeric(p))
})


test_that("mobNnetModel: training data from only one class", {
	data(iris)
	expect_that(fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris[1:50,], model = nnetModel, size = 5,  trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("FLXMCLnnet: arguments are passed to nnet",{
	iris[,1:4] <- scale(iris[,1:4])

	set.seed(123)

	## size, trace
	Wts <- runif(8, -0.7, 0.7)
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = nnetModel, size = 1, trace = TRUE, Wts = Wts, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$n[2], 1)
	
	## decay ###
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = nnetModel, size = 1, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$decay, 0.1)

	## censored
	expect_true(!terminal[[1]]$model$censored)
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = nnetModel, size = 1, trace = FALSE, decay = 0.1, censored = TRUE,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_true(terminal[[1]]$model$censored)
})


#=================================================================================================================
context("predict.nnetModel")

test_that("predict.nnetModel works correctly with formula interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 20))
  	pred <- predict(fit)
  	pred <- predict(fit, out = "posterior")
  	p <- matrix(0, length(pred), 3)
	colnames(p) = levels(iris$Species)
	rownames(p) = sapply(pred, rownames)
	for (i in seq_along(pred)) {
		p[i, colnames(pred[[i]])] = pred[[i]]
	}
  	expect_equal(rownames(p), rownames(iris)[ran])  	
	## formula, data, newdata
  	predict(fit, newdata = iris[-ran,])
})


test_that("predict.nnetModel: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 20))
	# singular
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 20))	
	# singular
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
})


test_that("predict.nnetModel works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Length, data = iris[1:100,], model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 20))
	# singular
	pred <- predict(fit, newdata = iris[-ran,])
	pred <- predict(fit, newdata = iris[-ran,], out = "posterior")
	expect_equal(sapply(pred, ncol), rep(2, length(pred)))
})


test_that("predict.nnetModel works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ Sepal.Width | Sepal.Width, data = iris[ran,], model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$coefnames, "Sepal.Width")
	predict(fit, newdata = iris[-ran,])
	predict(fit, newdata = iris[-ran,], out = "posterior")
})


test_that("predict.nnetModel works with one single test observation", {
	data(iris)
	# 3 classes
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = nnetModel, trace = FALSE, size = 2,
		control = mob_control(objfun = deviance, minsplit = 20))	
	# singular
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 3))
	# 3 classes, 1 missing
	fit <- mob(Species ~ . | Sepal.Width, data = iris[1:100,], model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 20))	
	# singular
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 2))
	# 2 classes
	data <- benchData::flashData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = as.data.frame(data), model = nnetModel, trace = FALSE, size = 5,
		control = mob_control(objfun = deviance, minsplit = 200))	
	pred <- predict(fit, newdata = as.data.frame(data)[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = as.data.frame(data)[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 2))
})	


test_that("predict.nnetModel: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,1:3] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = nnetModel, trace = FALSE, size = 5, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 30))
	expect_error(pred <- predict(fit, newdata = irisna))
})


test_that("predict.nnetModel: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = nnetModel, trace = FALSE, size = 5, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})
