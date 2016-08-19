context("mobQdaModel")

test_that("mobQdaModel: misspecified arguments", {
	# wrong variable names
	expect_that(mob(Species ~ V1 | Sepal.Length, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'V1' not found"))
	expect_that(mob(Species ~ Sepal.Length | V1, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'V1' not found"))
	expect_that(mob(y ~ Sepal.Length | Sepal.Width, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'y' not found"))
	# wrong class
	expect_error(mob(iris, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = qdaModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("mobQdaModel: binary problem", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(vBayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
})


test_that("mobQdaModel: multi-class problem", {
	data <- benchData::xor3Data(1000)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 100))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(xor3BayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
})


test_that("mobQdaModel throws a warning if grouping variable is numeric", {
	expect_that(fit <- mob(Petal.Width ~ . | Sepal.Length, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20)), gives_warning("'grouping' was coerced to a factor"))
	### try-error in estfun.wqda: covariance singular
})


test_that("mobQdaModel works if only one predictor variable is given", {
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 50))
	## warnings about empty groups
	## sporadic try-errors in deviance.wqda: covariance singular
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(ncol(terminal[[1]]$model$means), 1)
	expect_equal(dim(terminal[[1]]$model$cov[[1]]), c(1,1))
})


test_that("mobQdaModel: Local and global solution coincide if minsplit is large", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 500))
	w <- wqda(y ~ ., data = as.data.frame(data), method = "ML")
	expect_equal(fit@tree$model$prior, w$prior)
	expect_equal(fit@tree$model$means, w$means)
	expect_equal(fit@tree$model$cov, w$cov)
	pred <- predict(fit)
	p <- predict(w)
	expect_equal(pred, as.numeric(p$class))
})


test_that("mobQdaModel: training data from only one class", {
	expect_that(fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris[1:50,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("training data from only one group given"))
	## error in global fit
})


test_that("mobQdaModel: arguments are passed to wqda",{
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))
	# default: "ML" hard-coded
	expect_equal(fit@tree$right$model$method, "ML")	
	# unbiased
	expect_error(fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = qdaModel, method = "unbiased",
		control = mob_control(objfun = deviance, minsplit = 20)))
})


#=================================================================================================================
context("predict.qdaModel")

test_that("predict.qdaModel works correctly with formula interface and with missing newdata", {
	ran <- sample(1:150,100)
	## formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = qdaModel,
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


test_that("predict.qdaModel: retrieving training data works", {
	## no subset
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))
	## sporadic try-error in deviance.wqda: covariance singular
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
})


test_that("predict.qdaModel works with missing classes in the training data", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Length, data = iris[1:100,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))
	pred <- predict(fit, newdata = iris[-ran,])
	pred <- predict(fit, newdata = iris[-ran,], out = "posterior")
	# expect_equal(nlevels(pred$class), 3)
	# expect_equal(ncol(pred$posterior), 2)
})


test_that("predict.qdaModel works with one single predictor variable", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ Sepal.Width | Sepal.Width, data = iris[ran,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
	## sporadic try-errors in deviance.wqda: covariance singular
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(ncol(terminal[[1]]$model$means), 1)
	expect_equal(dim(terminal[[1]]$model$cov[[1]]), c(1,1))
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.qdaModel works with one single test observation", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
	## sporadic try-errors in deviance.wqda: covariance singular
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 3))
})	


test_that("predict.qdaModel: NA handling in newdata works", {
	set.seed(123)
	## NAs in explanatory variables are ok
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 30))
	## sporadic try-errors in deviance.wqda: covariance singular
	pred <- predict(fit, newdata = irisna) 
	## warnings: kein nicht-fehlendes Argument für max; gebe -Inf zurück
	expect_true(all(is.na(pred[1:17])))

	## NAs in splitting variable are not ok
	irisna[1:17,1:3] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 30))
	## sporadic try-errors in deviance.wqda: covariance singular
	expect_error(pred <- predict(fit, newdata = irisna))
	## error: VECTOR_ELT() can only be applied to a 'list', not a 'NULL'
})


test_that("predict.qdaModel: misspecified arguments", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 20))
	## sporadic try-errors in deviance.wqda: covariance singular
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})
