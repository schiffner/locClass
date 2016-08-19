context("mobMajorityModel")

test_that("mobMajorityModel: misspecified arguments", {
	# wrong variable names
	expect_that(mob(Species ~ V1 | Sepal.Length, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'V1' not found"))
	expect_that(mob(Species ~ Sepal.Length | V1, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'V1' not found"))
	expect_that(mob(y ~ Sepal.Length | Sepal.Width, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'y' not found"))
	# wrong class
	expect_error(mob(iris, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = majorityModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("mobMajorityModel: binary problem", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 50))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::vBayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
})


test_that("mobMajorityModel: multi-class problem", {
	data <- benchData::xor3Data(1000)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 50))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::xor3BayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
## does not work for xor problem !!!
})


test_that("mobMajorityModel throws a warning if grouping variable is numeric", {
	expect_that(fit <- mob(Petal.Width ~ . | Sepal.Length, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20)), gives_warning("'grouping' was coerced to a factor"))
})


test_that("mobMajorityModel works if only one predictor variable is given", {
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20))
})


test_that("mobMajorityModel: Local and global solution coincide if minsplit is large", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 500))
	w <- majority(y ~ ., data = as.data.frame(data))
	expect_equal(fit@tree$model$prior, w$prior)
	pred <- predict(fit)
	p <- predict(w)
	expect_equal(pred, as.numeric(p$class))
})


test_that("mobMajorityModel: training data from only one class", {
	expect_that(fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris[1:50,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("training data from only one group given"))
	## error in global fit (later on nodes containing observations from one single class are, of course, allowed)
})


#=================================================================================================================
context("predict.majorityModel")

test_that("predict.majorityModel works correctly with formula interface and with missing newdata", {
	ran <- sample(1:150,100)
	## formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 5))
  	pred <- predict(fit)
	mean(pred != as.numeric(iris$Species[ran]))
  	pred <- predict(fit, out = "posterior")
  	p <- matrix(0, length(pred), 3)
	colnames(p) = levels(iris$Species)
	rownames(p) = sapply(pred, rownames)
	for (i in seq_along(pred)) {
		p[i, colnames(pred[[i]])] = pred[[i]]
	}
  	expect_equal(rownames(p), rownames(iris)[ran])  	
	## formula, data, newdata
	p <- predict(fit, newdata = iris[-ran,])
	mean(p != as.numeric(iris$Species[-ran]))
})


test_that("predict.majorityModel: retrieving training data works", {
	## no subset
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 2,))
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
})


test_that("predict.majorityModel works with missing classes in the training data", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Length, data = iris[1:60,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 2))
	# leading minor not positive definite: reason: pure terminal node
	pred <- predict(fit, newdata = iris[-ran,])
	pred <- predict(fit, newdata = iris[-ran,], out = "posterior")
	# expect_equal(nlevels(pred$class), 3)
	# expect_equal(ncol(pred$posterior), 2)
})


test_that("predict.majorityModel works with one single predictor variable", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ Sepal.Width | Sepal.Width, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
	# expect_equal(ncol(fit$means), 1)
	# expect_equal(dim(fit$cov), rep(1, 2))
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.majorityModel works with one single test observation", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ Sepal.Width | Sepal.Width, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior")
	expect_equal(nrow(pred[[1]]), 1)
})	


test_that("predict.majorityModel: NA handling in newdata works", {
	## NAs in explanatory variables are ok
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 30))
	pred <- predict(fit, newdata = irisna) 
	## no NAs in pred since in majority model the explanatory variables are not used for prediction
	
	## NAs in splitting variable are not ok
	irisna[1:17,1:3] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 30))
	expect_error(pred <- predict(fit, newdata = irisna)) 
	## error: VECTOR_ELT() can only be applied to a 'list', not a 'NULL'
})


test_that("predict.majorityModel: misspecified arguments", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = majorityModel,
		control = mob_control(objfun = deviance, minsplit = 20))	
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})
