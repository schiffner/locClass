context("mobSvmModel")

test_that("mobSvmModel: misspecified arguments", {
	# wrong variable names
	expect_that(mob(Species ~ V1 | Sepal.Length, data = iris, model = svmModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'V1' not found"))
	expect_that(mob(Species ~ Sepal.Length | V1, data = iris, model = svmModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'V1' not found"))
	expect_that(mob(y ~ Sepal.Length | Sepal.Width, data = iris, model = svmModel,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("object 'y' not found"))
	# wrong class
	expect_error(mob(iris, data = iris, model = svmModel,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = svmModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("mobSvmModel: binary problem", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(vBayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
})


test_that("mobSvmModel: multi-class problem", {
	data <- benchData::xor3Data(1000)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(xor3BayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.07)	
})


test_that("mobSvmModel throws a warning if grouping variable is numeric", {
	iris$Species <- as.numeric(iris$Species)
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)) 
})


test_that("mobSvmModel works if only one predictor variable is given", {
	expect_that(fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)), gives_warning("some groups are empty"))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(length(terminal[[1]]$model$scale[[1]]), 1)
})


test_that("mobSvmModel: Local and global solution coincide if minsplit is large", {
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 500))
	w <- wsvm(y ~ ., data = as.data.frame(data), kernel = "linear", fitted = FALSE)
	expect_equal(fit@tree$model$coefs, w$coefs)
	expect_equal(fit@tree$model$SV, w$SV)
	expect_equal(fit@tree$model$obj, w$obj)
	pred <- predict(fit)
	p <- predict(w, newdata = as.data.frame(data))
	expect_equal(pred, as.numeric(p))
})


test_that("mobSvmModel: training data from only one class", {
	expect_that(fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris[1:50,], model = svmModel, kernel = "linear", 
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("need training data from at least two classes"))
	## error in global fit
})


test_that("mobSvmModel: additional arguments", {
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$kernel, 0)
	expect_true(is.null(terminal[[1]]$model$fitted))

	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = svmModel, fitted = TRUE,
		kernel = "polynomial", degree = 2, probability = TRUE, control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$kernel, 1)
	expect_equal(terminal[[1]]$model$degree, 2)
	expect_equal(length(terminal[[1]]$model$fitted), 150)
	expect_true(terminal[[1]]$model$compprob)
	expect_equal(length(terminal[[1]]$model$probA), 3)
	expect_equal(length(terminal[[1]]$model$probB), 3)
})


#=================================================================================================================
context("predict.svmModel")

test_that("predict.svmModel works correctly with formula interface and with missing newdata", {
	ran <- sample(1:150,100)
	## formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = svmModel, kernel = "linear", fitted = FALSE,
		probability = TRUE, control = mob_control(objfun = deviance, minsplit = 2))	
  	pred <- predict(fit)
  	pred <- predict(fit, out = "posterior")
	expect_equal(sapply(pred, sum), rep(1, 100))
  	p <- matrix(0, length(pred), 3)
	colnames(p) = levels(iris$Species)
	rownames(p) = sapply(pred, rownames)
	for (i in seq_along(pred)) {
		p[i, colnames(pred[[i]])] = pred[[i]]
	}
  	expect_equal(rownames(p), rownames(iris)[ran])  	
  	pred <- predict(fit, out = "decision")
	## formula, data, newdata
  	predict(fit, newdata = iris[-ran,])
})


test_that("predict.svmModel: retrieving training data works", {
	## no subset
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
})


test_that("predict.svmModel works with missing classes in the training data", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Length, data = iris[1:100,], model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))					
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(length(unique(pred)), 2)
	pred <- predict(fit, newdata = iris[-ran,], out = "posterior")
	expect_equal(ncol(pred[[1]]), 2)
})


test_that("predict.svmModel works with one single predictor variable", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ Sepal.Width | Sepal.Width, data = iris[ran,], model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(length(terminal[[1]]$model$x.scale[["scaled:center"]]), 1)
	expect_equal(length(terminal[[1]]$model$x.scale[["scaled:scale"]]), 1)
	expect_equal(ncol(terminal[[1]]$model$SV), 1)
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.svmModel works with one single test observation", {
	set.seed(123)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 3))
})	


test_that("predict.svmModel: NA handling in newdata works", {
	## NAs in explanatory variables are ok
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = svmModel, kernel = "linear", fitted = FALSE,
		probability = TRUE, control = mob_control(objfun = deviance, minsplit = 10))
	pred <- predict(fit, newdata = irisna)
	expect_true(all(is.na(pred[1:17])))
	pred <- predict(fit, newdata = irisna, out = "posterior")
	expect_true(all(unlist(sapply(pred[1:17], is.na))))
	pred <- predict(fit, newdata = irisna, out = "decision")
	expect_true(all(unlist(sapply(pred[1:17], is.na))))

	## NAs in splitting variable are not ok
	irisna[1:17,1:3] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = ldaModel,
		control = mob_control(objfun = deviance, minsplit = 30))
	expect_error(pred <- predict(fit, newdata = irisna)) 
	## error: VECTOR_ELT() can only be applied to a 'list', not a 'NULL'

})


test_that("predict.svmModel: misspecified arguments", {
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = svmModel, kernel = "linear", fitted = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))	
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})
