context("wsvm")

test_that("wsvm: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(wsvm(formula = Species ~ V1, data = iris))
	# wrong class
	expect_error(wsvm(formula = iris, data = iris))
	# target variable also in x
	expect_error(wsvm(y = iris$Species, x = iris))          ## funktioniert, sollte aber nicht
	# missing x
	expect_error(wsvm(y = iris$Species))
})


test_that("wsvm works if only one predictor variable is given", {
	data(iris)
	fit <- wsvm(Species ~ Petal.Width, data = iris, subset = 6:60)
	expect_equal(ncol(fit$SV), 1)
})
	

test_that("wsvm: training data from only one class", {
	data(iris)
	## y factor
	expect_that(wsvm(Species ~ ., data = iris, subset = 1:50), throws_error("need training data from at least two classes"))
	#expect_that(wsvm(Species ~ ., data = iris, subset = 1), throws_error("training data from only one class"))
	expect_that(wsvm(y = iris$Species, x = iris[,-5], subset = 1:50), throws_error("need training data from at least two classes"))
	## y integer & type = C-classification
	irisint <- iris
	irisint$Species <- as.numeric(irisint$Species)
	expect_that(wsvm(Species ~ ., data = irisint, subset = 1:50, type = "C-classification"), throws_error("need training data from at least two classes"))
	#expect_that(wsvm(Species ~ ., data = irisint, subset = 1, type = "C-classification"), throws_error("training data from only one class"))
	expect_that(wsvm(y = irisint$Species, x = irisint[,-5], subset = 1:50, type = "C-classification"), throws_error("need training data from at least two classes"))
	## y integer & type = nu-classification
	expect_that(wsvm(Species ~ ., data = irisint, subset = 1:50, type = "nu-classification"), throws_error("need training data from at least two classes"))
	#expect_that(wsvm(Species ~ ., data = irisint, subset = 1, type = "nu-classification"), throws_error("training data from only one class"))
	expect_that(wsvm(y = irisint$Species, x = irisint[,-5], subset = 1:50, type = "C-classification"), throws_error("need training data from at least two classes"))
})


test_that("wsvm.default and wsvm.formula yield the same results", {
	data(iris)	
	x <- iris[,-5]
	y <- iris$Species

	# neither subset nor case.weights
	fit1 <- wsvm(formula = Species ~ ., data = iris)
	fit2 <- wsvm(x = x, y = y)
	expect_equal(fit1[-c(1,32)], fit2[-1])

	# subset
	fit1 <- wsvm(Species ~ ., data = iris, subset = seq(1,150,3))
	fit2 <- wsvm(x = x, y = y, subset = seq(1,150,3))
	expect_equal(fit1[-c(1,32)], fit2[-1])

	# case.weights
	fit1 <- wsvm(formula = Species ~ ., data = iris, case.weights = 1:150)
	fit2 <- wsvm(x = x, y = y, case.weights = 1:150)
	expect_equal(fit1[-c(1,32)], fit2[-1])

	# subset & case.weights
	fit1 <- wsvm(Species ~ ., data = iris, subset = seq(1,150,3), case.weights = 1:150)
	fit2 <- wsvm(x = x, y = y, subset = seq(1,150,3), case.weights = 1:150)
	expect_equal(fit1[-c(1,32)], fit2[-1])
})


test_that("wsvm: weighting works correctly", {
	data(iris)
	## check if weighted solution with all weights = 1 equals unweighted solution
	fit1 <- wsvm(Species ~ ., data = iris)
	fit2 <- wsvm(Species ~ ., data = iris, weights = rep(1,150))
	expect_equal(fit1[-1],fit2[-1])
	## returned weights
	a <- rep(1,150)
	names(a) <- 1:150
	expect_equal(fit1$case.weights, a)
	expect_equal(fit2$case.weights, a)
	## weights and subsetting
	# formula, data
	fit <- wsvm(Species ~ ., data = iris, subset = 11:60)
	a <- rep(1,50)
	names(a) <- 11:60
	expect_equal(fit$case.weights, a)
	# formula, data, weights
	fit <- wsvm(Species ~ ., data = iris, case.weights = rep(1:3, 50), subset = 11:60)
	b <- rep(1:3,50)[11:60]
	names(b) <- 11:60
	expect_equal(fit$case.weights, b)
	# x, grouping
	fit <- wsvm(x = iris[,-5], y = iris$Species, subset = 11:60)
	expect_equal(fit$case.weights, a)	
	# x, grouping, weights
	fit <- wsvm(x = iris[,-5], y = iris$Species, case.weights = rep(1:3, 50), subset = 11:60)
	expect_equal(fit$case.weights, b)
	## wrong specification of weights argument
	# weights in a matrix
	weight <- matrix(seq(1:150),nrow=50)
	expect_error(wsvm(Species ~ ., data = iris, case.weights = weight))
	# weights < 0
	expect_error(wsvm(Species ~ ., data = iris, case.weights = rep(-5, 150)))
	# weights true/false
	expect_error(wsvm(Species ~ ., data = iris, case.weights = TRUE))
})


test_that("wsvm: subsetting works", {
	data(iris)
	# formula, data
	fit1 <- wsvm(Species ~ ., data = iris, subset = 1:80)
	fit2 <- wsvm(Species ~ ., data = iris[1:80,])
	expect_equal(fit1[-1], fit2[-1])
	# formula, data, weights
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = rep(1:3, each = 50), subset = 1:80)
	fit2 <- wsvm(Species ~ ., data = iris[1:80,], case.weights = rep(1:3, each = 50)[1:80])
	expect_equal(fit1[-1],fit2[-1])
	expect_equal(length(fit1$case.weights), 80)
	# x, grouping
	fit1 <- wsvm(y = iris$Species, x = iris[,-5], subset = 1:80)
	fit2 <- wsvm(y = iris$Species[1:80], x = iris[1:80,-5])
	expect_equal(fit1[-1], fit2[-1])
	# x, grouping, weights
	fit1 <- wsvm(y = iris$Species, x = iris[,-5], case.weights = rep(1:3, each = 50), subset = 1:80)
	fit2 <- wsvm(y = iris$Species[1:80], x = iris[1:80,-5], case.weights = rep(1:3, each = 50)[1:80])
	expect_equal(fit1[-1], fit2[-1])
	expect_equal(length(fit1$case.weights), 80)
	# wrong specification of subset argument
	expect_error(wsvm(Species ~ ., data = iris, subset = iris[1:10,]))
	expect_error(wsvm(Species ~ ., data = iris, subset = FALSE))
	expect_error(wsvm(Species ~ ., data = iris, subset = 0))
	expect_error(wsvm(Species ~ ., data = iris, subset = -10:50))
})


test_that("wsvm: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_error(wsvm(Species ~ ., data = irisna, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = irisna, subset = 6:60, na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = irisna, subset = 11:60)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	a <- rep(1, 50)
	names(a) <- 11:60
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], a)
	expect_equal(fit2$case.weights, a)
	## formula, data, weights
	# na.fail
	expect_error(wsvm(Species ~ ., data = irisna, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = irisna, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = irisna, subset = 11:60, case.weights = rep(1:3, 50))
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	b <- rep(1:3,50)[11:60]
	names(b) <- 11:60
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)
	## x, grouping
	# na.fail
	expect_error(wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.omit)
	fit2 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 11:60)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], a)
	expect_equal(fit2$case.weights, a)
	## x, grouping, weights
	# na.fail
	expect_error(wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 11:60, case.weights = rep(1:3, 50))
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)
	
	### NA in grouping
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_error(wsvm(Species ~ ., data = irisna, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = irisna, subset = 6:60, na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = irisna, subset = 11:60)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], a)
	expect_equal(fit2$case.weights, a)
	## formula, data, weights
	# na.fail
	expect_error(wsvm(Species ~ ., data = irisna, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = irisna, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = irisna, subset = 11:60, case.weights = rep(1:3, 50))
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)
	## x, grouping
	# na.fail
	expect_error(wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.omit)
	fit2 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 11:60)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], a)
	expect_equal(fit2$case.weights, a)
	## x, grouping, weights
	# na.fail
	expect_error(wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- wsvm(y = irisna$Species, x = irisna[,-5], subset = 11:60, case.weights = rep(1:3, 50))
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)

	### NA in weights
	weights <- rep(1:3,50)
	weights[1:10] <- NA
	## formula, data, weights
	# na.fail
	expect_error(wsvm(Species ~ ., data = iris, subset = 6:60, case.weights = weights, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = iris, subset = 6:60, case.weights = weights, na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = iris, subset = 11:60, case.weights = weights)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)
	## x, grouping, weights
	# na.fail
	expect_error(wsvm(y = iris$Species, x = iris[,-5], subset = 6:60, case.weights = weights, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = iris$Species, x = iris[,-5], subset = 6:60, case.weights = weights, na.action = na.omit)
	fit2 <- wsvm(y = iris$Species, x = iris[,-5], subset = 11:60, case.weights = weights)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_error(wsvm(Species ~ ., data = iris, subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = iris, subset = subset, na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = iris, subset = 11:60)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], a)
	expect_equal(fit2$case.weights, a)
	## formula, data, weights
	# na.fail
	expect_error(wsvm(Species ~ ., data = iris, subset = subset, case.weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(Species ~ ., data = iris, subset = subset, case.weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- wsvm(Species ~ ., data = iris, subset = 11:60, case.weights = rep(1:3, 50))
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)
	## x, grouping
	# na.fail
	expect_error(wsvm(y = iris$Species, x = iris[,-5], subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = iris$Species, x = iris[,-5], subset = subset, na.action = na.omit)
	fit2 <- wsvm(y = iris$Species, x = iris[,-5], subset = 11:60)
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], a)
	expect_equal(fit2$case.weights, a)
	## x, grouping, weights
	# na.fail
	expect_error(wsvm(y = iris$Species, x = iris[,-5], subset = subset, case.weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wsvm(y = iris$Species, x = iris[,-5], subset = subset, case.weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- wsvm(y = iris$Species, x = iris[,-5], subset = 11:60, case.weights = rep(1:3, 50))
	expect_equal(fit1[-c(1,28,31)], fit2[-c(1,28,31)])
	expect_equal(fit1$case.weights[1:length(fit1$case.weights)], b)
	expect_equal(fit2$case.weights, b)
})


#=================================================================================================================
context("predict.wsvm")

test_that("predict.wsvm works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- wsvm(formula = Species ~ ., data = iris, subset = ran, probability = TRUE)  
  	pred <- predict(fit, probability = TRUE, decision.values = TRUE) # keine probs/decision
  	expect_equal(names(pred), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- wsvm(formula = Species ~ ., data = iris, subset = ran, probability = TRUE)  
  	predict(fit, newdata = iris[-ran,], probability = TRUE, decision.values = TRUE)
	## grouping, x
	fit <- wsvm(x = iris[,-5], y = iris$Species, subset = ran, probability = TRUE)  
  	pred <- predict(fit, probability = TRUE, decision.values = TRUE) ## keine probs/decision
  	expect_equal(names(pred), rownames(iris)[ran])  	
	## grouping, x, newdata
	fit <- wsvm(x = iris[,-5], y = iris$Species, subset = ran, probability = TRUE)  
  	predict(fit, newdata = iris[-ran,-5], probability = TRUE, decision.values = TRUE)
})


test_that("predict.wsvm: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- wsvm(formula = Species ~ ., data = iris)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- wsvm(x = iris[,-5], y = iris$Species)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[,-5])
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- wsvm(formula = Species ~ ., data = iris, subset = ran)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
	# y, x
	fit <- wsvm(x = iris[ran,-5], y = iris$Species[ran])  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred1, pred2)
})


test_that("predict.wsvm works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- wsvm(Species ~ ., data = iris, subset = 1:100) ## no warning
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(pred), 3)

	fit <- wsvm(Species ~ ., data = iris, subset = 1:100, probability = TRUE) ## no warning
	pred <- predict(fit, newdata = iris[-ran,], probability = TRUE)

	expect_equal(nlevels(pred), 3)
	posterior <- attr(pred, "probabilities")
	expect_equal(ncol(posterior), 2)
	# a <- rep(0,50)
	# names(a) <- rownames(posterior)
	# expect_equal(posterior[,3], a)
})


test_that("predict.wsvm works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- wsvm(Species ~ Petal.Width, data = iris, subset = ran)
	#expect_equal(ncol(fit$means), 1)
	#expect_equal(dim(fit$cov), rep(1, 2))
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.wsvm works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- wsvm(Species ~ Petal.Width, data = iris, subset = ran, probability = TRUE)
  	pred <- predict(fit, newdata = iris[5,], probability = TRUE)
	posterior <- attr(pred, "probabilities")
	expect_equal(length(pred), 1)
	expect_equal(dim(posterior), c(1, 3))
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) <- 5
	attr(a, "probabilities") <- posterior
	expect_equal(pred, a) ##???
	pred <- predict(fit, newdata = iris[58,], probability = TRUE)
	posterior <- attr(pred, "probabilities")
	expect_equal(length(pred), 1)
	expect_equal(dim(posterior), c(1, 3))
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) <- 58
	attr(a, "probabilities") <- posterior	
	expect_equal(pred, a)
})	


test_that("predict.wsvm works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- wsvm(Species ~ Petal.Width, data = iris, subset = ran, probability = TRUE)
	expect_equal(ncol(fit$SV), 1)
	pred <- predict(fit, newdata = iris[5,], probability = TRUE)
	expect_equal(length(pred), 1)
	posterior <- attr(pred, "probabilities")
	expect_equal(dim(posterior), c(1, 3))
})


test_that("predict.wsvm: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- wsvm(Species ~ ., data = iris, subset = ran)
	pred <- predict(fit, newdata = irisna) ## default: na.omit
	expect_equal(length(pred), 133)
	expect_error(pred <- predict(fit, newdata = irisna, na.action = na.fail))
})


test_that("predict.wsvm: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- wsvm(Species ~ Petal.Width, data = iris, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
    # errors in prior
})
