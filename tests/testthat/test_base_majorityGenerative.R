context("majorityGenerative")

test_that("majorityGenerative: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(majorityGenerative(formula = Species ~ V1, data = iris))
	# wrong class
	expect_error(majorityGenerative(formula = iris, data = iris))
	# target variable also in x
	# expect_error(majorityGenerative(grouping = iris$Species, x = iris))
	# missing x
	expect_error(majorityGenerative(grouping = iris$Species))
})


test_that("majorityGenerative throws a warning if grouping variable is numeric", {
	data(iris)
	expect_warning(majorityGenerative(formula = Petal.Width ~ ., data = iris))
	expect_warning(majorityGenerative(grouping = iris[,1], x = iris[,-1]))
	expect_warning(majorityGenerative(grouping = iris$Petal.Width, x = iris[,-5]))
})


test_that("majorityGenerative works if only one predictor variable is given", {
	data(iris)
	expect_warning(fit <- majorityGenerative(Species ~ Petal.Width, data = iris, subset = 6:60), "group virginica is empty")
	expect_true(inherits(fit, "majorityGenerative"))
})


test_that("majorityGenerative: training data from only one class", {
	data(iris)
	expect_warning(expect_error(majorityGenerative(Species ~ Petal.Width, data = iris, subset = 1:50), "training data from only one group given"))
	expect_warning(expect_error(majorityGenerative(Species ~ Petal.Width, data = iris, subset = 1), "training data from only one group given"))
})


test_that("majorityGenerative: weighting works correctly", {
	data(iris)
	## check if weighted solution with all weights = 1 equals unweighted solution
	fit1 <- majorityGenerative(Species ~ ., data = iris)
	fit2 <- majorityGenerative(Species ~ ., data = iris, weights = rep(1,150))
	expect_equal(fit1[-9],fit2[-9])
	## returned weights	
	a <- rep(1,150)
	names(a) <- 1:150
	expect_equal(fit1$weights, a)
	expect_equal(fit2$weights, a)
	## weights and subsetting
	# formula, data
	expect_warning(fit <- majorityGenerative(Species ~ ., data = iris, subset = 11:60), "group virginica is empty")
	a <- rep(1,50)
	names(a) <- 11:60
	expect_equal(fit$weights, a)
	# formula, data, weights
	expect_warning(fit <- majorityGenerative(Species ~ ., data = iris, weights = rep(1:3, 50), subset = 11:60), "group virginica is empty")
	b <- rep(1:3,50)[11:60]
	names(b) <- 11:60
	expect_equal(fit$weights, b)
	# x, grouping
	expect_warning(fit <- majorityGenerative(x = iris[,-5], grouping = iris$Species, subset = 11:60), "group virginica is empty")
	expect_equal(fit$weights, a)	
	# x, grouping, weights
	expect_warning(fit <- majorityGenerative(x = iris[,-5], grouping = iris$Species, weights = rep(1:3, 50), subset = 11:60), "group virginica is empty")
	expect_equal(fit$weights, b)
	## wrong specification of weights argument
	# weights in a matrix
	weight <- matrix(seq(1:150),nrow=50)
	expect_error(majorityGenerative(Species ~ ., data = iris, weights = weight))
	# weights < 0
	expect_error(majorityGenerative(Species ~ ., data = iris, weights = rep(-5, 150)), "weights have to be larger or equal to zero")
	# weights true/false
	expect_error(majorityGenerative(Species ~ ., data = iris, weights = TRUE))
})


test_that("majorityGenerative: subsetting works", {
	data(iris)
	# formula, data
	expect_warning(fit1 <- majorityGenerative(Species ~ ., data = iris, subset = 1:80), "group virginica is empty")
	expect_warning(fit2 <- majorityGenerative(Species ~ ., data = iris[1:80,]), "group virginica is empty")
	expect_equal(fit1[-9],fit2[-9])
	# formula, data, weights
	fit1 <- majorityGenerative(Species ~ ., data = iris, weights = rep(1:3, each = 50), subset = 1:80)
	fit2 <- majorityGenerative(Species ~ ., data = iris[1:80,], weights = rep(1:3, each = 50)[1:80])
	expect_equal(fit1[-9],fit2[-9])
	expect_equal(length(fit1$weights), 80)
	# x, grouping
	fit1 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = 1:80)
	fit2 <- majorityGenerative(grouping = iris$Species[1:80], x = iris[1:80,-5])
	expect_equal(fit1[-9],fit2[-9])
	# x, grouping, weights
	fit1 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], weights = rep(1:3, each = 50), subset = 1:80)
	fit2 <- majorityGenerative(grouping = iris$Species[1:80], x = iris[1:80,-5], weights = rep(1:3, each = 50)[1:80])
	expect_equal(fit1[-9],fit2[-9])
	expect_equal(length(fit1$weights), 80)
	# wrong specification of subset argument
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = iris[1:10,]))
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = FALSE))
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = 0))
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = -10:50))
})


test_that("majorityGenerative: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = irisna, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = irisna, subset = 6:60, na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = irisna, subset = 11:60)
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## formula, data, weights
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = irisna, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = irisna, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = irisna, subset = 11:60, weights = rep(1:3, 50))
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping
	# na.fail
	expect_error(majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.omit)
	fit2 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 11:60)
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping, weights
	# na.fail
	expect_error(majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 11:60, weights = rep(1:3, 50))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	
	### NA in grouping
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = irisna, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = irisna, subset = 6:60, na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = irisna, subset = 11:60)
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## formula, data, weights
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = irisna, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = irisna, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = irisna, subset = 11:60, weights = rep(1:3, 50))
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping
	# na.fail
	expect_error(majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, na.action = na.omit)
	fit2 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 11:60)
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping, weights
	# na.fail
	expect_error(majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 6:60, weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- majorityGenerative(grouping = irisna$Species, x = irisna[,-5], subset = 11:60, weights = rep(1:3, 50))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)

	### NA in weights
	weights <- rep(1:3,50)
	weights[1:10] <- NA
	## formula, data, weights
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = 6:60, weights = weights, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = iris, subset = 6:60, weights = weights, na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = iris, subset = 11:60, weights = weights)
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping, weights
	# na.fail
	expect_error(majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = 6:60, weights = weights, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = 6:60, weights = weights, na.action = na.omit)
	fit2 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = 11:60, weights = weights)
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = iris, subset = subset, na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = iris, subset = 11:60)
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## formula, data, weights
	# na.fail
	expect_error(majorityGenerative(Species ~ ., data = iris, subset = subset, weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(Species ~ ., data = iris, subset = subset, weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- majorityGenerative(Species ~ ., data = iris, subset = 11:60, weights = rep(1:3, 50))
	expect_equal(fit1[-c(9,12)], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping
	# na.fail
	expect_error(majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = subset, na.action = na.omit)
	fit2 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = 11:60)
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)
	## x, grouping, weights
	# na.fail
	expect_error(majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = subset, weights = rep(1:3, 50), na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = subset, weights = rep(1:3, 50), na.action = na.omit)
	fit2 <- majorityGenerative(grouping = iris$Species, x = iris[,-5], subset = 11:60, weights = rep(1:3, 50))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(length(fit1$weights), 50)
})


#=================================================================================================================
context("predict.majorityGenerative")

test_that("predict.majorityGenerative works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- majorityGenerative(formula = Species ~ ., data = iris, subset = ran)  
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
  	expect_equal(names(pred$class), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- majorityGenerative(formula = Species ~ ., data = iris, subset = ran)  
  	pred <- predict(fit, newdata = iris[-ran,])
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])  	
  	expect_equal(names(pred$class), rownames(iris)[-ran])  	
	## grouping, x
	fit <- majorityGenerative(x = iris[,-5], grouping = iris$Species, subset = ran)  
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
  	expect_equal(names(pred$class), rownames(iris)[ran])  	
	## grouping, x, newdata
	fit <- majorityGenerative(x = iris[,-5], grouping = iris$Species, subset = ran)  
  	pred <- predict(fit, newdata = iris[-ran,-5])
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])  	
  	expect_equal(names(pred$class), rownames(iris)[-ran])  	
})


test_that("predict.majorityGenerative: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- majorityGenerative(formula = Species ~ ., data = iris)
	set.seed(120)
  	pred1 <- predict(fit)
	set.seed(120)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- majorityGenerative(x = iris[,-5], grouping = iris$Species)  
	set.seed(120)
  	pred1 <- predict(fit)
	set.seed(120)
  	pred2 <- predict(fit, newdata = iris[,-5])
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- majorityGenerative(formula = Species ~ ., data = iris, subset = ran)
	set.seed(120)
  	pred1 <- predict(fit)
	set.seed(120)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
	# y, x
	fit <- majorityGenerative(x = iris[,-5], grouping = iris$Species, subset = ran)  
	set.seed(120)
  	pred1 <- predict(fit)
	set.seed(120)
  	pred2 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred1, pred2)
})


test_that("predict.majorityGenerative works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- majorityGenerative(Species ~ ., data = iris, subset = 1:100)
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(pred$class), 3)
	expect_equal(ncol(pred$posterior), 2)
	# a <- rep(0,50)
	# names(a) <- rownames(pred$posterior)
	# expect_equal(pred$posterior[,3], a)
})


test_that("predict.majorityGenerative works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- majorityGenerative(Species ~ Petal.Width, data = iris, subset = ran)
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.majorityGenerative works with one single test observation", {
	set.seed(120)
	data(iris)
	ran <- sample(1:150,100)
	fit <- majorityGenerative(Species ~ Petal.Width, data = iris, subset = ran)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("virginica", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred$class, a)
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("virginica", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred$class, a)
})	


test_that("predict.majorityGenerative works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- majorityGenerative(Species ~ Petal.Width, data = iris, subset = ran)
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
})


## todo
# test_that("predict.majorityGenerative: NA handling in newdata works", {
	# data(iris)
	# ran <- sample(1:150,100)
	# irisna <- iris
	# irisna[1:17,c(1,3)] <- NA
	# fit <- majorityGenerative(Species ~ ., data = iris, subset = ran)
	# expect_warning(pred <- predict(fit, newdata = irisna))
	# expect_equal(all(is.na(pred$class[1:17])), TRUE)
	# expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
# })


test_that("predict.majorityGenerative: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- majorityGenerative(Species ~ Petal.Width, data = iris, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})
