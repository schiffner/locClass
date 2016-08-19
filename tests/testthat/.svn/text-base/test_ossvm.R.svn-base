context("ossvm")

test_that("ossvm: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(ossvm(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10))
	# wrong class
	expect_error(ossvm(formula = iris, data = iris, wf = "gaussian", bw = 10))
	expect_error(ossvm(iris, data = iris, wf = "gaussian", bw = 10))
	# target variable also in x
	expect_error(ossvm(y = iris$Species, x = iris, wf = "gaussian", bw = 10))
	expect_warning(ossvm(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10))           ## warning, Species on RHS removed
	# missing x
	expect_error(ossvm(y = iris$Species, wf = "gaussian", bw = 10))
})


# test_that("ossvm throws a warning if y variable is numeric", {
	# data(iris)
	# formula, data
	# expect_that(ossvm(formula = as.numeric(Species) ~ ., data = iris, wf = "gaussian", bw = 10), gives_warning("'y' was coerced to a factor"))
	# y, x
	# expect_that(ossvm(y = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10), gives_warning("'y' was coerced to a factor"))
# })


test_that("ossvm works if only one predictor variable is given", {
	data(iris)
	fit <- ossvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5)
	predict(fit)
})


test_that("ossvm: training data from only one class", {
	data(iris)
	expect_that(ossvm(Species ~ ., data = iris, bw = 2, subset = 1:50), throws_error("training data from only one class"))
	expect_error(ossvm(Species ~ ., data = iris, bw = 2, subset = 1))
	expect_that(ossvm(y = iris$Species, x = iris[,-5], bw = 2, subset = 1:50), throws_error("training data from only one class"))
	expect_error(ossvm(y = iris$Species, x = iris[,-5], bw = 2, subset = 1))
})


test_that("ossvm: subsetting works", {
	data(iris)
	# formula, data
	expect_that(fit1 <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2), gives_warning("group virginica is empty"))
	expect_equal(fit1[-1],fit2[-1])
	expect_equal(nrow(fit1$x), 80)
	expect_equal(length(fit1$y), 80)
	# x, y
	expect_that(fit1 <- ossvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(y = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2), gives_warning("group virginica is empty"))
	expect_equal(fit1[-1],fit2[-1])
	expect_equal(nrow(fit1$x), 80)
	expect_equal(length(fit1$y), 80)
	# wrong specification of subset argument
	expect_error(ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,]))
	expect_error(ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE)) #???
	expect_error(ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0)) #???
	expect_error(ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50))
})


test_that("ossvm: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(ossvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- ossvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(1:2,31)], fit2[-c(1:2,31)])
	expect_equivalent(fit1[2], fit2[2])

	## x, y
	# na.fail
	expect_that(ossvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- ossvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))##
	expect_that(fit2 <- ossvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(1:2,31)], fit2[-c(1:2,31)])
	expect_equivalent(fit1[2], fit2[2])
	
	### NA in y
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(ossvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- ossvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(1:2,31)], fit2[-c(1:2,31)])
	expect_equivalent(fit1[2], fit2[2])
	## x, y
	# na.fail
	expect_that(ossvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- ossvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(1:2,31)], fit2[-c(1:2,31)])
	expect_equivalent(fit1[2], fit2[2])

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(1:2,31)], fit2[-c(1:2,31)])
	expect_equivalent(fit1[2], fit2[2])
	## x, y
	# na.fail
	expect_that(ossvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- ossvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit), gives_warning("group virginica is empty"))
	expect_that(fit2 <- ossvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty"))
	expect_equal(fit1[-c(1:2,31)], fit2[-c(1:2,31)])
	expect_equivalent(fit1[2], fit2[2])
})


test_that("ossvm: try all weight functions", {
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, probability = TRUE)    
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = gaussian(2), probability = TRUE)    
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, probability = TRUE)    
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = gaussian(2), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)
	
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 5, k = 30, probability = TRUE)    
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = gaussian(bw = 5, k = 30), probability = TRUE)    
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 5, k = 30, probability = TRUE)    
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = gaussian(bw = 5, k = 30), probability = TRUE)
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	# pred1 <- predict(fit1, newdata = iris[sample(1:150),], probability = TRUE, decision.values = TRUE)
	# pred2 <- predict(fit2, newdata = iris[1:10,], probability = TRUE, decision.values = TRUE)
	# pred3 <- predict(fit3, newdata = iris[1,-5], probability = TRUE, decision.values = TRUE)
	# pred4 <- predict(fit4, newdata = iris[1,-5], probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)
	
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 30, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 30), probability = TRUE)
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "epanechnikov", bw = 5, k = 30, probability = TRUE)
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = epanechnikov(5, 30), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 30, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 30), probability = TRUE)
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "rectangular", bw = 5, k = 30, probability = TRUE)
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = rectangular(5, 30), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 30, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = triangular(5, k = 30), probability = TRUE)
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "triangular", bw = 5, k = 30, probability = TRUE)
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = triangular(5, 30), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = biweight(5), probability = TRUE)
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "biweight", bw = 5, probability = TRUE)
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = biweight(5), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 30, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = optcosine(5, k = 30), probability = TRUE)
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "optcosine", bw = 5, k = 30, probability = TRUE)
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = optcosine(5, 30), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)

	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = cosine(5, k = 30), probability = TRUE)
	fit3 <- ossvm(x = iris[,-5], y = iris$Species, wf = "cosine", bw = 5, k = 30, probability = TRUE)
	fit4 <- ossvm(x = iris[,-5], y = iris$Species, wf = cosine(5, 30), probability = TRUE)    
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit3[-c(1,4)], fit4[-c(1,4)])
	expect_equal(fit2[-c(1,2,32)], fit4[-c(1,2)])
	expect_equivalent(fit2[2], fit4[2])
	set.seed(120)
	pred1 <- predict(fit1, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred2 <- predict(fit2, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred3 <- predict(fit3, probability = TRUE, decision.values = TRUE)
	set.seed(120)
	pred4 <- predict(fit4, probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred3, pred4)
	expect_equal(pred2, pred4)
})


test_that("ossvm: local solution with rectangular window function and large bw and global solution coincide", {
	data(iris)
	library(e1071)
	## newdata missing
	fit1 <- wsvm(formula = Species ~ ., data = iris)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = rectangular(20))
	fit3 <- svm(Species ~ ., data = iris)
	pred1 <- predict(fit1)
	pred2 <- predict(fit2)
	pred3 <- predict(fit3)
	expect_equal(pred1, pred2)
	expect_equal(pred1, pred3)
	## newdata given
	fit1 <- wsvm(formula = Species ~ ., data = iris, probability = TRUE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = rectangular(8), probability = TRUE)
	fit3 <- svm(Species ~ ., data = iris, probability = TRUE)
	pred1 <- predict(fit1, newdata = iris, probability = TRUE, decision.values = TRUE)
	pred2 <- predict(fit2, newdata = iris, probability = TRUE, decision.values = TRUE)
	pred3 <- predict(fit3, newdata = iris, probability = TRUE, decision.values = TRUE)
	# pred1 <- predict(fit1, newdata = iris[1,], probability = TRUE, decision.values = TRUE)
	# pred2 <- predict(fit2, newdata = iris[1,], probability = TRUE, decision.values = TRUE)
	# pred3 <- predict(fit3, newdata = iris[1,], probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)
	expect_equal(pred1, pred3)
	expect_equal(pred2, pred3)
})


test_that("ossvm: labels vector set correctly",{

	## all classes, correct order
	data(iris)
	iris[,1:4] <- scale(iris[,1:4])
	k <- 100
	n <- 90
	x <- as.matrix(iris[,-5])
	dist <- sqrt(colSums((t(x) - x[n,])^2))
	w <- rectangular(k = k)(dist)
	
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = w/sum(w) * 150, probability = TRUE, scale = FALSE)
	set.seed(120)
	pred1 <- predict(fit1, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	fit2 <- ossvm(Species ~ ., data = iris, probability = TRUE, wf = "rectangular", k = k, scale = FALSE)
	set.seed(120)
	pred2 <- predict(fit2, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	expect_equal(pred1, pred2)				# not exactly equal

	## all classes, but different order 1
	data(iris)
	iris[,1:4] <- scale(iris[,1:4])
	perm <- 150:1  ## 3, 2, 1
	iris <- iris[perm,]
	k = 100
	n <- 90
	x <- as.matrix(iris[,-5])
	dist <- sqrt(colSums((t(x) - x[n,])^2))
	w <- rectangular(k = k)(dist)
	
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = w/sum(w) * 150, probability = TRUE, scale = FALSE)
	set.seed(120)
	pred1 <- predict(fit1, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	fit2 <- ossvm(Species ~ ., data = iris, probability = TRUE, wf = "rectangular", k = k, scale = FALSE)
	set.seed(120)
	pred2 <- predict(fit2, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	expect_equivalent(pred1, pred2)	
	expect_equal(attr(pred1, "probabilities"), attr(pred2, "probabilities")[,3:1, drop = FALSE])				# not exactly equal	
	expect_equal(as.vector(attr(pred1, "decision.values")), -as.vector(attr(pred2, "decision.values")[,3:1]))	
		
	## all classes, but different order 2
	data(iris)
	iris[,1:4] <- scale(iris[,1:4])
	perm <- c(150:101,1:100)  ## 3, 1, 2
	iris <- iris[perm,]
	k = 100
	n <- 90
	x <- as.matrix(iris[,-5])
	dist <- sqrt(colSums((t(x) - x[n,])^2))
	w <- rectangular(k = k)(dist)
	
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = w/sum(w) * 150, probability = TRUE, scale = FALSE)
	set.seed(120)
	pred1 <- predict(fit1, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	fit2 <- ossvm(Species ~ ., data = iris, probability = TRUE, wf = "rectangular", k = k, scale = FALSE)
	set.seed(120)
	pred2 <- predict(fit2, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	expect_equivalent(pred1, pred2)	
	expect_equal(attr(pred1, "probabilities"), attr(pred2, "probabilities")[,c(3,1,2), drop = FALSE])	
	expect_equivalent(c(1,-1,-1)*attr(pred1, "decision.values")[c(3,1,2)], as.vector(attr(pred2, "decision.values")))
		
	## 2 classes, correct order
	data(iris)
	iris[,1:4] <- scale(iris[,1:4])
	k <- 50
	n <- 90
	x <- as.matrix(iris[,-5])
	dist <- sqrt(colSums((t(x) - x[n,])^2))
	w <- rectangular(k = k)(dist)
	
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = w/sum(w) * 150, probability = TRUE, scale = FALSE)
	set.seed(120)
	pred1 <- predict(fit1, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	fit2 <- ossvm(Species ~ ., data = iris, probability = TRUE, wf = "rectangular", k = k, scale = FALSE)
	set.seed(120)
	pred2 <- predict(fit2, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	expect_equivalent(pred1, pred2)	
	expect_equal(attr(pred1, "probabilities"), attr(pred2, "probabilities")[,colnames(attr(pred1, "probabilities")), drop = FALSE])	
	expect_equal(as.numeric(attr(pred1, "decision.values")), attr(pred2, "decision.values")[,!is.na(attr(pred2, "decision.values"))])	
		
	## 2 classes, but different order 1
	data(iris)
	iris[,1:4] <- scale(iris[,1:4])
	perm <- 150:1
	iris <- iris[perm,]
	k <- 40
	n <- 99
	x <- as.matrix(iris[,-5])
	dist <- sqrt(colSums((t(x) - x[n,])^2))
	w <- rectangular(k = k)(dist)
	
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = w/sum(w) * 150, probability = TRUE, scale = FALSE)
	set.seed(120)
	pred1 <- predict(fit1, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	fit2 <- ossvm(Species ~ ., data = iris, probability = TRUE, wf = "rectangular", k = k, scale = FALSE)
	set.seed(120)
	pred2 <- predict(fit2, newdata = iris[n,], probability = TRUE, decision.values = TRUE)
	expect_equivalent(pred1, pred2)
	expect_equal(attr(pred1, "probabilities"), attr(pred2, "probabilities")[,colnames(attr(pred1, "probabilities")), drop = FALSE])	
	expect_equal(as.numeric(attr(pred1, "decision.values")), -attr(pred2, "decision.values")[!is.na(attr(pred2, "decision.values"))])

	## 2 classes, but different order 2
	data(iris)
	iris[,1:4] <- scale(iris[,1:4])
	perm <- c(150:101,1:100)
	iris <- iris[perm,]
	k <- 50
	n <- 90
	x <- as.matrix(iris[,-5])
	dist <- sqrt(colSums((t(x) - x[n,])^2))
	w <- rectangular(k = k)(dist)
	
	fit1 <- wsvm(Species ~ ., data = iris, case.weights = w/sum(w) * 150, probability = TRUE, scale = FALSE)
	set.seed(120)
	pred1 <- predict(fit1, newdata = iris[n,], probability = TRUE, decision.values = TRUE)	
	fit2 <- ossvm(Species ~ ., data = iris, probability = TRUE, wf = "rectangular", k = k, scale = FALSE)
	set.seed(120)
	pred2 <- predict(fit2, newdata = iris[n,], probability = TRUE, decision.values = TRUE)	
	expect_equivalent(pred1, pred2)
	expect_equal(attr(pred1, "probabilities"), attr(pred2, "probabilities")[,colnames(attr(pred1, "probabilities")), drop = FALSE])	
	expect_equal(as.numeric(attr(pred1, "decision.values")), attr(pred2, "decision.values")[!is.na(attr(pred2, "decision.values"))])
})


test_that("ossvm: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- ossvm(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- ossvm(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-1], fit2[-1])

	expect_that(fit1 <- ossvm(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5), gives_warning("argument 'bw' is ignored"))	
	fit2 <- ossvm(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-1], fit2[-1])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- ossvm(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- ossvm(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-1], fit2[-1])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- ossvm(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5), gives_warning("argument 'bw' is ignored"))
	fit2 <- ossvm(Species ~ ., data = iris, wf = function(x) exp(-x))
	expect_equal(fit1[-1], fit2[-1])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	fit <- ossvm(formula = Species ~ ., data = iris, wf = gaussian) ## error because length(weights) and nrow(x) are different
	expect_error(predict(fit))
	
	# bw, k missing
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = gaussian()), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = gaussian(), k = 10), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(ossvm(Species ~ ., data = iris), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5), throws_error("'bw' must be positive"))
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(ossvm(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris))), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50), throws_error("'k' must be positive"))

	# k too small
	#fit <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005)
	#expect_equal(length(is.na(predict(fit)$class)), 150)

	# k too large
	expect_that(ossvm(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(ossvm(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris))), gives_warning("only first element of 'k' used"))
})


test_that("ossvm: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = rectangular(bw = 5))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "rectangular", k = 50)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = rectangular(k = 50))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	
	# nn.only not needed
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", k = 50)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = gaussian(k = 50))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)

	# adaptive bw, all obs
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)

	# fixed bw, only knn
	fit1 <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50)
	fit2 <- ossvm(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50))
	expect_equal(fit1[-c(1,4)], fit2[-c(1,4)])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})	


#=================================================================================================================
context("predict.ossvm")

test_that("predict.ossvm works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, probability = TRUE)
  	pred <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])   #####
  	expect_equal(names(pred), rownames(iris)[ran])  	
	## formula, data, newdata
  	pred <- predict(fit, newdata = iris[-ran,], probability = TRUE, decision.values = TRUE)
  	expect_equal(names(pred), rownames(iris)[-ran])  	
  	expect_equal(rownames(attr(pred, "probabilities")), rownames(iris)[-ran])  	
  	expect_equal(rownames(attr(pred, "decision.values")), rownames(iris)[-ran])  	
	## y, x
	fit <- ossvm(x = iris[ran,-5], y = iris$Species[ran], wf = "gaussian", bw = 2, probability = TRUE)  
  	pred <- predict(fit)
  	expect_equal(names(pred), rownames(iris)[ran])  	
	## y, x, newdata
  	pred <- predict(fit, newdata = iris[-ran,-5], probability = TRUE, decision.values = TRUE)
  	expect_equal(names(pred), rownames(iris)[-ran])  	
  	expect_equal(rownames(attr(pred, "probabilities")), rownames(iris)[-ran])  	
  	expect_equal(rownames(attr(pred, "decision.values")), rownames(iris)[-ran])  	
})


test_that("predict.ossvm: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- ossvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[,-5])
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- ossvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
	# y, x
	fit <- ossvm(x = iris[ran,-5], y = iris$Species[ran], wf = "gaussian", bw = 2)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred1, pred2)
})


test_that("predict.ossvm works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100, probability = TRUE), gives_warning("group virginica is empty"))
	expect_equal(length(fit$y), 100)
	expect_equal(nrow(fit$x), 100)
	expect_equal(fit$nclass, 2)
	pred <- predict(fit, newdata = iris[-ran,], probability = TRUE, decision.values = TRUE)
	expect_equal(nlevels(pred), 3)
	expect_equal(ncol(attr(pred, "probabilities")), 2)
	expect_equal(ncol(attr(pred, "decision.values")), 1)
})


test_that("predict.ossvm works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- ossvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, probability = TRUE)
	expect_equal(ncol(fit$x), 1)
	predict(fit, newdata = iris[-ran,], probability = TRUE, decision.values = TRUE)
})


test_that("predict.ossvm works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran, probability = TRUE)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred, a)
  	pred <- predict(fit, newdata = iris[5,], probability = TRUE, decision.values = TRUE)
	expect_equal(length(pred), 1)
	expect_equal(dim(attr(pred, "probabilities")), c(1, 3))
	expect_equal(dim(attr(pred, "decision.values")), c(1, 3))
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(length(pred), 1)
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred, a)
	pred <- predict(fit, newdata = iris[58,], probability = TRUE, decision.values = TRUE)
	expect_equal(length(pred), 1)
	expect_equal(dim(attr(pred, "probabilities")), c(1, 3))
	expect_equal(dim(attr(pred, "decision.values")), c(1, 3))
})	


test_that("predict.ossvm works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- ossvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran, probability = TRUE)
	expect_equal(ncol(fit$x), 1)
	pred <- predict(fit, newdata = iris[5,], probability = TRUE, decision.values = TRUE)
	expect_equal(length(pred), 1)
	expect_equal(dim(attr(pred, "probabilities")), c(1, 3))
	expect_equal(dim(attr(pred, "decision.values")), c(1, 3))
})

   
test_that("predict.ossvm: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran, probability = TRUE)
	## na.omit
	pred <- predict(fit, newdata = irisna, na.action = na.omit, probability = TRUE, decision.values = TRUE)
	expect_equal(length(pred), 133)
	expect_equal(names(pred), as.character(18:150))
	expect_equal(nrow(attr(pred, "probabilities")), 133)
	expect_equal(rownames(attr(pred, "probabilities")), as.character(18:150))
	expect_equal(nrow(attr(pred, "decision.values")), 133)
	expect_equal(rownames(attr(pred, "decision.values")), as.character(18:150))
	## na.fail
	expect_that(predict(fit, newdata = irisna, na.action = na.fail, probability = TRUE, decision.values = TRUE), throws_error("missing values in object"))
})


test_that("predict.ossvm: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- ossvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})  	


