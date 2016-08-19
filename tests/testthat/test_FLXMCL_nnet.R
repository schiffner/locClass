context("FLXMCLnnet")

test_that("FLXMCLnnet: misspecified arguments", {
	# wrong variable names
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ V1, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("object 'V1' not found"))
	expect_that(fit <- flexmix(Species ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ V1), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("object 'V1' not found"))
	expect_that(fit <- flexmix(y ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("object 'y' not found"))
	# wrong class
	expect_error(fit <- flexmix(iris, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = majorityModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("FLXMCLnnet: arguments are passed to nnet",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster

	## size, trace
	Wts <- runif(20, -0.7, 0.7)
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLnnet(size = 1, trace = FALSE, Wts = Wts, decay = 0.1), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$n[2], 1)
	
	## decay ###
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLnnet(size = 1, trace = FALSE, decay = 0.1, reps = 5), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$decay, 0.1)
	expect_equal(fit@components[[1]][[1]]@parameters$reps, 5)

	## censored
	expect_true(!fit@components[[1]][[1]]@parameters$censored)
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLnnet(size = 1, trace = FALSE, decay = 0.1, censored = TRUE, reps = 5), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_true(fit@components[[1]][[1]]@parameters$censored)
	expect_equal(fit@components[[1]][[1]]@parameters$reps, 5)

})


test_that("FLXMCLnnet without concomitant variable model",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster
	Wts <- runif(20, -0.7, 0.7)

	## weighted
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLnnet(size = 1, trace = FALSE, Wts = Wts, decay = 0.1), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# not monotone
	# not monotone with given Wts
	# montone with Wts and decay
	# not monotone with decay
	
	## hard
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLnnet(size = 1, trace = FALSE, decay = 0.1), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))	
	# not monotone
	# monotone with Wts
	# montone with Wts and decay
	# montone with decay
})


test_that("FLXMCLnnet with several options works",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(120)
	cluster <- kmeans(iris[,1], centers = 2)$cluster
	Wts <- runif(20, -0.7, 0.7)

	## weighted, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# # no convergence
	# # not monotone at all
	
# #####	## hard, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# # no convergence
	# # not monotone at all
	
	## weighted, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, Wts = Wts), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1, tolerance = 10^-4))
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, reps = 5), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1, tolerance = 10^-4))
	# not monotone
	# monotone with Wts
				
	## hard, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, Wts = Wts), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1, tolerance = 10^-4))
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, reps = 5), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1, tolerance = 10^-4))
	# ok
})


test_that("FLXMCLnnet throws a warning if grouping variable is numeric", {
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	Wts <- runif(20, -0.7, 0.7)
	expect_that(tr2 <- flexmix(Petal.Width ~ Petal.Length + Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Petal.Length + Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, Wts = Wts), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1)), gives_warning("currently only classification networks are supported and 'y' was coerced to a factor"))
})


test_that("FLXMCLnnet ist set up correctly", {
	library(mlbench)
	data(Sonar)
	## 2 classes
	cluster <- kmeans(Sonar[,1:4], centers = 2)$cluster
	fit <- flexmix(Class ~ V1, data = Sonar, concomitant = FLXPmultinom(~ V2), model = FLXMCLnnet(size = 2, trace = FALSE, decay = 0.1), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1, tolerance = 10^-4))
	library(nnet)	
	m <- nnet(Class ~ V1, data = Sonar, size = 2, trace = FALSE)
	# y <- fit@model[[1]]@y
	# expect_equal(ncol(y),2)
	# expect_true(!attr(y, "is.matrix"))
	expect_equal(fit@components$Comp.1[[1]]@parameters$softmax, m$softmax)
	expect_equal(fit@components$Comp.1[[1]]@parameters$entropy, m$entropy)
	# x <- fit@model[[1]]@x
	# expect_true(all(m$wts[!attr(x, "mask")] == 0))
	
	## > 2 classes
	Wts <- runif(20, -0.7, 0.7)
	cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, Wts = Wts), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1, tolerance = 10^-4))
	m <- nnet(Species ~ Sepal.Width, data = iris, size = 1, trace = FALSE)
	# y <- fit@model[[1]]@y
	# expect_equal(ncol(y),3)
	# expect_true(attr(y, "is.matrix"))
	expect_equal(fit@components$Comp.1[[1]]@parameters$softmax, m$softmax)
	expect_equal(fit@components$Comp.1[[1]]@parameters$entropy, m$entropy)
	# x <- fit@model[[1]]@x
	# expect_true(all(m$wts[!attr(x, "mask")] == 0))


### VERHALTEN nnet BEI NUMERIC überlgen
	# ## > 2 classes, numeric target variable
	# iris$Species <- as.numeric(iris$Species)
	# cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	# expect_that(fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), gives_warning("currently only classification is supported, 'y' was coerced to a factor"))
	# m <- nnet(Species ~ Sepal.Width, data = iris, size = 1)
	# y <- fit@model[[1]]@y
	# expect_equal(ncol(y),3)
	# expect_equal(attr(y, "softmax"), m$softmax)
	# expect_equal(attr(y, "entropy"), m$entropy)
	# # expect_true(attr(y, "is.matrix"))
	# # x <- fit@model[[1]]@x
	# # expect_true(all(m$wts[!attr(x, "mask")] == 0))		
})


test_that("FLXMCLnnet: Local and global solution coincide if only one cluster is given", {
	set.seed(123)
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLnnet(size = 1, trace = FALSE, Wts = runif(8)), cluster = 1, control = list(iter.max = 200, classify = "hard"))
	set.seed(123)
	w <- nnet(Species ~ Sepal.Width, data = iris, size = 1, Wts = runif(8))
	expect_equal(fit@components[[1]][[1]]@parameters$wts, w$wts)
	expect_equal(fit@components[[1]][[1]]@parameters$softmax, w$softmax)
	expect_equal(fit@components[[1]][[1]]@parameters$entropy, w$entropy)
	pred <- mypredict(fit)
	p <- predict(w)
	expect_equal(pred[[1]], p)
})


test_that("FLXMCLnnet works if only one predictor variable is given", {
	cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE, reps = 5), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
})


test_that("FLXMCLnnet: training data from only one class", {
	cluster <- kmeans(iris[1:50,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width + Sepal.Length, data = iris[1:50,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("FLXMCLnnet: missing classes in individual clusters", {
	library(mlbench)
	data(Glass)
	Glass[,1:9] <- scale(Glass[,1:9])
	set.seed(120)
	cluster <- kmeans(Glass[-c(177:182),1:9], centers = 2)$cluster
	fit <- flexmix(Type ~ Na, data = Glass[-c(177:182),], concomitant = FLXPmultinom(as.formula(paste("~", paste(colnames(Glass)[1:9], collapse = "+")))), model = FLXMCLnnet(size = 1, reps = 5, decay = 0.01, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	pred <- mypredict(fit, aggregate = FALSE)
	expect_equal(colnames(pred$Comp.1), as.character(c(1:3,5:7)))
	expect_equal(colnames(pred$Comp.2), as.character(c(1:3,5:7)))
	expect_true(all(pred$Comp.1[,c(1,3)] < 0.01))
})


test_that("FLXMCLnnet: removing clusters works", {
	set.seed(120)	
	data <- benchData::flashData(500)
	cluster <- kmeans(data$x, centers = 10)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLnnet(size = 1, trace = FALSE, reps = 5, decay = 0.05), cluster = cluster, control = list(classify = "weighted", iter.max = 200, verb = 1))
## Convergence!!!
	expect_equal(length(tr2@components), 5)
	expect_equal(ncol(tr2@posterior$scaled), 5)
})


#=================================================================================================================
context("FLXMCLnnet")

test_that("predict FLXMCLnnet works correctly with missing newdata", {
	set.seed(120)	
	ran <- sample(1:500,300)
	data <- benchData::flashData(500)
	cluster <- kmeans(data$x[ran,], centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data)[ran,], concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLnnet(size = 1, trace = FALSE, decay = 0.1, reps = 5), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1[[1]]), rownames(as.data.frame(data)[ran,]))
})


test_that("predict FLXMCLnnet works with missing classes in the training data", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[1:100,1], centers = 2)$cluster
	expect_that(tr2 <- flexmix(Species ~ Sepal.Width, data = iris[1:100,], concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLnnet(size = 1, decay = 0.1, reps = 5, trace = FALSE), cluster = cluster, control = list(iter.max = 200)), gives_warning("group virginica is empty"))
	pred <- mypredict(tr2, aggregate = FALSE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	expect_true(all(pred[[1]][,"virginica"] == 0))
	pred <- mypredict(tr2, aggregate = TRUE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	expect_true(all(pred[[1]][,"virginica"] == 0))
})


test_that("predict FLXMCLnnet works with one single predictor variable", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLnnet(size = 1, decay = 0.1, reps = 5, trace = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, newdata = iris[-ran,], aggregate = FALSE)
	pred <- mypredict(tr2, aggregate = TRUE)
})


test_that("predict FLXMCLnnet works with one single test observation", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLnnet(size = 1, decay = 0.1, reps = 5, trace = FALSE), cluster = cluster, control = list(iter.max = 200))
  	pred <- mypredict(tr2, newdata = iris[5,])
	expect_equal(dim(pred[[1]]), c(1,3))
  	pred <- mypredict(tr2, newdata = iris[5,], aggregate = TRUE)
	expect_equal(dim(pred[[1]]), c(1,3))
})	


test_that("predict FLXMCLnnet: NA handling in newdata works", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Length + Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLnnet(size = 1, reps = 5, decay = 0.1, trace = FALSE), cluster = cluster, control = list(iter.max = 200))

	## NAs in explanatory variables are ok
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	expect_error(pred <- mypredict(tr2, newdata = irisna))
	expect_error(pred <- mypredict(tr2, newdata = irisna, aggregate = TRUE))
	
	## NAs in splitting variable are not ok if aggregation is desired
	irisna[1:17,1:3] <- NA
	expect_error(pred <- mypredict(tr2, newdata = irisna))
	expect_error(pred <- mypredict(tr2, newdata = irisna, aggregate = TRUE))
})


test_that("predict FLXMCLnnet: misspecified arguments", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width + Petal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLnnet(size = 1, reps = 5, decay = 0.1, trace = FALSE), cluster = cluster, control = list(iter.max = 200))
    # errors in newdata
    expect_error(mypredict(tr2, newdata = TRUE))
    expect_error(mypredict(tr2, newdata = -50:50))
})



#=================================================================================================================

# library(benchData)
# #d <- vNormalData(500)
# d <- flashData(500)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))


# cluster <- kmeans(d$x, center = 2)$cluster
# model <- FLXMCLnnet(size = 1, trace = FALSE)
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res

# # model <- FLXMCLnnet(size = 1, trace = FALSE)
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, k = 2)
# # res

# # model <- FLXMCLnnet(size = 1, trace = FALSE)
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, k = 2, control = list(classify = "hard"))
# # res
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, cluster = res@cluster)
# # res

# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,1])
# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,2])

# plot(d$x, col = d$y, cex = res@posterior$scaled[,1])
# plot(d$x, col = d$y, cex = res@posterior$scaled[,2])

# pred <- predict(res, newdata = as.data.frame(d), local.aggregate = TRUE)

# pred.grid <- predict(res, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)

# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)

# n <- nnet(y ~ ., data = d, size = 2)
# pred.grid <- predict(n, grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)
