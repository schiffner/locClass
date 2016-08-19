context("FLXMCLqda")

test_that("FLXMCLqda: misspecified arguments", {
	# wrong variable names
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ V1, data = iris, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'V1' nicht gefunden"))
	expect_that(fit <- flexmix(y ~ Sepal.Length, data = iris, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'y' nicht gefunden"))
	# wrong class
	expect_error(fit <- flexmix(iris, data = iris, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = constantModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("FLXMCLqda: arguments are passed to wqda",{
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 2)$cluster

	# default: "ML" hard-coded
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$method, "ML")
	
	# unbiased
	expect_error(fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLqda(method = "unbiased"), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1)))
})


test_that("FLXMCLqda with several options works",{
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 2)$cluster

	## weighted
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# ok
	
	## hard
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLqda(), cluster = 2, control = list(iter.max = 200, classify = "hard", verb = 1))	
	# ok
})


test_that("FLXMCLqda with concomitant model works",{
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 2)$cluster
	
	## weighted, FLXPwlda
	# fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# # not monotone
	
	# ## hard, FLXPwlda
	# fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# # not monotone

	## weighted, FLXPmultinom
	fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# not monotone
		
	## hard, FLXPmultinom
	fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# ok
})


test_that("FLXMCLqda throws a warning if grouping variable is numeric", {
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(tr2 <- flexmix(Petal.Width ~ Petal.Length, data = iris, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), gives_warning("'grouping' was coerced to a factor"))
})


test_that("FLXMCLqda works if only one predictor variable is given", {
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLqda(), cluster = 2, control = list(iter.max = 200, classify = "hard"))
	expect_equal(ncol(fit@components$Comp.1[[1]]@parameters$means), 1)
	expect_equal(dim(fit@components$Comp.1[[1]]@parameters$cov[[1]]), rep(1,2))
})


test_that("FLXMCLqda: Local and global solution coincide if only one cluster is given", {
	fit <- flexmix(Species ~ ., data = iris, model = FLXMCLqda(), cluster = 1, control = list(iter.max = 200, classify = "hard"))
	w <- wqda(Species ~ ., data = iris, method = "ML")
	expect_equal(fit@components[[1]][[1]]@parameters$prior, w$prior)
	expect_equal(fit@components[[1]][[1]]@parameters$means, w$means)
	expect_equal(fit@components[[1]][[1]]@parameters$cov, w$cov)
	pred <- mypredict(fit)
	p <- predict(w)
	expect_equal(pred[[1]]/rowSums(pred[[1]]), p$posterior)
})


test_that("FLXMCLqda: training data from only one class", {
	cluster <- kmeans(iris[1:50,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width + Sepal.Length, data = iris[1:50,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("training data from only one group given"))
})


test_that("FLXMCLqda: missing classes in individual clusters", {
	library(mlbench)
	data(Glass)
	set.seed(120)
	cluster <- kmeans(Glass[,1], centers = 2)$cluster
	fit <- flexmix(Type ~ Al, data = Glass, model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	expect_equal(rownames(fit@components$Comp.1[[1]]@parameters$means), as.character(c(1:3,5:7)))
	expect_equal(names(fit@components$Comp.1[[1]]@parameters$prior), as.character(c(1:3,5:7)))
	expect_equal(rownames(fit@components$Comp.2[[1]]@parameters$means), as.character(c(1:3,5,7)))
	expect_equal(names(fit@components$Comp.2[[1]]@parameters$prior), as.character(c(1:3,5,7)))
	pred <- mypredict(fit, aggregate = FALSE)
	expect_equal(colnames(pred$Comp.1), as.character(c(1:3,5:7)))
	expect_equal(colnames(pred$Comp.2), as.character(c(1:3,5:7)))
	expect_true(all(pred$Comp.2[,5] == 0))
})


test_that("FLXMCLqda: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
	expect_equal(length(tr2@components), 9)
	expect_equal(ncol(tr2@posterior$scaled), 9)
})


#=================================================================================================================
context("predict FLXMCLqda")

test_that("predict FLXMCLqda works correctly with missing newdata", {
	set.seed(120)	
	ran <- sample(1:500,300)
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x[ran,], centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data)[ran,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1[[1]]), rownames(as.data.frame(data)[ran,]))
})


test_that("predict FLXMCLqda works with missing classes in the training data", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[1:100,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ ., data = iris[1:100,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, aggregate = FALSE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	pred <- mypredict(tr2, aggregate = TRUE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
})


test_that("predict FLXMCLqda works with one single predictor variable", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, newdata = iris[-ran,], aggregate = FALSE)
	pred <- mypredict(tr2, aggregate = TRUE)
})


test_that("predict FLXMCLqda works with one single test observation", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
  	pred <- mypredict(tr2, newdata = iris[5,])
	expect_equal(dim(pred[[1]]), c(1,3))
  	pred <- mypredict(tr2, newdata = iris[5,], aggregate = TRUE)
	expect_equal(dim(pred[[1]]), c(1,3))
})	


test_that("predict FLXMCLqda: NA handling in newdata works", {
	set.seed(129)
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Length + Sepal.Width, data = iris[ran,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))

	## NAs in explanatory variables are ok
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	pred <- mypredict(tr2, newdata = irisna)
	expect_equal(all(is.na(pred[[1]][1:17,])), TRUE)
	expect_equal(all(is.na(pred[[2]][1:17,])), TRUE)
	pred <- mypredict(tr2, newdata = irisna, aggregate = TRUE)
	expect_equal(all(is.na(pred[[1]][1:17,])), TRUE)
	
	## NAs in splitting variable are not ok if aggregation is desired
	irisna[1:17,1:3] <- NA
	pred <- mypredict(tr2, newdata = irisna)
	expect_equal(all(is.na(pred[[1]][1:17,])), TRUE)
	expect_equal(all(is.na(pred[[2]][1:17,])), TRUE)
	pred <- mypredict(tr2, newdata = irisna, aggregate = TRUE)
	expect_equal(all(is.na(pred[[1]][1:17,])), TRUE)
})


test_that("predict FLXMCLqda: misspecified arguments", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
    # errors in newdata
    expect_error(mypredict(tr2, newdata = TRUE))
    expect_error(mypredict(tr2, newdata = -50:50))
})


#=================================================================================================================
# library(locClassData)
# d <- flashData(500)
# #d <- vNormalData(500)
# #d <- vNormalQuadraticData(500)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))

# cluster <- kmeans(d$x, center = 2)$cluster
# model <- FLXMCLlda()
# #res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster)
# res


# # model <- FLXMCLlda()
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, k = 2, control = list(classify = "hard"))
# # res
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = res@cluster)
# # res


# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,1])
# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,2])

# plot(d$x, col = d$y, cex = res@posterior$scaled[,1])
# plot(d$x, col = d$y, cex = res@posterior$scaled[,2])
# points(res@components[[1]][[1]]@parameters$means, col = "blue", pch = 19, cex = 2)
# points(res@components[[2]][[1]]@parameters$means, col = "green", pch = 19, cex = 2)

# pred <- predict(res, newdata = as.data.frame(d), local.aggregate = TRUE)

# pred.grid <- predict(res, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)

# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
# points(d$x, col = d$y)

# ## plot predicted local membership
# pred.loc.grid <- predict(res@concomitant, newdata = grid)
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.loc.grid[,1], length(seq(-6,6,0.2))), add = TRUE)
