context("FLXMCLlda")

test_that("FLXMCLlda: misspecified arguments", {
	# wrong variable names
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ V1, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'V1' nicht gefunden"))
	expect_that(fit <- flexmix(y ~ Sepal.Length, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'y' nicht gefunden"))
	# wrong class
	expect_error(fit <- flexmix(iris, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = constantModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("FLXMCLlda: arguments are passed to wlda",{
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster

	# default: "ML" hard-coded
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$method, "ML")
	
	# unbiased
	expect_error(fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLlda(method = "unbiased"), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1)))
})


test_that("FLXMCLlda with several options works",{
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster

	## weighted
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# ok
	
	## hard
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))	
	# ok
})


test_that("FLXMCLlda with concomitant model works",{
	cluster <- kmeans(iris[,1:4], centers = 2)$cluster

	# ## weighted, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# # likelihood not monotone
	
	# ## hard, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# # ok

	## weighted, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# no convergence: jumps between two likelihod values
		
	## hard, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# ok
})


test_that("FLXMCLlda throws a warning if grouping variable is numeric", {
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(tr2 <- flexmix(Petal.Width ~ Petal.Length + Sepal.Length, data = iris, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), gives_warning("'grouping' was coerced to a factor"))
})


test_that("FLXMCLlda works if only one predictor variable is given", {
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLlda(), cluster = 2, control = list(iter.max = 200, classify = "hard"))
	expect_equal(ncol(fit@components$Comp.1[[1]]@parameters$means), 1)
	expect_equal(dim(fit@components$Comp.1[[1]]@parameters$cov), rep(1,2))
})


test_that("FLXMCLlda: Local and global solution coincide if only one cluster is given", {
	fit <- flexmix(Species ~ ., data = iris, model = FLXMCLlda(), cluster = 1, control = list(iter.max = 200, classify = "hard"))
	w <- wlda(Species ~ ., data = iris, method = "ML")
	expect_equal(fit@components[[1]][[1]]@parameters$prior, w$prior)
	expect_equal(fit@components[[1]][[1]]@parameters$means, w$means)
	expect_equal(fit@components[[1]][[1]]@parameters$cov, w$cov)
	pred <- mypredict(fit)
	p <- predict(w)
	expect_equal(pred[[1]]/rowSums(pred[[1]]), p$posterior)
})


test_that("FLXMCLlda: training data from only one class", {
	cluster <- kmeans(iris[1:50,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width + Sepal.Length, data = iris[1:50,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("training data from only one group given"))
})


test_that("FLXMCLlda: missing classes in individual clusters", {
	library(mlbench)
	data(Glass)
	set.seed(120)
	cluster <- kmeans(Glass[,1:9], centers = 2)$cluster
	fit <- flexmix(Type ~ ., data = Glass, model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	expect_equal(rownames(fit@components$Comp.1[[1]]@parameters$means), as.character(c(1:3,5:7)))
	expect_equal(names(fit@components$Comp.1[[1]]@parameters$prior), as.character(c(1:3,5:7)))
	expect_equal(rownames(fit@components$Comp.2[[1]]@parameters$means), as.character(c(2,5:7)))
	expect_equal(names(fit@components$Comp.2[[1]]@parameters$prior), as.character(c(2,5:7)))
	pred <- mypredict(fit, aggregate = FALSE)
	expect_equal(colnames(pred$Comp.1), as.character(c(1:3,5:7)))
	expect_equal(colnames(pred$Comp.2), as.character(c(1:3,5:7)))
	expect_true(all(pred$Comp.2[,1] == 0))
	expect_true(all(pred$Comp.2[,3] == 0))
})


test_that("FLXMCLlda: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))
	expect_equal(length(tr2@components), 8)
	expect_equal(ncol(tr2@posterior$scaled), 8)
})


#=================================================================================================================
context("predict FLXMCLlda")

test_that("predict FLXMCLlda works correctly with missing newdata", {
	set.seed(120)	
	ran <- sample(1:500,300)
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x[ran,], centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data)[ran,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1[[1]]), rownames(as.data.frame(data)[ran,]))
})


test_that("predict FLXMCLlda works with missing classes in the training data", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[1:100,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ ., data = iris[1:100,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, aggregate = FALSE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	pred <- mypredict(tr2, aggregate = TRUE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
})


test_that("predict FLXMCLlda works with one single predictor variable", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, newdata = iris[-ran,], aggregate = FALSE)
	pred <- mypredict(tr2, aggregate = TRUE)
})


test_that("predict FLXMCLlda works with one single test observation", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))
  	pred <- mypredict(tr2, newdata = iris[5,])
	expect_equal(dim(pred[[1]]), c(1,3))
  	pred <- mypredict(tr2, newdata = iris[5,], aggregate = TRUE)
	expect_equal(dim(pred[[1]]), c(1,3))
})	


test_that("predict FLXMCLlda: NA handling in newdata works", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Length + Sepal.Width, data = iris[ran,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))

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


test_that("predict FLXMCLlda: misspecified arguments", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width + Petal.Width, data = iris[ran,], model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200))
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
