context("FLXMCLmultinom")

test_that("FLXMCLmultinom: misspecified arguments", {
	# wrong variable names
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ V1, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("object 'V1' not found"))
	expect_that(fit <- flexmix(Species ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ V1), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("object 'V1' not found"))
	expect_that(fit <- flexmix(y ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("object 'y' not found"))
	# wrong class
	expect_error(fit <- flexmix(iris, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = majorityModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("FLXMCLmultinom: arguments are passed to multinom/nnet",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster

	## trace
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLmultinom(trace = TRUE), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	
	## decay
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLmultinom(trace = FALSE, decay = 0.1), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$decay, 0.1)

	## censored
	expect_true(!fit@components[[1]][[1]]@parameters$censored)
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLmultinom(trace = FALSE, decay = 0.1, censored = TRUE), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_true(fit@components[[1]][[1]]@parameters$censored)

})


test_that("FLXMCLmultinom without concomitant variable model",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster

	## weighted
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLmultinom(decay = 0.1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# not monotone with decay = 0
	
	## hard
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))	
	# ok
})


test_that("FLXMCLmultinom with several options works",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(123)
	cluster <- kmeans(iris[,1], centers = 2)$cluster

	# ## weighted, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# # ok
	
	# ## hard, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# # ok
	
	## weighted, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# not monotone

	## hard, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# ok
})


test_that("FLXMCLmultinom throws a warning if grouping variable is numeric", {
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(tr2 <- flexmix(Petal.Width ~ Petal.Length + Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Petal.Length + Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), gives_warning("'Y' was coerced to a factor"))
})


test_that("FLXMCLmultinom ist set up correctly", {
	## 2 classes
	cluster <- kmeans(iris[1:100,1], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris[1:100,], concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	library(nnet)	
	m <- multinom(Species ~ Sepal.Width, data = iris[1:100,], trace = FALSE)
	# y <- fit@model[[1]]@y
	# expect_equal(ncol(y),1)
	# expect_true(!attr(y, "is.matrix"))
	# x <- fit@model[[1]]@x
	expect_equal(fit@components$Comp.1[[1]]@parameters$softmax, m$softmax)
	expect_equal(fit@components$Comp.1[[1]]@parameters$entropy, m$entropy)
	expect_true(all(m$wts[!fit@components$Comp.1[[1]]@parameters$mask] == 0))
	
	## > 2 classes
	set.seed(120)
	cluster <- kmeans(iris[,1], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	m <- multinom(Species ~ Sepal.Width, data = iris, trace = FALSE)
	# y <- fit@model[[1]]@y
	# expect_equal(ncol(y),3)
	# expect_true(attr(y, "is.matrix"))
	# x <- fit@model[[1]]@x
	expect_equal(fit@components$Comp.2[[1]]@parameters$softmax, m$softmax)
	expect_equal(fit@components$Comp.2[[1]]@parameters$entropy, m$entropy)
	expect_true(all(m$wts[!fit@components$Comp.2[[1]]@parameters$mask] == 0))
	expect_true(fit@components$Comp.1[[1]]@parameters$softmax)
	expect_true(!fit@components$Comp.1[[1]]@parameters$entropy)
	
	## > 2 classes, numeric target variable
	iris$Species <- as.numeric(iris$Species)
	set.seed(120)
	cluster <- kmeans(iris[,1], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	m <- multinom(Species ~ Sepal.Width, data = iris, trace = FALSE)
	# y <- fit@model[[1]]@y
	# expect_equal(ncol(y),3)
	# expect_true(attr(y, "is.matrix"))
	# x <- fit@model[[1]]@x
	expect_equal(fit@components$Comp.2[[1]]@parameters$softmax, m$softmax)
	expect_equal(fit@components$Comp.2[[1]]@parameters$entropy, m$entropy)
	expect_true(all(m$wts[!fit@components$Comp.1[[1]]@parameters$mask] == 0))
	expect_true(fit@components$Comp.1[[1]]@parameters$softmax)
	expect_true(!fit@components$Comp.1[[1]]@parameters$entropy)
})


test_that("FLXMCLmultinom: Local and global solution coincide if only one cluster is given", {
	fit <- flexmix(Species ~ ., data = iris, model = FLXMCLmultinom(trace = FALSE), cluster = 1, control = list(iter.max = 200, classify = "hard"))
	w <- multinom(Species ~ ., data = iris, trace = FALSE)
	expect_equal(fit@components[[1]][[1]]@parameters$wts, w$wts)
	pred <- mypredict(fit)
	p <- predict(w, type = "probs")
	expect_equal(pred[[1]], p)
})


test_that("FLXMCLmultinom works if only one predictor variable is given", {
	cluster <- kmeans(iris[,1], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
})


test_that("FLXMCLmultinom: training data from only one class", {
	cluster <- kmeans(iris[1:50,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width + Sepal.Length, data = iris[1:50,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("need two or more classes to fit a multinom model"))
})


test_that("FLXMCLmultinom: missing classes in individual clusters", {
	library(mlbench)
	data(Glass)
	set.seed(120)
	cluster <- kmeans(Glass[,1:9], centers = 2)$cluster
	fit <- flexmix(Type ~ ., data = Glass, concomitant = FLXPmultinom(as.formula(paste("~", paste(colnames(Glass)[1:9], collapse = "+")))), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	pred <- mypredict(fit, aggregate = FALSE)
	expect_equal(colnames(pred$Comp.1), as.character(c(1:3,5:7)))
	expect_equal(colnames(pred$Comp.2), as.character(c(1:3,5:7)))
	expect_equal(pred$Comp.2[,c(1,3)], matrix(0, nrow(pred$Comp.2), 2, dimnames = dimnames(pred$Comp.2[,c(1,3)])))
})



test_that("FLXMCLmultinom: removing clusters works", {
	set.seed(120)	
	data <- benchData::flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLmultinom(trace = FALSE, decay = 0.2), cluster = cluster, control = list(iter.max = 200, verb = 1))
	expect_equal(length(tr2@components), 6)
	expect_equal(ncol(tr2@posterior$scaled), 6)
})


#=================================================================================================================
context("predict FLXMCLmultinom")

test_that("predict FLXMCLmultinom works correctly with missing newdata", {
	set.seed(120)	
	ran <- sample(1:500,300)
	data <- benchData::flashData(500)
	cluster <- kmeans(data$x[ran,], centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data)[ran,], concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1[[1]]), rownames(as.data.frame(data)[ran,]))
})


test_that("predict FLXMCLmultinom works with missing classes in the training data", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[1:100,1:4], centers = 2)$cluster
	expect_that(tr2 <- flexmix(Species ~ ., data = iris[1:100,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200)), gives_warning("group ‘virginica’ is empty")) ## warning
	pred <- mypredict(tr2, aggregate = FALSE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	pred <- mypredict(tr2, aggregate = TRUE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
})


test_that("predict FLXMCLmultinom works with one single predictor variable", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, newdata = iris[-ran,], aggregate = FALSE)
	pred <- mypredict(tr2, aggregate = TRUE)
})


test_that("predict FLXMCLmultinom works with one single test observation", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200))
  	pred <- mypredict(tr2, newdata = iris[5,])
	expect_equal(dim(pred[[1]]), c(1,3))
  	pred <- mypredict(tr2, newdata = iris[5,], aggregate = TRUE)
	expect_equal(dim(pred[[1]]), c(1,3))
})	


test_that("predict FLXMCLmultinom: NA handling in newdata works", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Length + Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200))

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


test_that("predict FLXMCLmultinom: misspecified arguments", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width + Petal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLmultinom(trace = FALSE), cluster = cluster, control = list(iter.max = 200))
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
# model <- FLXMCLmultinom(trace = FALSE)
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res


# # model <- FLXMCLmultinom(trace = FALSE)
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, k = 2)
# # res

# # model <- FLXMCLmultinom(trace = FALSE)
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
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)

# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
# points(d$x, col = as.numeric(d$y) + 2)

# ## plot predicted local membership
# pred.loc.grid <- predict(res@concomitant, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.loc.grid[,1], length(seq(-6,6,0.2))))

