context("FLXMCLconstant")

test_that("FLXMCLconstant: misspecified arguments", {
	# wrong variable names
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ V1, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'V1' nicht gefunden"))
	expect_that(fit <- flexmix(Species ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ V1), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'V1' nicht gefunden"))
	expect_that(fit <- flexmix(y ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'y' nicht gefunden"))
	# wrong class
	expect_error(fit <- flexmix(iris, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard")))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = constantModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## works , but should not
})


test_that("FLXMCLconstant without concomitant variable model works",{
	cluster <- kmeans(iris[,1:4], centers = 4)$cluster

	## weighted
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	
	## hard
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))	
})


test_that("FLXMCLconstant with several options works",{
	cluster <- kmeans(iris[,1:4], centers = 4)$cluster

	# ## weighted, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# # ok
		
	# ## hard, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# # not monotone

	## weighted, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# not monotone, tolerance?
		
	## hard, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# ok
})


test_that("FLXMCLconstant throws a warning if grouping variable is numeric", {
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(tr2 <- flexmix(Petal.Width ~ Petal.Length + Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Petal.Length + Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), gives_warning("'grouping' was coerced to a factor"))
})


test_that("FLXMCLconstant works if only one predictor variable is given", {
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard", verbose = 1))
})


test_that("FLXMCLconstant: Local and global solution coincide if only one cluster is given", {
	fit <- flexmix(Species ~ ., data = iris, model = FLXMCLconstant(), cluster = 1, control = list(iter.max = 200, classify = "hard"))
	w <- constant(Species ~ ., data = iris)
	expect_equal(fit@components[[1]][[1]]@parameters$prior, w$prior)
	pred <- mypredict(fit)
	p <- predict(w)
	expect_equal(pred[[1]], p$posterior)
})


test_that("FLXMCLconstant: training data from only one class", {
	cluster <- kmeans(iris[1:50,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width + Sepal.Length, data = iris[1:50,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("training data from only one group given"))
})


test_that("FLXMCLconstant: missing classes in clusters", {
	set.seed(123)
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	tr2 <- flexmix(Species ~ ., data = iris, concomitant = FLXPmultinom(as.formula(paste("~", paste(colnames(iris)[1:4], collapse = "+")))), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	expect_equal(tr2@components$Comp.1[[1]]@parameters$prior, c(setosa = 1))
	expect_equal(tr2@components$Comp.2[[1]]@parameters$prior, c(virginica = 1))
	pred1 <- mypredict(tr2, aggregate = FALSE)
})


test_that("FLXMCLconstant: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))
	expect_equal(length(tr2@components), 8)
	expect_equal(ncol(tr2@posterior$scaled), 8)
})

#=================================================================================================================
context("predict FLXMCLconstant")

test_that("predict FLXMCLconstant works correctly with missing newdata", {
	set.seed(120)	
	ran <- sample(1:500,300)
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x[ran,], centers = 4)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data)[ran,], concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1[[1]]), rownames(as.data.frame(data)[ran,]))
})


test_that("predict FLXMCLconstant works with missing classes in the training data", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[1:100,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ ., data = iris[1:100,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, aggregate = FALSE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	pred <- mypredict(tr2, aggregate = TRUE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
})


test_that("predict FLXMCLconstant works with one single predictor variable", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, newdata = iris[-ran,], aggregate = FALSE)
	pred <- mypredict(tr2, aggregate = TRUE)
})


test_that("predict FLXMCLconstant works with one single test observation", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))
  	pred <- mypredict(tr2, newdata = iris[5,])
	expect_equal(dim(pred[[1]]), c(1,3))
  	pred <- mypredict(tr2, newdata = iris[5,], aggregate = TRUE)
	expect_equal(dim(pred[[1]]), c(1,3))
})	


test_that("predict FLXMCLconstant: NA handling in newdata works", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width + Petal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))

	## NAs in explanatory variables are ok
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	pred <- mypredict(tr2, newdata = irisna) 
	pred <- mypredict(tr2, newdata = irisna, aggregate = TRUE) 
	## no NAs in pred since in constant model the explanatory variables are not used for prediction
	
	## NAs in splitting variable are not ok if aggregation is desired
	irisna[1:17,1:3] <- NA
	pred <- mypredict(tr2, newdata = irisna)
	pred <- mypredict(tr2, newdata = irisna, aggregate = TRUE)
	expect_equal(all(is.na(pred[[1]][1:17,])), TRUE)
})


test_that("predict FLXMCLconstant: misspecified arguments", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width + Petal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200))
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
# model <- FLXMCLconstant()
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster)
# res


# model <- FLXMCLconstant()
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, k = 2, control = list(classify = "hard"))
# res
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = res@cluster)
# res


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
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.loc.grid[,1], length(seq(-6,6,0.2))))

