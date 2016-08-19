context("FLXMCLsvm")

test_that("FLXMCLsvm: misspecified arguments", {
	# wrong variable names
	cluster <- kmeans(iris[,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ V1, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'V1' nicht gefunden"))
	expect_that(fit <- flexmix(Species ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ V1), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'V1' nicht gefunden"))
	expect_that(fit <- flexmix(y ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Objekt 'y' nicht gefunden"))
	# wrong class
	expect_error(fit <- flexmix(iris, data = iris, concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = constantModel,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("FLXMCLsvm: arguments are passed to wsvm",{
	iris[,1:4] <- scale(iris[,1:4])
	set.seed(123)
	cluster <- kmeans(iris[,"Sepal.Width"], centers = 3)$cluster

	## kernel, cost
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLsvm(kernel = "linear", cost = 2), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$kernel, 0)
	expect_equal(fit@components[[1]][[1]]@parameters$cost, 2)
	expect_true(fit@components[[1]][[1]]@parameters$fitted)

	## degree, coef0
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLsvm(kernel = "polynomial", coef0 = 2, degree = 2, fitted = TRUE, tolerance = 10^-5), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$kernel, 1)
	expect_equal(fit@components[[1]][[1]]@parameters$coef0, 2)
	expect_equal(fit@components[[1]][[1]]@parameters$degree, 2)
	expect_true(fit@components[[1]][[1]]@parameters$fitted)

	## gamma
	fit <- flexmix(Species ~ Sepal.Width, data = iris, model = FLXMCLsvm(kernel = "radial", gamma = 2, fitted = FALSE, tolerance = 10^-5), cluster = cluster, control = list(iter.max = 50, classify = "weighted", verb = 1))
	expect_equal(fit@components[[1]][[1]]@parameters$kernel, 2)
	expect_equal(fit@components[[1]][[1]]@parameters$gamma, 2)
	expect_true(!fit@components[[1]][[1]]@parameters$fitted)
})


test_that("FLXMCLsvm with several options works",{
	cluster <- kmeans(iris[,1], centers = 2)$cluster

	# ## weighted, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# # ok
		
	# ## hard, FLXPwlda
	# fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPwlda(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# # not monotone, empty classes

	## weighted, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
	# ok
	
	## hard, FLXPmultinom
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1))
	# ok
})


test_that("FLXMCLsvm throws a warning if grouping variable is numeric", {
	iris$Species <- as.numeric(iris$Species)
	cluster <- kmeans(iris[,c(1,3)], centers = 2)$cluster
	expect_that(tr2 <- flexmix(Species ~ Sepal.Length, data = iris, concomitant = FLXPmultinom(~ Petal.Length + Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE, tolerance = 10^-6), cluster = cluster, control = list(iter.max = 50, classify = "weighted", tolerance = 10^-4, verb = 1)), gives_warning("'grouping' was coerced to a factor"))
	## use weighted here instead of hard, otherwise SVM does not work
})


test_that("FLXMCLsvm works if only one predictor variable is given", {
	cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "weighted", verb = 1))
})


test_that("FLXMCLsvm: Local and global solution coincide if only one cluster is given", {
	fit <- flexmix(Species ~ ., data = iris, model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = 1, control = list(iter.max = 200, classify = "hard"))
	w <- wsvm(Species ~ ., data = iris, kernel = "linear")
	expect_equal(fit@components[[1]][[1]]@parameters$coefs, w$coefs)
	pred <- max.col(mypredict(fit)[[1]])
	p <- predict(w)
	expect_equal(pred, as.numeric(p))
})


test_that("FLXMCLsvm: training data from only one class", {
	cluster <- kmeans(iris[1:50,1:4], centers = 3)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width + Sepal.Length, data = iris[1:50,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("need training data from at least two classes"))
})


test_that("FLXMCLsvm: missing classes in individual clusters", {
	set.seed(123)
	cluster <- kmeans(iris[,1], centers = 2)$cluster
	expect_that(fit <- flexmix(Species ~ Sepal.Width, data = iris, concomitant = FLXPmultinom(~ Sepal.Length), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 100, classify = "hard")), gives_warning("some groups are empty"))
	expect_equal(ncol(fit@components$Comp.1[[1]]@parameters$coefs), 2)
	expect_equal(ncol(fit@components$Comp.2[[1]]@parameters$coefs), 1)
	pred <- mypredict(fit, aggregate = FALSE)
	expect_equal(colnames(pred$Comp.1), levels(iris$Species))
	expect_equal(colnames(pred$Comp.2), levels(iris$Species))
	expect_true(all(pred$Comp.2[,1] == 0))

	irisna <- iris
	irisna$Sepal.Width[1:12] <- NA
	pred <- mypredict(fit, newdata = irisna, aggregate = FALSE)
	expect_equal(colnames(pred$Comp.1), levels(iris$Species))
	expect_equal(colnames(pred$Comp.2), levels(iris$Species))
	expect_true(all(pred$Comp.2[-c(1:12),1] == 0))

	# library(mlbench)
	# data(Glass)
	# set.seed(120)
	# cluster <- kmeans(Glass[,1], centers = 2)$cluster
	# expect_that(fit <- flexmix(Type ~ Na, data = Glass, concomitant = FLXPwlda(as.formula(paste("~", paste(colnames(Glass)[1:9], collapse = "+")))), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", verb = 1)), gives_warning("some groups are empty"))
	# expect_equal(ncol(fit@components$Comp.1[[1]]@parameters$coefs), 4)
	# expect_equal(ncol(fit@components$Comp.2[[1]]@parameters$coefs), 5)
	# pred <- mypredict(fit, aggregate = FALSE)
	# expect_equal(colnames(pred$Comp.1), as.character(c(1:3,5:7)))
	# expect_equal(colnames(pred$Comp.2), as.character(c(1:3,5:7)))
	# expect_equal(colnames(pred$Comp.1[[2]]), c("1/2", "1/3", "1/5", "1/6", "1/7", "2/3", "2/5", "2/6", "2/7", "3/5", "3/6", "3/7", "5/6", "5/7", "6/7"))
	# expect_equal(colnames(pred$Comp.2[[2]]), c("1/2", "1/3", "1/5", "1/6", "1/7", "2/3", "2/5", "2/6", "2/7", "3/5", "3/6", "3/7", "5/6", "5/7", "6/7"))
	# expect_true(all(pred$Comp.2[[1]][,5] == 0))
	# expect_true(all(pred$Comp.2[[2]][,c(4,8,11,13,15)] == 0))
	
	# Glassna <- Glass
	# Glassna$Na[1:12] <- NA
	# pred <- mypredict(fit, newdata = Glassna, aggregate = FALSE)
	# expect_equal(colnames(pred$Comp.1[[1]]), as.character(c(1:3,5:7)))
	# expect_equal(colnames(pred$Comp.2[[1]]), as.character(c(1:3,5:7)))
	# expect_equal(colnames(pred$Comp.1[[2]]), c("1/2", "1/3", "1/5", "1/6", "1/7", "2/3", "2/5", "2/6", "2/7", "3/5", "3/6", "3/7", "5/6", "5/7", "6/7"))
	# expect_equal(colnames(pred$Comp.2[[2]]), c("1/2", "1/3", "1/5", "1/6", "1/7", "2/3", "2/5", "2/6", "2/7", "3/5", "3/6", "3/7", "5/6", "5/7", "6/7"))
	# expect_true(all(pred$Comp.2[[1]][-c(1:12),5] == 0))
	# expect_true(all(pred$Comp.2[[2]][-c(1:12),c(4,8,11,13,15)] == 0))
	# expect_true(all(is.na(pred$Comp.1[[1]][1:12,])))
	# expect_true(all(is.na(pred$Comp.1[[2]][1:12,])))
	# expect_true(all(is.na(pred$Comp.2[[1]][1:12,])))
	# expect_true(all(is.na(pred$Comp.2[[2]][1:12,])))
})


test_that("FLXMCLsvm: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLsvm(tolerance = 10^-6), cluster = cluster, control = list(iter.max = 100, verb = 1))
	expect_equal(length(tr2@components), 8)
	expect_equal(ncol(tr2@posterior$scaled), 8)
})


#=================================================================================================================
context("predict FLXMCLsvm")

test_that("predict FLXMCLsvm works correctly with missing newdata", {
	set.seed(120)	
	ran <- sample(1:500,300)
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x[ran,], centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data)[ran,], concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	expect_equal(rownames(pred1$Comp.1), rownames(as.data.frame(data)[ran,]))
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = as.data.frame(data)[ran,])
	expect_equal(pred1, pred2)
	expect_equal(rownames(pred1[[1]]), rownames(as.data.frame(data)[ran,]))
})


test_that("predict FLXMCLsvm works with missing classes in the training data", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[1:100,1:4], centers = 2)$cluster
	expect_that(tr2 <- flexmix(Species ~ ., data = iris[1:100,], concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length), model = FLXMCLsvm(), cluster = cluster, control = list(iter.max = 200)), gives_warning("some groups are empty"))
	pred <- mypredict(tr2, aggregate = FALSE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	expect_equal(ncol(pred[[2]]), 3) #!!!
	expect_true(all(pred[[1]][,3] == 0))
	expect_true(all(pred[[2]][,3] == 0))
	pred <- mypredict(tr2, aggregate = TRUE)
	expect_equal(ncol(pred[[1]]), 3) #!!!
	expect_true(all(pred[[1]][,3] == 0))
})


test_that("predict FLXMCLsvm works with one single predictor variable", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred <- mypredict(tr2, newdata = iris[-ran,], aggregate = FALSE)
	pred <- mypredict(tr2, aggregate = TRUE)
})


test_that("predict FLXMCLsvm works with one single test observation", {
	set.seed(120)
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200))
  	pred <- mypredict(tr2, newdata = iris[5,])
	expect_equal(dim(pred[[1]]), c(1,3))
	expect_equal(dim(pred[[2]]), c(1,3))
  	pred <- mypredict(tr2, newdata = iris[5,], aggregate = TRUE)
	expect_equal(dim(pred[[1]]), c(1,3))
})	


test_that("predict FLXMCLsvm: NA handling in newdata works", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Length + Sepal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200))

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


test_that("predict FLXMCLsvm: misspecified arguments", {
	ran <- sample(1:150,100)
	cluster <- kmeans(iris[ran,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ Sepal.Width + Petal.Width, data = iris[ran,], concomitant = FLXPmultinom(~ Sepal.Width + Petal.Width), model = FLXMCLsvm(), cluster = cluster, control = list(iter.max = 200))
    # errors in newdata
    expect_error(mypredict(tr2, newdata = TRUE))
    expect_error(mypredict(tr2, newdata = -50:50))
})

#=================================================================================================================

# library(locClass)
# d <- flashData(500)
# #d <- vNormalData(500)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))

# model <- FLXMCLsvm(kernel = "linear")
# cluster <- kmeans(d$x, center = 2)$cluster
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res

# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,1])
# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,2])

# plot(d$x, col = d$y, cex = res@posterior$scaled[,1])
# plot(d$x, col = d$y, cex = res@posterior$scaled[,2])

# pred <- predict(res, newdata = as.data.frame(d), local.aggregate = TRUE)

# pred.grid <- predict(res, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["decision",][[1]], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["decision",][[1]], length(seq(-6,6,0.2))), add = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["decision",][[1]], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["decision",][[1]], length(seq(-6,6,0.2))), add = TRUE)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["posterior",][[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["posterior",][[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["posterior",][[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["posterior",][[1]][,1], length(seq(-6,6,0.2))), add = TRUE)


# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$decision, length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$decision, length(seq(-6,6,0.2))), add = TRUE)
# points(d$x, col = as.numeric(d$y)+2)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$posterior[,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$posterior[,1], length(seq(-6,6,0.2))), add = TRUE)
# points(d$x, col = as.numeric(d$y)+2)

# ## plot predicted local membership
# pred.loc.grid <- predict(res@concomitant, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.loc.grid[,1], length(seq(-6,6,0.2))))

