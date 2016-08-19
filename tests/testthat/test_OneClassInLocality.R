context("behavior of classification methods if only one class is given")


####	global ================================================================================

test_that("behavior of classification methods if only one class is given: majority", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		majority - ok
	##		weights > 0 for only one class
	fit <- majority(Species ~ ., data = iris, weights = c(rep(1,50), rep(0,100)))
	expect_equal(fit$prior, c(setosa = 1))
	expect_equal(fit$counts, c(setosa = 50, versicolor = 50, virginica = 50))
	expect_equal(fit$lev, c("setosa", "versicolor", "virginica"))
	expect_equal(fit$N, 150)
	pred <- predict(fit)
	expect_equal(ncol(pred$posterior), 1)		# 1 column, FIXME: better 3 columns?
	##		only one class in training data
	expect_that(fit <- majority(Species ~ ., data = irisOne), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: wlda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		wlda - ok
	##		weights > 0 for only one class
	fit <- wlda(Species ~ ., data = iris, weights = c(rep(1,50), rep(0,100)))
	expect_equal(fit$prior, c(setosa = 1))
	expect_equal(fit$counts, c(setosa = 50, versicolor = 50, virginica = 50))
	expect_equal(nrow(fit$means), 1)
	expect_equal(fit$lev, c("setosa", "versicolor", "virginica"))
	expect_equal(fit$N, 150)
	pred <- predict(fit)
	expect_equal(ncol(pred$posterior), 1)		# 1 column, FIXME: better 3 columns?
	##		only one class in training data
	expect_that(fit <- wlda(Species ~ ., data = irisOne), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: wqda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		wqda - ok
	##		weights > 0 for only one class
	fit <- wqda(Species ~ ., data = iris, weights = c(rep(1,50), rep(0,100)))
	expect_equal(fit$prior, c(setosa = 1))
	expect_equal(fit$counts, c(setosa = 50, versicolor = 50, virginica = 50))
	expect_equal(nrow(fit$means), 1)
	expect_equal(fit$lev, c("setosa", "versicolor", "virginica"))
	expect_equal(fit$N, 150)
	pred <- predict(fit)
	expect_equal(ncol(pred$posterior), 1)		# 1 column, FIXME: better three columns?
	##		only one class in training data
	expect_that(fit <- wqda(Species ~ ., data = irisOne), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: multinom", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		multinom - ok
	##		weights > 0 for only one class
	fit <- multinom(Species ~ ., data = iris, weights = c(rep(1,50), rep(0,100)), trace = FALSE)	# ok
	pred <- predict(fit)
	expect_equal(pred, factor(rep("setosa", 150), levels = c("setosa", "versicolor", "virginica")))
	pred <- predict(fit, type = "prob")												# 3 columns
	a <- rep(1, 150)
	names(a) <- 1:150
	expect_equal(pred[,1], a)
	##		only one class in training data
	expect_that(fit <- multinom(Species ~ ., data = irisOne), throws_error("need two or more classes to fit a multinom model"))
})


test_that("behavior of classification methods if only one class is given: nnet", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		nnet - ok
	##		weights > 0 for only one class
	fit <- nnet(Species ~ ., data = iris, weights = c(rep(1,50), rep(0,100)), size = 1, trace = FALSE)	# ok
	pred <- predict(fit)																# 3 columns
	a <- rep(1, 150)
	names(a) <- 1:150
	expect_equal(pred[,1], a)
	##		only one class in training data
	expect_that(fit <- nnet(Species ~ ., data = irisOne, size = 1), throws_error("'softmax = TRUE' requires at least two response categories"))
	expect_that(fit <- nnet(Species ~ ., data = irisOne, size = 1, entropy = TRUE), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("behavior of classification methods if only one class is given: wsvm", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		wsvm - not ok, but cannot do anything about it
	##		weights > 0 for only one class
	fit <- wsvm(Species ~ ., data = iris, case.weights = c(rep(1,50), rep(0,100)))	# warning: some groups are empty
	fit <- wsvm(Species ~ ., data = iris, case.weights = c(rep(1,50), rep(0,100)), fitted = FALSE)	# warning: some groups are empty
	fit <- wsvm(Species ~ ., data = iris, case.weights = c(rep(1,50), rep(0,100)), fitted = FALSE, probability = TRUE)	# warning: some groups are empty
	pred <- predict(fit, newdata = iris)															# ok
	a <- rep("setosa", 150)
	names(a) <- 1:150
	expect_equal(pred, factor(a, levels = c("setosa", "versicolor", "virginica")))
	pred <- predict(fit, newdata = iris, decision.values = TRUE)									# NA
	pred <- predict(fit, newdata = iris, probability = TRUE)										# ok
	expect_equal(attr(pred, "probabilities"), matrix(1, 150, 1, dimnames = list(1:150, "setosa")))
	##		only one class in training data
	expect_that(fit <- wsvm(Species ~ ., data = irisOne), throws_error("need training data from at least two classes"))
})

#### mixtures =========================================================================

test_that("behavior of classification methods if only one class is given: FLXMCLmajority", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		FLXMCLmajority - ok
	##		weights > 0 for only one class
	model <- FLXMCLmajority()
	set.seed(123)
	cluster <- kmeans(iris[,-5], center = 8)$cluster
	fit <- flexmix(Species ~ ., data = iris, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster, control = list(classify = "hard"))
	expect_equal(fit@components$Comp.1[[1]]@parameters$prior, c(setosa = 1))
	expect_equal(fit@components$Comp.3[[1]]@parameters$prior, c(virginica = 1))
	expect_equal(fit@components$Comp.4[[1]]@parameters$prior, c(setosa = 1))
	pred <- predict(fit)
	expect_true(all(pred$Comp.1[,2:3] == 0))
	expect_true(all(pred$Comp.3[,1:2] == 0))
	expect_true(all(pred$Comp.4[,2:3] == 0))
	##		only one class in training data
	cluster <- kmeans(irisOne[,-5], center = 5)$cluster
	expect_that(fit <- flexmix(Species ~ ., data = irisOne, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster), throws_error("training data from only one group given"))
})


### beim renormalisieren der priors nach dem entfernen zu kleiner komponenten entstehen NAs
# model <- FLXMCLmultinom()
# cluster <- kmeans(iris[,-5], center = 10)$cluster
# fit <- flexmix(Species ~ ., data = iris, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster, control = list(classify = "hard"))


test_that("behavior of classification methods if only one class is given: FLXMCLlda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	####	FLXMCLlda - ok
	##		weights > 0 for only one class
	model <- FLXMCLlda()
	set.seed(123)
	cluster <- kmeans(iris[,-5], center = 4)$cluster
	fit <- flexmix(Species ~ ., data = iris, model = model, cluster = cluster, control = list(classify = "hard"))
	expect_equal(fit@components$Comp.1[[1]]@parameters$prior, c(setosa = 1))
	expect_equal(fit@components$Comp.2[[1]]@parameters$prior, c(virginica = 1))
	expect_equal(nrow(fit@components$Comp.1[[1]]@parameters$means), 1)
	expect_equal(nrow(fit@components$Comp.2[[1]]@parameters$means), 1)
	pred <- predict(fit)
	expect_true(all(pred$Comp.1[,2:3] == 0))
	expect_true(all(pred$Comp.2[,1:2] == 0))
	##		only one class in training data
	cluster <- kmeans(irisOne[,-5], center = 5)$cluster
	expect_that(fit <- flexmix(Species ~ ., data = irisOne, model = model, cluster = cluster), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: FLXMCLqda", {
	benchData::xor3Data(1000)
	data <- as.data.frame(data)

	dataOne <- data[data$y == 1, ]
	dataOne$y <- factor(dataOne$y, levels = "1")

	####	FLXMCLqda - 
	##		weights > 0 for only one class, problem ist that I cannot etablish this situation because covariance is singular
	model <- FLXMCLqda()
	set.seed(123)
	cluster <- kmeans(data[,-3], center = 4)$cluster
	fit <- flexmix(y ~ ., data = data, model = model, cluster = cluster, control = list(classify = "hard"))

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	##		only one class in training data
	cluster <- kmeans(irisOne[,-5], center = 5)$cluster
	expect_that(fit <- flexmix(Species ~ ., data = irisOne, model = model, cluster = cluster), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: FLXMCLmultinom", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	#### FLXMCLmultinom - ok, posteriors numerically not 0
	##		weights > 0 for only one class, schwer herzustellen
	model <- FLXMCLmultinom(trace = FALSE, decay = 0.1)
	set.seed(123)
	cluster <- kmeans(iris[,-5], center = 6)$cluster
	fit <- flexmix(Species ~ ., data = iris, model = model, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), cluster = cluster, control = list(classify = "hard"))
	pred <- predict(fit)
	expect_true(all(pred$Comp.1[,2:3] < 0.01))
	expect_true(all(pred$Comp.2[,1:2] < 0.04))
	expect_true(all(pred$Comp.4[,1:2] < 0.03))
	expect_true(all(pred$Comp.6[,2:3] < 0.01))
	##		only one class in training data
	cluster <- kmeans(irisOne[,-5], center = 5)$cluster
	expect_that(fit <- flexmix(Species ~ ., data = irisOne, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster), throws_error("need two or more classes to fit a multinom model"))
})


test_that("behavior of classification methods if only one class is given: FLXMCLnnet", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	#### FLXMCLnnet - ok, posteriors numerically not 0
	##		weights > 0 for only one class, schwer herzustellen
	model <- FLXMCLnnet(trace = FALSE, size = 1, decay = 0.1)
	set.seed(123)
	cluster <- kmeans(iris[,-5], center = 5)$cluster
	fit <- flexmix(Species ~ ., data = iris, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster, control = list(classify = "hard"))
	pred <- predict(fit)
	expect_true(all(pred$Comp.1[,2:3] < 0.01))
	expect_true(all(pred$Comp.2[,1:2] < 0.02))
	expect_true(all(pred$Comp.4[,1:2] < 0.01))
	##		only one class in training data
	cluster <- kmeans(irisOne[,-5], center = 5)$cluster
	expect_that(fit <- flexmix(Species ~ ., data = irisOne, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("behavior of classification methods if only one class is given: FLXMCLsvm", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	#### FLXMCLsvm - ok
	##		weights > 0 for only one class, schwer herzustellen
	model <- FLXMCLsvm(kernel = "linear")
	set.seed(123)
	cluster <- kmeans(iris[,-5], center = 5)$cluster
	fit <- flexmix(Species ~ ., data = iris, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster, control = list(classify = "hard")) 	# error: need training data with positive 'case.weights' from at least two classes
	pred <- predict(fit)
	expect_true(all(pred$Comp.1[,2:3] == 0))
	expect_true(all(pred$Comp.2[,1:2] == 0))
	expect_true(all(pred$Comp.4[,1:2] == 0))
	####
	##		only one class in training data
	cluster <- kmeans(irisOne[,-5], center = 5)$cluster
	expect_that(fit <- flexmix(Species ~ ., data = irisOne, concomitant = FLXPmultinom(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length), model = model, cluster = cluster), throws_error("need training data from at least two classes"))
})


####	trees ============================================================================

test_that("behavior of classification methods if only one class is given: majorityModel", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		majority - ok
	##		weights > 0 for only one class
	set.seed(123)
	fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = iris, model = majorityModel,
	     control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, 2)
	expect_equal(terminal[[1]]$model$prior, c(setosa = 1))
	terminal <- nodes(fit, 7)
	expect_equal(terminal[[1]]$model$prior, c(virginica = 1))
	pred <- predict(fit)
	expect_equal(pred[iris$Petal.Length <= 1.9], rep(1,50))
	##		only one class in training data - ok
	expect_that(fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = irisOne, model = majorityModel,
	     control = mob_control(objfun = deviance, minsplit = 50)), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: ldaModel", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	####	lda - ok
	##		weights > 0 for only one class
	set.seed(123)
	fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = iris, model = ldaModel,
	     control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, 2)
	expect_equal(terminal[[1]]$model$prior, c(setosa = 1))
	terminal <- nodes(fit, 5)
	expect_equal(terminal[[1]]$model$prior, c(versicolor = 1))
	pred <- predict(fit)
	expect_equal(pred[iris$Petal.Length <= 1.9], rep(1,50))
	##		only one class in training data - ok
	expect_that(fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = irisOne, model = ldaModel,
	     control = mob_control(objfun = deviance, minsplit = 50)), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: qdaModel", {
	set.seed(110)
	data <- benchData::xor3Data(300)
	data <- as.data.frame(data)

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	set.seed(123)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = qdaModel,
	     control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, 16)
	expect_equal(terminal[[1]]$model$prior, c("1" = 1))
	terminal <- nodes(fit, 11)
	expect_equal(terminal[[1]]$model$prior, c("2" = 1))
	pred <- predict(fit)
	nodes <- predict(fit, type = "node")
	expect_equal(pred[nodes == 16], rep(1,21))
	expect_equal(pred[nodes == 11], rep(2,21))
	##		only one class in training data - ok
	expect_that(fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = irisOne, model = qdaModel,
	     control = mob_control(objfun = deviance, minsplit = 50)), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: multinomModel", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	####		multinom - ok
	##		weights > 0 for only one class
	set.seed(123)
	fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = iris, model = multinomModel, trace = FALSE,
	     control = mob_control(objfun = deviance, minsplit = 10))
	# node <- predict(fit, type = "node")
	# table(node, iris$Species)
	expect_equal(predict(fit, newdata = iris[iris$Petal.Length <= 4.7 & iris$Sepal.Length <= 4.7,]), rep(1,11))
	##		only one class in training data - ok
	expect_that(fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = irisOne, model = multinomModel, trace = FALSE,
	     control = mob_control(objfun = deviance, minsplit = 50)), throws_error("need two or more classes to fit a multinom model"))
})


test_that("behavior of classification methods if only one class is given: nnetModel", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	####		nnet - ok
	##		weights > 0 for only one class
	set.seed(123)
	fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = iris, model = nnetModel, trace = FALSE, size = 1,
	     control = mob_control(objfun = deviance, minsplit = 10))
	# node <- predict(fit, type = "node")
	# table(node, iris$Species)
	expect_equal(predict(fit, newdata = iris[iris$Petal.Length > 5.3 & iris$Sepal.Length > 6,]), rep(3,30))
	##		only one class in training data - ok
	expect_that(fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = irisOne, model = nnetModel, trace = FALSE, size = 1,
	     control = mob_control(objfun = deviance, minsplit = 50)), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("behavior of classification methods if only one class is given: svmModel", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	####	svm
	##		weights > 0 for only one class, cannot establish this situation because these splits are not taken
	set.seed(123)
	fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = iris, model = svmModel, kernel = "linear",
	     control = mob_control(objfun = deviance, minsplit = 10))
	terminal <- nodes(fit, 3)
	expect_equal(terminal[[1]]$model$nSV, 0)
	expect_equal(terminal[[1]]$model$obj, 0)
	pred <- predict(fit)
	nodes <- predict(fit, type = "node")
	expect_equal(pred[nodes == 3], rep(1,50))
	# node <- predict(fit, type = "node")
	# table(node, iris$Species)
	# expect_equal(predict(fit, newdata = iris[iris$Petal.Length > 5.3 & iris$Sepal.Length > 6,]), rep(3,30))
	##		only one class in training data - ok
	expect_that(fit <- mob(Species ~ Petal.Width + Sepal.Width | Petal.Length + Sepal.Length, data = irisOne, model = svmModel, kernel = "linear",
	     control = mob_control(objfun = deviance, minsplit = 50)), throws_error("need training data from at least two classes"))
})


####	os ===============================================================================
test_that("behavior of classification methods if only one class is given: osmajority", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		majority - ok
	##		small k
	fit <- kda(Species ~ ., data = iris, wf = "rectangular", k = 2)
	pred <- predict(fit, newdata = iris[102,])
	pred <- predict(fit)
	### NaNs in Vorhersage??? ist ok, liegt daran, dass Testbeobachtungen = Trainingsbeobachtungen sind und zusätzlich Bindungen auftreten, so dass adaptive bandbreite = 0 ist
	##		all neighbors in the same class
	fit <- kda(Species ~ ., data = iris, wf = "rectangular", k = 15)
	pred <- predict(fit, newdata = iris[119,])
	expect_equal(pred$posterior, matrix(c(0,0,1), 1, dimnames = list(119, c("setosa", "versicolor", "virginica"))))
	##		only one class in training data - ok
	expect_that(fit <- kda(Species ~ ., data = irisOne, wf = "rectangular", k = 2), throws_error("training data from only one group given"))
})	


test_that("behavior of classification methods if only one class is given: oslda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		lda - ok
	##		small k
	fit <- oslda(Species ~ ., data = iris, wf = "rectangular", k = 2)
	pred <- predict(fit)
	# singular, NA
	fit <- oslda(Species ~ ., data = iris, wf = "rectangular", k = 3)
	pred <- predict(fit, newdata = iris[119,])
	# singular, NA
	fit <- oslda(Species ~ ., data = iris, wf = "rectangular", k = 4)
	pred <- predict(fit, newdata = iris[119,])
	# ok
	##		all neighbors in the same class
	fit <- oslda(Species ~ ., data = iris, wf = "rectangular", k = 15)
	pred <- predict(fit, newdata = iris[119,])
	expect_equal(pred$posterior, matrix(c(0,0,1), 1, dimnames = list(119, c("setosa", "versicolor", "virginica"))))
	# ok
	##		only one class in training data - ok
	expect_that(fit <- oslda(Species ~ ., data = irisOne, wf = "rectangular", k = 2), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: osqda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		qda - ok
	##		small k
	fit <- osqda(Species ~ ., data = iris, wf = "rectangular", k = 2)
	pred <- predict(fit)
	# singular, NA
	##		all neighbors in the same class
	fit <- osqda(Species ~ ., data = iris, wf = "rectangular", k = 3)
	pred <- predict(fit, newdata = iris[119,])
	# singular, NA
	fit <- osqda(Species ~ ., data = iris, wf = "rectangular", k = 4)
	pred <- predict(fit, newdata = iris[119,])
	# ok
	##		all neighbors in the same class
	fit <- osqda(Species ~ ., data = iris, wf = "rectangular", k = 15)
	pred <- predict(fit, newdata = iris[119,])
	expect_equal(pred$posterior, matrix(c(0,0,1), 1, dimnames = list(119, c("setosa", "versicolor", "virginica"))))
	# ok
	##		only one class in training data - ok
	expect_that(fit <- osqda(Species ~ ., data = irisOne, wf = "rectangular", k = 2), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: osmultinom", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		multinom - ok
	##		small k
	fit <- osmultinom(Species ~ ., data = iris, wf = "rectangular", k = 2, decay = 0.1, trace = FALSE)
	expect_error(pred <- predict(fit))
	# error: Anfangswert in vmmin ist nicht endlich
	##		all neighbors in the same class
	fit <- osmultinom(Species ~ ., data = iris, wf = "rectangular", k = 3, decay = 0.1, trace = FALSE)
	pred <- predict(fit, newdata = iris[119,])
	pred <- predict(fit, newdata = iris[119,], type = "prob")	# nicht 0-1
	# ok
	fit <- osmultinom(Species ~ ., data = iris, wf = "rectangular", k = 15, decay = 0.1, trace = FALSE)
	pred <- predict(fit, newdata = iris[119,])
	expect_equal(pred, factor(c("119" = "virginica"), levels = c("setosa", "versicolor", "virginica")))
	pred <- predict(fit, newdata = iris[119,], type = "prob")	# nicht 0-1
	expect_true(all(pred[,1:2] < 1e-04))
	##		only one class in training data - ok
	expect_that(fit <- osmultinom(Species ~ ., data = irisOne, wf = "rectangular", k = 2), throws_error("need two or more classes to fit a multinom model"))
})


test_that("behavior of classification methods if only one class is given: osnnet", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		nnet
	##		small k
	fit <- osnnet(Species ~ ., data = iris, wf = "rectangular", k = 2, size = 1, decay = 0.1, trace = FALSE)
	expect_error(pred <- predict(fit))
	# error: Anfangswert in vmmin ist nicht endlich
	##		all neighbors in the same class
	fit <- osnnet(Species ~ ., data = iris, wf = "rectangular", k = 3, size = 1, decay = 0.1, trace = FALSE)
	pred <- predict(fit, newdata = iris[119,])			# nicht 0-1
	# ok
	fit <- osnnet(Species ~ ., data = iris, wf = "rectangular", k = 15, size = 1, decay = 0.1, trace = FALSE)
	pred <- predict(fit, newdata = iris[119,])			# nicht 0-1
	expect_true(all(pred[,1:2] < 0.002))
	# ok
	##		only one class in training data - ok
	expect_that(fit <- osnnet(Species ~ ., data = irisOne, size = 1, wf = "rectangular", k = 2), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("behavior of classification methods if only one class is given: ossvm", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		svm
	##		small k
	fit <- ossvm(Species ~ ., data = iris, wf = "rectangular", k = 2, kernel = "linear")
	pred <- predict(fit)
	## ok
	##		all neighbors in the same class
	fit <- ossvm(Species ~ ., data = iris, wf = "rectangular", k = 3, kernel = "linear")
	pred <- predict(fit, newdata = iris[119,])
	expect_equal(pred, factor(c("119" = "virginica"), levels = c("setosa", "versicolor", "virginica")))
	pred <- predict(fit, newdata = iris[119,], decision.values = TRUE)		# sind NA; liegt daran, dass nur eine klasse da ist, FIXME
	pred <- predict(fit, newdata = iris[119,], probability = TRUE)			# ok
	expect_equal(attr(pred, "probabilities"), matrix(c(0,0,1), 1, dimnames = list(119, c("setosa", "versicolor", "virginica"))))
	
	fit <- ossvm(Species ~ ., data = iris, wf = "rectangular", k = 15, kernel = "linear")
	pred <- predict(fit, newdata = iris[119,])
	expect_equal(pred, factor(c("119" = "virginica"), levels = c("setosa", "versicolor", "virginica")))
	pred <- predict(fit, newdata = iris[119,], decision.values = TRUE)		# sind NA; liegt daran, dass nur eine klasse da ist, FIXME
	pred <- predict(fit, newdata = iris[119,], probability = TRUE)			# ok
	expect_equal(attr(pred, "probabilities"), matrix(c(0,0,1), 1, dimnames = list(119, c("setosa", "versicolor", "virginica"))))
	##		only one class in training data - ok
	expect_that(fit <- ossvm(Species ~ ., data = irisOne, size = 1, wf = "rectangular", k = 2), throws_error("training data from only one class"))
})


#### da ===============================================================================
test_that("behavior of classification methods if only one class is given: dalda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		dalda
	##		weights > 0 for only one class
	fit <- dalda(Species ~ ., data = iris, wf = "rectangular", k = 6)
	# expect_equal(fit$prior, c(setosa=1))
	expect_equal(fit$counts, c(setosa = 50, versicolor = 50, virginica = 50))
	# expect_equal(nrow(fit$means), 1)
	expect_equal(fit$lev, c("setosa", "versicolor", "virginica"))
	expect_equal(fit$N, 150)
	pred <- predict(fit)
	# expect_equal(ncol(pred$posterior), 1)
	##		only one class in training data
	expect_that(fit <- dalda(Species ~ ., data = irisOne, wf = "rectangular", k = 20), throws_error("training data from only one group given"))
})
# table(iris$Species[fit$weights[[1]] > 0])


test_that("behavior of classification methods if only one class is given: daqda", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		wqda
	##		weights > 0 for only one class
	fit <- daqda(Species ~ ., data = iris, wf = "rectangular", k = 15)
	# expect_equal(fit$prior, c(setosa=1))
	expect_equal(fit$counts, c(setosa = 50, versicolor = 50, virginica = 50))
	# expect_equal(nrow(fit$means), 1)
	expect_equal(fit$lev, c("setosa", "versicolor", "virginica"))
	expect_equal(fit$N, 150)
	pred <- predict(fit)
	# expect_equal(ncol(pred$posterior), 1)
	##		only one class in training data
	expect_that(fit <- daqda(Species ~ ., data = irisOne, wf = "rectangular", k = 20), throws_error("training data from only one group given"))
})


test_that("behavior of classification methods if only one class is given: damultinom", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		multinom
	##		weights > 0 for only one class
	fit <- damultinom(Species ~ ., data = iris, wf = "rectangular", k = 2)	# ok
	pred <- predict(fit)
	##		only one class in training data
	expect_that(fit <- damultinom(Species ~ ., data = irisOne, wf = "rectangular", k = 20), throws_error("need two or more classes to fit a damultinom model"))
})


test_that("behavior of classification methods if only one class is given: dannet", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		nnet
	##		weights > 0 for only one class
	fit <- dannet(Species ~ ., data = iris, wf = "rectangular", k = 2, size = 1)	# ok
	pred <- predict(fit)
	##		only one class in training data
	expect_that(fit <- nnet(Species ~ ., data = irisOne, size = 1), throws_error("'softmax = TRUE' requires at least two response categories"))
	expect_that(fit <- nnet(Species ~ ., data = irisOne, size = 1, entropy = TRUE), throws_error("'softmax = TRUE' requires at least two response categories"))
})


test_that("behavior of classification methods if only one class is given: dasvm", {

	irisOne <- iris[1:50,]
	irisOne$Species <- factor(irisOne$Species, levels = "setosa")

	###		wsvm
	##		weights > 0 for only one class
	fit <- dasvm(Species ~ ., data = iris, wf = "rectangular", k = 3)					# break out
	pred <- predict(fit, newdata = iris)
	fit <- dasvm(Species ~ ., data = iris, wf = "rectangular", k = 3, fitted = FALSE)	# break out
	pred <- predict(fit, newdata = iris)
	##		only one class in training data
	expect_that(fit <- dasvm(Species ~ ., data = irisOne, wf = "rectangular", k = 20), throws_error("need training data from at least two classes"))
})
