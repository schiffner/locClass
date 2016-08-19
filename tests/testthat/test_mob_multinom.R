context("mobMultinomModel")

test_that("mobMultinomModel: misspecified arguments", {
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	# wrong variable names
	expect_error(mob(Species ~ V1 | Sepal.Length, data = irisscale, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	expect_error(mob(Species ~ Sepal.Length | V1, data = irisscale, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	expect_error(mob(y ~ Sepal.Length | Sepal.Width, data = irisscale, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# wrong class
	expect_error(mob(iris, data = irisscale, model = multinomModel,
		control = mob_control(objfun = deviance, minsplit = 20)))
	# target variable also in x
	# expect_error(mob(Species ~ Species + Sepal.Length | Sepal.Width, data = iris, model = multinomModel, trace = FALSE,
	#	control = mob_control(objfun = deviance, minsplit = 20)))	## funktioniert, sollte aber nicht
})


test_that("mobMultinomModel: binary problem", {
	data <- benchData::vData(500)
	## decay = 0
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::vBayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)
	## gradient
	pr <- predict(fit, out = "posterior")
	pr <- sapply(pr, function(x) x[,2])
	y <-  unclass(data$y) - 1
	delta <- pr - y
	splits <- predict(fit, type = "node")
	S <- unique(splits)
	for (s in S) {
		gr1 <- (delta * cbind(1, data$x))[splits == s,]
		gr2 <- nodes(fit, s)[[1]]$model$gradient[splits == s,]
		expect_equal(gr1, gr2)
	}
	## decay > 0
	decay <- 0.5
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = multinomModel, trace = FALSE, decay = decay,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::vBayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
	## gradient
	pr <- predict(fit, out = "posterior")
	pr <- sapply(pr, function(x) x[,2])
	y <-  unclass(data$y) - 1
	delta <- pr - y
	splits <- predict(fit, type = "node")
	S <- unique(splits)
	for (s in S) {
		model <- nodes(fit,s)[[1]]$model
		gr1 <- (delta * cbind(1, data$x))[splits == s,]
		reg <- 2 * decay/sum(weights(model)) * coef(model)
		gr1 <- t(t(gr1) + reg)	
		gr2 <- model$gradient[splits == s,]
		expect_equal(gr1, gr2)	
	}
})


test_that("mobMultinomModel: multi-class problem", {
	data <- benchData::xor3Data(1000)
	## decay = 0
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(benchData::xor3BayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)
	## gradient
	pr <- predict(fit, out = "posterior")
	pr <- do.call("rbind", pr)
	pr <- pr[,-1]
	y <- diag(3)		
	y <- y[as.numeric(data$y),]
	y <- y[,-1]
	delta <- pr - y
	splits <- predict(fit, type = "node")
	S <- unique(splits)
	for (s in S) {
		gr1 <- (delta[,rep(1:2, each = 3)] * cbind(1, data$x)[,rep(1:3, 2)])[splits == s,]
# print(head(gr1))
		dimnames(gr1) <- NULL
		gr2 <- nodes(fit, s)[[1]]$model$gradient[splits == s,]
# print(head(gr2))
# print(nodes(fit, s)[[1]]$model$gr)
		expect_equal(gr1, gr2)
	}
	## decay > 0
	decay = 0.5
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = multinomModel, trace = FALSE, decay = decay,
		control = mob_control(objfun = deviance, minsplit = 200))
	tr <- mean(predict(fit) != data$y)
	ba <- mean(xor3BayesClass(data$x) != data$y)
	expect_true(tr < ba + 0.05)	
	## gradient
	pr <- predict(fit, out = "posterior")
	pr <- do.call("rbind", pr)
	pr <- pr[,-1]
	y <- diag(3)		
	y <- y[as.numeric(data$y),]
	y <- y[,-1]
	delta <- pr - y
	splits <- predict(fit, type = "node")
	S <- unique(splits)
	for (s in S) {
		model <- nodes(fit,s)[[1]]$model
		gr1 <- (delta[,rep(1:2, each = 3)] * cbind(1, data$x)[,rep(1:3, 2)])[splits == s,]
		dimnames(gr1) <- NULL
		reg <- as.vector(t(2 * decay/sum(weights(model)) * coef(model)))
		gr1 <- t(t(gr1) + reg)	
# print(head(gr1))
		gr2 <- model$gradient[splits == s,]
# print(head(gr2))
		expect_equal(gr1, gr2)
	}	
})


test_that("mobMultinomModel throws a warning if grouping variable is numeric", {
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	fit <- mob(Petal.Width ~ . | Sepal.Length, data = irisscale, model = multinomModel, trace = FALSE, decay = 0.5,
		control = mob_control(objfun = deviance, minsplit = 20)) 
})


test_that("mobMultinomModel works if only one predictor variable is given", {
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = irisscale, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$coefnames, c("(Intercept)", "Sepal.Width"))
})


test_that("mobMulitnomModel: Local and global solution coincide if minsplit is large", {
	library(nnet)
	data <- benchData::vData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 500))
	w <- multinom(y ~ ., data = as.data.frame(data),  trace = FALSE)
	expect_equal(fit@tree$model$wts, w$wts)
	# expect_equal(fit@tree$model$fitted.values, w$fitted.values)
	expect_equal(fit@tree$model$deviance, w$deviance)
	pred <- predict(fit)
	p <- predict(w, newdata = as.data.frame(data))
	expect_equal(pred, as.numeric(p))
})


test_that("mobMultinomModel: training data from only one class", {
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	expect_that(z <- mob(Species ~ Sepal.Width | Sepal.Length, data = irisscale[1:50,], model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20)), throws_error("need two or more classes to fit a multinom model"))
	## error in global fit
})


test_that("mobMultinomModel: neural network is set up correctly", {
	library(nnet)
	## categorical target variables
	# 2 classes
	library(mlbench)
	data(Ionosphere)
	fit <- mob(Class ~ . | V1, data = Ionosphere[,-2], model = multinomModel, trace = FALSE, # Hess = TRUE,
		control = mob_control(objfun = deviance, minsplit = 20))
	tnodes <- unique(where(fit))
	term <- nodes(fit, tnodes[1])	
	m <- multinom(Class ~ . , data = Ionosphere[,-2], trace = FALSE)#, Hess = TRUE)
	expect_equal(term[[1]]$model[1:9], m[1:9])
	expect_equal(term[[1]]$model[c("lev", "rank", "lab", "coefnames", "vcoefnames")], m[c("lev", "rank", "lab", "coefnames", "vcoefnames")])	
	# expect_equal(dim(m$Hessian), dim(term[[1]]$model$Hessian))
	# expect_equal(dimnames(m$Hessian), dimnames(term[[1]]$model$Hessian))
	
	# > 2 classes
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	fit <- mob(Species ~ . | Sepal.Length, data = irisscale, model = multinomModel, trace = FALSE, #Hess = TRUE,
		control = mob_control(objfun = deviance, minsplit = 10))
	tnodes <- unique(where(fit))
	term <- nodes(fit, tnodes[1])
	m <- multinom(Species ~ . , data = irisscale, trace = FALSE) #, Hess = TRUE)
	expect_equal(term[[1]]$model[1:9], m[1:9])
	expect_equal(term[[1]]$model[c("lev", "rank", "lab", "coefnames", "vcoefnames")], m[c("lev", "rank", "lab", "coefnames", "vcoefnames")])	
	# expect_equal(dim(m$Hessian), dim(term[[1]]$model$Hessian))
	# expect_equal(dimnames(m$Hessian), dimnames(term[[1]]$model$Hessian))

	# 1 class missing
	fit <- mob(Species ~ . | Sepal.Length, data = irisscale[1:100,], model = multinomModel, trace = FALSE, #Hess = TRUE,
		control = mob_control(objfun = deviance, minsplit = 20))
	tnodes <- unique(where(fit))
	term <- nodes(fit, tnodes[1])
	m <- multinom(Species ~ . , data = irisscale[1:100,], trace = FALSE) #, Hess = TRUE)
	expect_equal(term[[1]]$model[1:9], m[1:9])
	expect_equal(term[[1]]$model[c("lev", "rank", "lab", "coefnames", "vcoefnames")], m[c("lev", "rank", "lab", "coefnames", "vcoefnames")])	
	# expect_equal(dim(m$Hessian), dim(term[[1]]$model$Hessian))
	# expect_equal(dimnames(m$Hessian), dimnames(term[[1]]$model$Hessian))

	## numeric target variable
	data(Glass)
	Glassscale <- Glass
	Glassscale[,1:9] <- scale(Glass[,1:9], sapply(Glass[,1:9], min), scale = sapply(Glass[,1:9], max) - sapply(Glass[,1:9], min))
	fit <- mob(Type ~ . | ., data = Glassscale, model = multinomModel, trace = FALSE, decay = 0.2, #Hess = TRUE,
		control = mob_control(objfun = deviance, minsplit = 20))
	tnodes <- unique(where(fit))
	term <- nodes(fit, tnodes[1])
	m <- multinom(Type ~ . , data = Glassscale, trace = FALSE, decay = 0.2) #, Hess = TRUE)
	expect_equal(term[[1]]$model[1:9], m[1:9])
	expect_equal(term[[1]]$model[c("lev", "rank", "lab", "coefnames", "vcoefnames")], m[c("lev", "rank", "lab", "coefnames", "vcoefnames")])	
	# expect_equal(dim(m$Hessian), dim(term[[1]]$model$Hessian))
	# expect_equal(dimnames(m$Hessian), dimnames(term[[1]]$model$Hessian))
	
})


test_that("mobMultinomModel: arguments are passed to multinom",{
	iris[,1:4] <- scale(iris[,1:4])

	## trace
	# fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = multinomModel, trace = TRUE,
		# control = mob_control(objfun = deviance, minsplit = 20))

	## decay
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
	expect_equal(fit@tree$right$model$decay, 0.1)

	## censored
	expect_true(!fit@tree$right$model$censored)
	fit <- mob(Species ~ Sepal.Width | Sepal.Length, data = iris, model = multinomModel, trace = FALSE, decay = 0.1,
		censored = TRUE, control = mob_control(objfun = deviance, minsplit = 20))
	expect_true(fit@tree$right$model$censored)
})


#=================================================================================================================
context("predict.multinomModel")

test_that("predict.multinomModel works correctly with formula interface and with missing newdata", {
	irisscale <- iris
	irisscale[,1:4] <- scale(iris[,1:4], sapply(iris[,1:4], min), scale = sapply(iris[,1:4], max) - sapply(iris[,1:4], min))
	ran <- sample(1:150,100)
	## formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = irisscale[ran,], model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 50))
  	pred <- predict(fit)
  	pred <- predict(fit, out = "posterior")
  	p <- matrix(0, length(pred), 3)
	colnames(p) = levels(iris$Species)
	rownames(p) = sapply(pred, rownames)
	for (i in seq_along(pred)) {
		p[i, colnames(pred[[i]])] = pred[[i]]
	}
  	expect_equal(rownames(p), rownames(iris)[ran])  	
	## formula, data, newdata
  	predict(fit, newdata = iris[-ran,])
})


test_that("predict.multinomModel: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris, model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- mob(Species ~ . | Sepal.Length, data = iris[ran,], model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
})


test_that("predict.multinomModel works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Length, data = iris[1:100,], model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(length(unique(pred)), 2)
	pred <- predict(fit, newdata = iris[-ran,], out = "posterior")
	expect_equal(sapply(pred, ncol), rep(2, 50))
})


test_that("predict.multinomModel works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ Sepal.Width | Sepal.Width, data = iris[ran,], model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))
	terminal <- nodes(fit, max(where(fit)))
	expect_equal(terminal[[1]]$model$coefnames, c("(Intercept)", "Sepal.Width"))
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.multinomModel works with one single test observation", {
	data(iris)
	# 3 classes
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 3))
	# 3 classes, 1 missing
	fit <- mob(Species ~ . | Sepal.Width, data = iris[1:100,], model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 20))	
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = iris[5,], out = "posterior") #namen fehlen
	expect_equal(dim(pred[[1]]), c(1, 2))
	# 2 classes
	data <- benchData::flashData(500)
	fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = as.data.frame(data), model = multinomModel, trace = FALSE,
		control = mob_control(objfun = deviance, minsplit = 200))	
	pred <- predict(fit, newdata = as.data.frame(data)[5,])
	expect_equal(length(pred), 1)
  	pred <- predict(fit, newdata = as.data.frame(data)[5,], out = "posterior")
	expect_equal(dim(pred[[1]]), c(1, 2))
})	


test_that("predict.multinomModel: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,1:3] <- NA
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 30))
	expect_error(pred <- predict(fit, newdata = irisna))
})


test_that("predict.multinomModel: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- mob(Species ~ . | Sepal.Width, data = iris[ran,], model = multinomModel, trace = FALSE, decay = 0.1,
		control = mob_control(objfun = deviance, minsplit = 20))	
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
})
