context("generatewf")

test_that("generatewf: if there are no ties k values are larger than zero", {
	k <- 50
	x <- (0:100)/1000

	expect_equal(sum(biweight(k = k)(x) > 0), k)
	expect_equal(sum(cauchy(k = k)(x) > 0), k)
	expect_equal(sum(cosine(k = k)(x) > 0), k) ## problem is the cosine function
	expect_equal(sum(epanechnikov(k = k)(x) > 0), k)
	expect_equal(sum(exponential(k = k)(x) > 0), k)
	expect_equal(sum(gaussian(k = k)(x) > 0), k)
	expect_equal(sum(optcosine(k = k)(x) > 0), k)
	expect_equal(sum(rectangular(k = k)(x) > 0), k)
	expect_equal(sum(triangular(k = k)(x) > 0), k)

	expect_equal(sum(locClass:::generatewf("biweight", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cauchy", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cosine", k = k, n = 101)(x) > 0), k) ##
	expect_equal(sum(locClass:::generatewf("epanechnikov", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("exponential", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("gaussian", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("optcosine", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("rectangular", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("triangular", k = k, n = 101)(x) > 0), k)

	expect_equal(sum(biweight(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(cauchy(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(cosine(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(epanechnikov(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(exponential(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(gaussian(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(optcosine(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(rectangular(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(triangular(k = k, bw = 5)(x) > 0), k)

	expect_equal(sum(locClass:::generatewf("biweight", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cauchy", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cosine", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("epanechnikov", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("exponential", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("gaussian", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("optcosine", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("rectangular", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("triangular", k = k, bw = 5, n = 101)(x) > 0), k)


	x <- 0:100
	
	expect_equal(sum(biweight(k = k)(x) > 0), k)
	expect_equal(sum(cauchy(k = k)(x) > 0), k)
	expect_equal(sum(cosine(k = k)(x) > 0), k) ###
	expect_equal(sum(epanechnikov(k = k)(x) > 0), k)
	expect_equal(sum(exponential(k = k)(x) > 0), k)
	expect_equal(sum(gaussian(k = k)(x) > 0), k)
	expect_equal(sum(optcosine(k = k)(x) > 0), k)
	expect_equal(sum(rectangular(k = k)(x) > 0), k)
	expect_equal(sum(triangular(k = k)(x) > 0), k)

	expect_equal(sum(locClass:::generatewf("biweight", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cauchy", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cosine", k = k, n = 101)(x) > 0), k)###
	expect_equal(sum(locClass:::generatewf("epanechnikov", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("exponential", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("gaussian", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("optcosine", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("rectangular", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("triangular", k = k, n = 101)(x) > 0), k)

	expect_equal(sum(biweight(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(cauchy(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(cosine(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(epanechnikov(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(exponential(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(gaussian(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(optcosine(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(rectangular(k = k, bw = 50)(x) > 0), k)
	expect_equal(sum(triangular(k = k, bw = 50)(x) > 0), k)

	expect_equal(sum(locClass:::generatewf("biweight", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cauchy", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cosine", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("epanechnikov", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("exponential", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("gaussian", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("optcosine", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("rectangular", k = k, bw = 50, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("triangular", k = k, bw = 50, n = 101)(x) > 0), k)

})


test_that("generatewf: if there are ties k values are larger than zero", {
	z <- 3
	k <- 50
	x <- (0:100)/1000
	x <- c(x, rep(x[k],z))#, x[seq(2, 100, 2)])

	expect_equal(sum(biweight(k = k)(x) > 0), k)
	expect_equal(sum(cauchy(k = k)(x) > 0), k)
	expect_equal(sum(cosine(k = k)(x) > 0), k) ###
	expect_equal(sum(epanechnikov(k = k)(x) > 0), k)
	expect_equal(sum(exponential(k = k)(x) > 0), k)
	expect_equal(sum(gaussian(k = k)(x) > 0), k)
	expect_equal(sum(optcosine(k = k)(x) > 0), k)
	expect_equal(sum(rectangular(k = k)(x) > 0), k)
	expect_equal(sum(triangular(k = k)(x) > 0), k)

	expect_equal(sum(locClass:::generatewf("biweight", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cauchy", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cosine", k = k, n = 101)(x) > 0), k) ###
	expect_equal(sum(locClass:::generatewf("epanechnikov", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("exponential", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("gaussian", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("optcosine", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("rectangular", k = k, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("triangular", k = k, n = 101)(x) > 0), k)

	expect_equal(sum(biweight(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(cauchy(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(cosine(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(epanechnikov(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(exponential(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(gaussian(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(optcosine(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(rectangular(k = k, bw = 5)(x) > 0), k)
	expect_equal(sum(triangular(k = k, bw = 5)(x) > 0), k)

	expect_equal(sum(locClass:::generatewf("biweight", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cauchy", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("cosine", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("epanechnikov", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("exponential", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("gaussian", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("optcosine", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("rectangular", k = k, bw = 5, n = 101)(x) > 0), k)
	expect_equal(sum(locClass:::generatewf("triangular", k = k, bw = 5, n = 101)(x) > 0), k)
})


test_that("generatewf: R and C implementations of weight functions give equal results", {

	names <- c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")

	## variant 1: fixed bandwith
	N <- 150
	k <- NULL
	bw <- 1.5
	weights <- numeric(N)	
	dist <- abs(iris[1,1] - iris[,1])
# par(mfrow = c(2,5))
	for (name in names) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 1, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		w1 <- f(weights, dist, N, bw, k)
		w2 <- get(name, mode = "function")(bw = bw)(dist)	
# plot(dist, w2, main = name)
		w3 <- locClass:::generatewf(name, bw = bw)(dist)
		expect_equal(w1, w2)
		expect_equal(w1, w3)
	}


	## variant 2: fixed bandwidth, nn only
	N <- 150
	k <- 100
	bw <- 1
	weights <- numeric(N)	
	dist <- sort(abs(iris[1,1] - iris[,1]))

# par(mfrow = c(2,5))
	for (name in names) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 2, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		set.seed(123)
		w1 <- f(weights, dist, N, bw, k)
		set.seed(123)
		w2 <- get(name, mode = "function")(bw = bw, k = k)(dist)	
# plot(dist, w2, main = name)
		set.seed(123)
		w3 <- locClass:::generatewf(name, bw = bw, k = k, n = N)(dist)
		expect_equal(w1, w2)
		expect_equal(w1, w3)
		expect_equal(w2, w3)
	}
	
	
	## variant 3: adaptive bandwidth, nn only
	N <- 150
	k <- 100
	bw <- NULL
	weights <- numeric(N)	
	dist <- abs(iris[1,1] - iris[,1])

# par(mfrow = c(2,5))
	for (name in names) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 3, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		set.seed(123)
		w1 <- f(weights, dist, N, bw, k)
		set.seed(123)
		w2 <- get(name, mode = "function")(k = k)(dist)	
# plot(dist, w2, main = name)
		set.seed(123)
		w3 <- locClass:::generatewf(name, k = k, n = N)(dist)
		expect_equal(w1, w2)
		expect_equal(w1, w3)
		expect_equal(w2, w3)
	}


	## variant 4: adaptive bandwith, all observations
	N <- 150
	k <- 100
	bw <- NULL
	weights <- numeric(N)	
	dist <- abs(iris[1,1] - iris[,1])

# par(mfrow = c(2,5))
	for (name in c("cauchy", "exponential", "gaussian")) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 4, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		w1 <- f(weights, dist, N, bw, k)
		w2 <- get(name, mode = "function")(k = k, nn.only = FALSE)(dist)	
# plot(dist, w2, main = name)
		w3 <- locClass:::generatewf(name, k = k, n = N, nn.only = FALSE)(dist)
		expect_equal(w1, w2)
		expect_equal(w1, w3)
	}
	
})


test_that("generatewf: weights functions throw error in case of negative distances", {

	names <- c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")

	## variant 1: fixed bandwith
	N <- 150
	k <- NULL
	bw <- 1.5
	weights <- numeric(N)	
	dist <- -abs(iris[1,1] - iris[,1])
	for (name in names) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 1, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		expect_that(f(weights, dist, N, bw, k), throws_error("'dist' must be positive"))
		expect_that(get(name, mode = "function")(bw = bw)(dist), throws_error("'x' must be positive"))
		expect_that(locClass:::generatewf(name, bw = bw)(dist), throws_error("'x' must be positive"))
	}


	## variant 2: fixed bandwidth, nn only
	N <- 150
	k <- 100
	bw <- 1
	weights <- numeric(N)	
	dist <- sort(-abs(iris[1,1] - iris[,1]))

	for (name in names) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 2, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		expect_that(f(weights, dist, N, bw, k), throws_error("'dist' must be positive"))
		expect_that(get(name, mode = "function")(bw = bw, k = k)(dist), throws_error("'x' must be positive"))
		expect_that(locClass:::generatewf(name, bw = bw, k = k, n = N)(dist), throws_error("'x' must be positive"))
	}
	
	
	## variant 3: adaptive bandwidth, nn only
	N <- 150
	k <- 100
	bw <- NULL
	weights <- numeric(N)	
	dist <- -abs(iris[1,1] - iris[,1])

	for (name in names) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 3, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		expect_that(f(weights, dist, N, bw, k), throws_error("'dist' must be positive"))
		expect_that(get(name, mode = "function")(k = k)(dist), throws_error("'x' must be positive"))
		expect_that(locClass:::generatewf(name, k = k, n = N)(dist), throws_error("'x' must be positive"))
	}


	## variant 4: adaptive bandwith, all observations
	N <- 150
	k <- 100
	bw <- NULL
	weights <- numeric(N)	
	dist <- -abs(iris[1,1] - iris[,1])

	for (name in c("cauchy", "exponential", "gaussian")) {
		f <- function(weights, dist, N, bw, k) {
			.C(paste(name, 4, sep = ""),
				weights = as.double(weights),
				as.double(dist),
				as.integer(N),
				as.double(bw),
				as.integer(k))$weights
		}
		expect_that(f(weights, dist, N, bw, k), throws_error("'dist' must be positive"))
		expect_that(get(name, mode = "function")(k = k, nn.only = FALSE)(dist), throws_error("'x' must be positive"))
		expect_that(locClass:::generatewf(name, k = k, n = N, nn.only = FALSE)(dist), throws_error("'x' must be positive"))
	}
	
})
