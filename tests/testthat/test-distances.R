context('distances')

library(MASS)

set.seed(1)

n_features = 4
n_samples = 200
rho = 0.5
V <- rho ^ abs(outer(1:n_features, 1:n_features, "-"))
X <- as.matrix(mvrnorm(n_samples, mu = rep(0, n_features), Sigma = V))
beta <- as.matrix(c(1, 1, 0.5, 0.5))
Y <- X %*% beta + rnorm(n_samples)
sigma <- sample_covariance(X)

test_that('euclidean distance matches built-in.', {
  x1 <- c(0.1, 0.2, 0.3)
  x2 <- c(0.4, 0.5, 0.6)

  expected_dist <- as.numeric(dist(rbind(x1, x2)))

  expect_equal(mydist(x1, x2), expected_dist)
})

test_that('mahalanobis distances matches euclidian for un-correlated zero-mean data.', {
  x1 <- c(0.1, 0.2)
  x2 <- c(0.3, 0.5)

  d1 <- mydist2(x1, x2, s = diag(1, nrow = 2, ncol = 2))
  d2 <- mydist(x1, x2)

  expect_equal(d1, d2)
})

test_that('mahalanobis matches the calculation in the `stats` package.', {
  x1 <- X[1, ]
  x2 <- X[2, ]

  d1 <- mydist2(x1, x2, s = sigma)
  d2 <- sqrt(mahalanobis(x1, center = x2, cov = sigma))

  expect_equal(d1, d2)
})