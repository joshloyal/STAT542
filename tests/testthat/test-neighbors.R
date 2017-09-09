context('neighbors')

library(MASS)

set.seed(1)

# generate some fake data
n_features = 4
n_samples = 200
rho = 0.5
V <- rho ^ abs(outer(1:n_features, 1:n_features, "-"))
X <- as.matrix(mvrnorm(n_samples, mu = rep(0, n_features), Sigma = V))
beta <- as.matrix(c(1, 1, 0.5, 0.5))
Y <- X %*% beta + rnorm(n_samples)

test_that('nearest neighbors query.', {
  x <- c(0.5, 0.5, 0.5, 0.5)
  indices <- nearest_neighbors(X, x_query = x, k = 5)

  expected <- c(36, 144, 154, 165, 188)
  expect_equal(indices, expected)
})

test_that('knn predictions using all the data returns the sample mean.', {
  x <- c(0.5, 0.5, 0.5, 0.5)
  pred <- predict_knn_single(X, Y, x_query = x, k = n_samples)

  expect_equal(pred, mean(Y))
})

test_that('knn predictions on k < n_samples is not the mean.', {
  x <- c(0.5, 0.5, 0.5, 0.5)
  pred <- predict_knn_single(X, Y, x_query = x, k = 5)

  expect_false(identical(pred, mean(Y)))
})