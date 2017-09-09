context("linear-algebra")

library(expm)

set.seed(123)

test_that('sample_covariance is symmetric and matches the built-in version.', {
  n_samples <- 100
  n_features <- 4

  X <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)
  sigma <- sample_covariance(X)

  # we can calculate the biased estimate from the unbiased estimate
  # by changing the scale factor.
  expected_sigma <- (n_samples - 1) / n_samples * cov(X)

  expect_true(isSymmetric(sigma))
  expect_equal(sigma, expected_sigma)
})

test_that('matrix_sqrt matches the version in the expm package.', {
  n_samples <- 100
  n_features <- 4

  X <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)
  sigma <- sample_covariance(X)

  sigma_sqrt <- matrix_sqrt(sigma)
  expected_sqrt <- sqrtm(sigma)

  expect_equal(sigma_sqrt, expected_sqrt)
})