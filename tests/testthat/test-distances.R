context('distances')

set.seed(123)

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