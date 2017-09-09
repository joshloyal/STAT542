## @knitr sample_covariance

#' Sample Variance-Covariance Matrix
#'
#' Calculates the sample variance-covariance matrix using the biased MLE estimate.
#'
#' @param X A matrix of shape [n_samples, n_features].
#' @return The covariance matrix of \code{X}. Has shape [n_features, n_features].
sample_covariance <- function(X) {
  n_samples <- nrow(X)

  # center the data matrix first
  X <- scale(X, center = TRUE, scale = FALSE)

  (1 / n_samples) * t(X) %*% X
}

## @knitr end-of-sample_covariance

## @knitr matrix_sqrt

#' Matrix Square Root
#'
#' Calculates the square root of a matrix by using its spectral decomposition.
#'
#' @param X A matrix
#' @param symmetric If \code{TRUE}, the matrix is assumed to be symmetric.
#'   Passed to \code{eigen}.
#' @return A matrix of the same shape as \code{X}.
matrix_sqrt <- function(X, symmetric = FALSE) {
  # Perform the spectral decomposition.
  # Covariance matrices are symmetric, so we should pass on this optimization.
  X.eig <- eigen(X, symmetric = symmetric)

  # extract the Q eigen-vector matrix and the eigen-values
  Q <- X.eig$vectors
  values <- X.eig$values
  Q_inv <- solve(Q) # Q^{-1}

  Q %*% diag(sqrt(values)) %*% Q_inv
}

## @knitr end-of-matrix_sqrt