## @knitr euclidean_distance

#' Euclidean Distance
#'
#' Calculates the euclidean distance between two vectors
#' \code{x1} and \code{x2}.
#'
#' @param x1,x2 numeric vectors.
#' @return the euclidean distance between the inputs.
mydist <- function(x1, x2) {
  # sanity check that x1 and x2 have the same number of features.
  if (length(x1) != length(x2))
    stop("`x1` and `x2` must be the same length.")

  sqrt(sum((x1 - x2)^2))
}

## @knitr end-of-euclidean_distance

## @knitr mahalanobis_distance

#' Mahalanobis Distance
#'
#' Calculates teh mahalanobis distance between two vectors
#' \code{x1} and \code{x2} using a arbitrary covariance
#' matrix \code{s}.
#'
#' @param x1,x2 numeric vectors.
#' @param s covariance matrix.
#' @return the euclidean distance between the inputs.
mydist2 <- function(x1, x2, s) {
  if (length(x1) != length(x2))
    stop('`x1` and `x2` must have the same length.')
  if (nrow(s) != length(x1) || ncol(s) != length(x2))
    stop('`s` must be square and nrow = ncol = lengh(xi)')

  diff <- matrix(x1 - x2, nrow = 1)
  s_inv <- solve(s)

  as.numeric(sqrt(diff %*% s_inv %*% t(diff)))
}

## @knitr end-of-mahalanobis_distance

## @knitr nearest_neighbors

#' Nearest Neighbors
#'
#' Find the k nearest neighbors from a point \code{x_query}
#' and all rows in a dataset \code{X}.
#'
#' @param X Feature matrix of shape [n_samples, n_features].
#' @param x_query A single vector of shape [n_features].
#' @param k The number of nearest neighbors to compute.
#' @param dist_func A function that defines a valid distance.
#'   The default is euclidean distance.
#' @param ... These parameters are passed into \code{dist_func}.
#' @return The indices of the k-nearest neighbors.
nearest_neighbors <- function(X, x_query, k, dist_func = mydist, ...) {
  # If k > the number of points they are
  # all nearest-neighbors
  if (k >= nrow(X))
    return(1:nrow(X))

  # bind distance function to the y vector
  partial_dist <- function(x) {
    dist_func(x, x_query, ...)
  }

  # row-wise distances
  distances <- apply(X, 1, partial_dist)

  # kth largest values
  max_value <- sort(distances, partial = k)[k]

  # indices that are less than or equal to the kth smallest value
  # are the nearest-neighbors
  which(distances <= max_value)[1:k]
}

## @knitr end-of-nearest_neighbors

## @knitr predict_knn_single

#' K-Nearest Neighbor Predictions for a Single Data-Point
#'
#' Make predictions on a data point \code{x_query}
#' based on the k-nearest neighbors in the dataset
#' \code{X}. Predictions are made by averaging the
#' \code{Y} values of the k-neighest neighbors.
#'
#' @param X dataset of shape [n_samples, n_features]
#' @param Y target vector of shape [n_samples]
#' @param x_query vector of shape [n_features] to make predictions on.
#' @param k number of nearest neighbors to use for predictions.
#' @param dist_func function used to calculate the distance between two points.
#' @param ... These arguments are passed into \code{dist_func}.
#' @return A single prediction for \code{x_query}.
predict_knn_single <- function(X, Y, x_query, k, dist_func = mydist, ...) {
  neighbors <- nearest_neighbors(X, x_query = x_query, k = k, dist_func = dist_func, ...)
  mean(Y[neighbors])
}

## @knitr end-of-predict_knn_single

## @knitr predict_knn

predict_knn <- function(X, Y, X_query, k, dist_func = mydist, ...) {
  knn.predict <- function(x_query) {
    predict_knn_single(X = X, Y = Y, x_query = x_query, k = k, dist_func = dist_func, ...)
  }
  apply(X_query, 1, knn.predict)
}

## @knitr end-of-predict_knn