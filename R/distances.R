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