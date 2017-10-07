## @knitr mean_squared_error

#' Mean Squared Error
mean_squared_error <- function(y_true, y_pred) {
  y_true <- as.numeric(y_true)
  y_pred <- as.numeric(y_pred)

  mean( (y_true - y_pred)^2 )
}

## @knitr end-of-mean_squared_error

## @knitr accuracy

#' Accuracy
accuracy <- function(y_true, y_pred) {
  mean(y_true == y_pred)
}

## @knitr end-of-accuracy

## @knitr log_loss

#' Binary Log-Loss
clip_values <- function(x, x_min = -Inf, x_max = Inf) {
  ifelse(x <= x_min, x_min,
         ifelse(x >= x_max, x_max, x))
}

log_loss <- function(y_true, y_pred, eps = 1e-15) {
  y_pred <- clip_values(y_pred, eps, 1 - eps)

  # re-normalize after clipping
  y_pred <- y_pred / sum(y_pred)

  - (1 / nrow(y_true)) * sum(y_true * log(y_pred))
}

## @knitr end-of-log_loss