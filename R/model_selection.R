#' Create a train-test split of a dataset.
train_test_split <- function(X, y = NULL, test_size = 0.2, shuffle = TRUE, seed = 123) {
  if (is.vector(y)) {
    y <- as.matrix(y)
  }

  if (nrow(X) != nrow(y)) {
    stop("`X` and `y` must have the same number of rows.")
  }

  set.seed(seed)

  n_samples <- nrow(X)
  n_test <- floor(n_samples * test_size)
  n_train <- n_samples - n_test

  if (shuffle) {
    indices <- sample(n_samples)
  } else {
    indices = 1:n_samples
  }

  X_train <- X[1:n_train, ]
  X_test <- X[(n_train + 1):n_samples, ]

  if (!is.null(y)) {
    y_train <-y[1:n_train]
    y_test <- y[(n_train + 1):n_samples]
  } else {
    y_train <- NULL
    y_test <- NULL
  }

  list(
    X_train = X_train,
    X_test = X_test,
    y_train = y_train,
    y_test = y_test
  )
}