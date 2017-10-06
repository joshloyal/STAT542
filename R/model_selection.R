## @knitr train_test_split

#' Create a train-test split of a dataset.
#'
#' For model evaluation it is useful to split a
#' dataset into a training and testing set. This
#' helper functions shuffles the dataset and then
#' splits of `test_size` percentage of the dataset for
#' testing.
train_test_split <- function(X, y = NULL, test_size = 0.2,
                             shuffle = TRUE, seed = 123) {
  if (is.vector(y)) {
    y <- as.matrix(y)
  }

  if (!is.null(y) && nrow(X) != nrow(y)) {
    stop("`X` and `y` must have the same number of rows.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

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
    X.train = X_train,
    X.test = X_test,
    y.train = y_train,
    y.test = y_test
  )
}

## @knitr end-of-train_test_split

#' Generate fold indices for K-Fold Cross-Validation
kfold_cv <- function(X, y = NULL, n_folds = 5, seed = 123) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_samples <- nrow(X)

  as.factor(sample(rep(1:n_folds, length.out = n_samples)))
}