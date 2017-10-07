## @knitr soft_threshold

#' Soft Thresholding
#'
#' Computes the soft thresholding function defined as
#' $\text{sign}(\beta)(|\beta| - \lambda)_{+}$.
#'
#' @param beta Value to apply the soft thresholding operation.
#' @param lambda Threshold constant.
#' @returns The value of beta after soft thresholding is applied.
soft_threshold <- function(beta, lambda) {
  sign(beta) * max(abs(beta) - lambda, 0)
}

## @knitr end-of-soft_threshold

## @knitr should_stop

#' Stopping criteria for cd solver
#'
#' Compares the change in the average loss a window of length
#' `window / 2` to determine if the average loss has changed more
#' than `tol`.
#'
#' @params loss vector of objective values
#' @params window full window length
#' @params tol stopping tolerance
#' @return boolean indicating whether to stop or not.
should_stop <- function(loss, window, k, tol = 1e-10) {
  window_length = floor(window / 2) - 1
  prev_loss <- mean(loss[(k-(window-1)):(k-(window-1)+window_length)])
  curr_loss <- mean(loss[(k-(window-1)+window_length):k])

  abs(curr_loss - prev_loss) < tol
}

## @knitr end-of-should_stop

## @knitr lasso_cd

#' Coordinate-Descent Solver for Lasso Regression
#'
#' Performs coordinate descent for a single lambda value.
#' Assumes the design matrix X is standardized and that
#' the target vector y is centered at mean zero.
#'
#' @param X predictors of shape [n_samples, n_features]
#' @param y target variable of shape [n_samples]
#' @param beta initial values for beta of shape [n_features]
#' @param lambda penalization parameter
#' @param tol stopping tolerance
#' @param max_iter maximum number of iteration to run the coordinate descent
#'  solver
#' @param patience number of iterations to wait before checking the stopping
#'   criteria.
lasso_cd <- function(x, y, lambda, beta = NULL, tol = 1e-10, max_iter = 500,
                     patience = 10) {
  # if the initial beta is null, then start at zero
  if(is.null(beta)) {
    beta = rep(0, ncol(x))
  }

  # value of the loss function (MSE) at each iteration
  loss <- rep(0, max_iter)

  for (k in 1:max_iter) {
    # the full residual used to check the value of the loss function
    residual <- y - x %*% beta
    loss[k] <- mean(residual * residual)

    for (j in 1:ncol(x)) {
      # partial residual (effect of all the other co-variates)
      residual <- residual + x[, j] * beta[j]

      # single variable OLS estimate
      beta_ols_j <- mean(residual * x[, j])

      # soft-threshold the result
      beta[j] <- soft_threshold(beta_ols_j, lambda)

      # restore the residual
      # (although adding back the effect of the updated beta)
      residual <- residual - x[, j] * beta[j]
    }
    # end-of coefficient loop

    if (k > patience) {
      # I changed the early stopping criterion to compare the mean loss change
      # across a window length of one half the patience (5 by default).
      # The one provided was not stopping early (even when converged),
      # so the algorithm was taking ages...
      if (should_stop(loss, k = k, window = patience, tol = tol)) {
        break
      }
    }
  }
  # end-of iteration loop

  beta
}

## @knitr end-of-lasso_cd

## @knitr lasso

#' Lasso Regression
#'
#' @param x,y The predictors and the target matrix
#' @param lambda A vector of lambda values to try.
#'   If not supplied an appropriate scale is determined from the data.
#' @param n_lambda If lambda is not supplied, this parameter indicates
#'   how many lambda values to include in the lasso path.
#' @param lambda_ratio The scale of the lasso path, i.e.
#'   lambda_min / lambda_max = 1e-4.
#' @param max_iter The maximum number of iterations to perform for
#'   each coordinate-descent algorithm.
#' @param tol Stopping tolerance.
#' @param patience The number of iterations to wait before checking
#'   the stopping criteria.
#' @returns A lasso object.
lasso <- function(x, y, lambda = NULL, n_lambda = 100, lambda_ratio = 1e-4,
                  max_iter = 500, tol = 1e-10, patience = 10) {
  # now we need to standardize our predictors
  # and scale the response.
  x <- scale(x)
  y <- scale(y, scale = FALSE)

  # if not supplied use a sequence of decreasing lambdas,
  # such that the largest lambda has only one non-zero coefficient.
  if (is.null(lambda)) {
    max_lambda <- (1 / nrow(x)) * max(abs(t(x) %*% y))
    lambda <- exp(seq(log10(max_lambda), log10(max_lambda * lambda_ratio),
                      length.out = n_lambda))
  }

  # the coefficients for each lambda value
  beta_all <- matrix(NA, nrow = ncol(x), ncol = length(lambda))
  rownames(beta_all) <- colnames(x)

  # the intercept for each lambda value
  beta0_all <- rep(NA, length(lambda))

  # initial beta value for the largest lambda
  bhat <- rep(0, ncol(x))

  for (i in 1:length(lambda)) # loop from the largest lambda value
  {
    # solve the lasso objective using coordinate-descent for a given lambda
    bhat <- lasso_cd(x, y, lambda[i], beta = bhat, tol = tol,
                     max_iter = max_iter, patience = patience)

    # to get the unscaled / uncentered version we need to apply the scale to
    # the coefficients
    # Note: x_new = x_old * center + scale => beta_new = beta_old / center.
    #       From SLR theory.
    x_scale <- attr(x, 'scaled:scale')
    beta_all[, i] <-  bhat / x_scale

    # We can always calculate the intercept from the other coefficients
    # Note: beta0 = ybar - sum(xbar_i * beta_i). From MLR theory.
    y_mean <- attr(y, 'scaled:center')
    x_mean <- attr(x, 'scaled:center')
    beta0_all[i] <- y_mean - sum(x_mean * beta_all[, i])
  }

  # create a lasso object (mostly to make my life easier)
  lasso_init(intercept=beta0_all, coef=beta_all, lambda=lambda)
}

## @knitr end-of-lasso

## @knitr lasso_object

#' Lasso model object
#'
#' A constructor for a lasso model object. Performs some sanity checks
#' on the arguments before creating the structure.
#'
#' @param intercept a vector of intercept values for each lambda
#' @param coef a matrix of coefficients [n_features, n_lambda]
#' @param lambda a vector of lambda value used in during the lasso path.
#'
#' @return a lasso model object.
lasso_init <- function(intercept, coef, lambda) {
  if (!is.numeric(intercept)) {
    stop('intercept must be numeric')
  }

  if (!is.matrix(coef)) {
    stop('coef must be a matrix')
  }

  if (!is.numeric(lambda)) {
    stop('lambda must be numeric')
  }

  if (length(lambda) != ncol(coef)) {
    stop('lambda must be the same length as the coef matrix')
  }

  structure(list(intercept=intercept, coef=coef, lambda=lambda),
            class = 'lasso')
}

#' Plots the lasso path for the coefficents
#'
#' The lasso algorithm is solved using pathwise block-coordinate descent,
#' such that a solution for sequence of lambdas starting from large to small
#' is determined. This plot shows the coefficient magnitudes as a function
#' of the available L1 norm (remember lambda has a one-to-one correspondents
#' with the bound sum(|beta|) < t).
#'
#' @param model a lasso model object
#' @param include.legend boolean indicating whether to include a variable
#'   legend.
#' @return a ggplot2 object
plot.lasso <- function(model, include.legend = FALSE) {
  data <- as.tibble(t(model$coef)) %>%
    # L1-norm for each lambda value
    mutate(l1_norm = colSums(abs(model$coef))) %>%
    # map each coefficient to its lambda path for visualization
    gather(key = name, value = value, -l1_norm)

  p <- ggplot(data, aes(x = l1_norm, y = value, color = name)) +
    geom_line() + geom_point() +
    xlab('L1 norm') +
    ylab('Coefficients')

  # Add a legend if necessary. The names of the variables are
  # stored in the rows of the coefficient vector.
  if (is.null(rownames(model$coef)) || !include.legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.text = element_text(size = 8),
                   legend.position = 'bottom')
  }

  p
}

#' Make predictions from a "lasso" object.
#'
#' Similar to toher predict methods, this function predicts fitted values.
#' The default returns a matrix of shape [n_samples, n_lambdas] containing
#' predictions for all lambda values. You can request single lambda value
#' predictions by setting \code{lamba.index}.
#'
#' @param model Fitted 'lasso' model object
#' @param x_new Matrix of new values for x at which predictions are made.
#' @param lambda.index Index indicating which lambda value to predict for. If
#'   NULL all lambda values are used and a matrix is returned.
#' @return A matrix if lambda.index is NULL. Otherwise a vector.
predict.lasso <- function(model, X_new, lambda.index = NULL) {
  # X_new %*% model$coef is matrix of size [n_samples, n_lambda],
  # which has the X * beta predictions for a given lambda on the columns.
  # We then sweep the lambda intercepts across the columns to get the full
  # predictions.
  #
  # a.k.a numpy style broadcasting for (X * beta + beta_0).
  preds <- sweep(X_new %*% model$coef, 2, model$intercept, FUN = "+")

  # if lambda.index is set, then return a vector
  if (!is.null(lambda.index)) {
    preds <- preds[, lambda.index]
  }

  preds
}

## @knitr end-of-lasso_object