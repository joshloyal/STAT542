context('lasso')

library(glmnet)

test_that('lasso matches glment.', {
  set.seed(1)
  N = 400
  P = 20

  Beta = c(1:5/5, rep(0, P-5))
  Beta0 = 0.5

  # genrate X
  V = matrix(0.5, P, P)
  diag(V) = 1

  X = as.matrix(mvrnorm(N, mu = 3*runif(P)-1, Sigma = V))

  # create artifical scale of X
  X = sweep(X, 2, 1:10/5, "*")

  # genrate Y
  y = Beta0 + X %*% Beta + rnorm(N)

  my_beta <- lasso_cd(scale(X), scale(y, scale = FALSE), lambda = 0.2, tol = 1e-7, max_iter = 500)
  fit <- lasso_fit(X, y)
  print(fit$coef)
  #print(Beta)
  #print(glmnet(X, y)$beta)


})
