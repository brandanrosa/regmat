mymatreg <- function(obj) { # obj = lm() object

  # summary
  sm <- summary(obj)

  # data frame
  dat <- model.frame(obj)

  # n and k
  n <- dim(dat)[1]
  k <- dim(dat)[2] - 1

  # matrices
  X <- model.matrix(obj)
  Y <- dat[, 1]
  Y <- as.matrix(Y)

  # beta estimates
  betahat <- solve(t(X) %*% X) %*% t(X) %*% Y

  # RSS
  RSS <- t(Y) %*% Y - t(betahat) %*% t(X) %*% Y

  # ssq & s
  ssq <- RSS / (n - (k + 1))
  s <- sqrt(ssq)

}
