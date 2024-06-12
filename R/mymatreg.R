#' mymatreg
#'
#' A function which returns many great things from matrix algebra for regression
#'
#' @param obj an object from an `lm()`
#'
#' @return an invisible list to call including: summary, data frame, characteristic & response matrices, beta estimates, RSS, ssq, s, fitted values, and residuals.
#' @export
#'
#' @examples \dontrun{mymatreg(ylm)}
mymatreg <- function(obj) { # obj = lm() object

  # summary
  sm <- summary(obj)

  # data frame
  dat <- stats::model.frame(obj)

  # n and k
  n <- dim(dat)[1]
  k <- dim(dat)[2] - 1

  # matrices
  X <- stats::model.matrix(obj)
  Y <- dat[, 1]
  Y <- as.matrix(Y)

  # beta estimates
  betahat <- solve(t(X) %*% X) %*% t(X) %*% Y

  # RSS
  RSS <- t(Y) %*% Y - t(betahat) %*% t(X) %*% Y

  # ssq & s
  ssq <- RSS / (n - (k + 1))
  s <- sqrt(ssq)

  # fitted and residuals
  yhat <- X %*% betahat
  res <- Y - yhat

  # list
  invisible(list(summary=sm,
                 dat=dat,
                 X=X,
                 Y=Y,
                 betahat=betahat,
                 RSS=RSS,
                 ssq=ssq,
                 s=s,
                 fit=yhat,
                 resid=res))
}
