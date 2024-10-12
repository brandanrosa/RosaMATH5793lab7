#' statdist
#'
#' A function which tests for multivariate normality
#'
#' @param df a data frame
#' @param contour level of contour (default is 0.05)
#'
#' @return a Chi-Sq plot and a named list
#' @export
#'
#' @examples \dontrun{statdist(cars)}
statdist <- function(df, contour=0.05) { # df = data frame
  X <- as.matrix(df)
  n <- length(X[,1])
  p <- length(X[1,])
  mu <- as.matrix(colMeans(X))
  sigma <- as.matrix(stats::cov(X))

  dsqd <- function(X, df) {
    t(X - mu) %*% solve(sigma) %*% (X - mu)
  }

  v <- apply(df, 1, dsqd, df = df)
  ind <- which(v > stats::qchisq(1-contour, p)) # indices of condition
  out <- df[ind,] # unusual points

  # ChiSq Plot
  j <- 1:n
  dsq <- sort(v)
  qchi <- stats::qchisq((j-1/2)/n, p)
  plot(qchi, dsq, main = "ChiSq Plot",
       xlab = "ChiSq Quantiles",
       ylab = "Ordered Squared Dist",
       pch=21, bg='hotpink', cex=1.35)

  dff <- data.frame(dsq, qchi)

  # List
  list(Dist_qChisq = dff, PctOutContour=out)
}
