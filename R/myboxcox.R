#' myboxcox
#'
#' Returns the MLE for lambda
#'
#' @param x vector
#'
#' @return a value for the MLE and a plot
#' @export
#'
#' @importFrom MASS boxcox
#'
#' @examples \dontrun{myboxcox(x)}
myboxcox <- function(x) {
  x=x
  out <- boxcox(stats::lm(x ~ 1))
  ind <- which.max(out$y)
  out$x[ind]
}
