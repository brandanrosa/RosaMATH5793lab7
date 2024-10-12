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
  ylm <- stats::lm(x~1)
  out <- boxcox(ylm)
  ind <- which.max(out$y)
  out$x[ind]
}
