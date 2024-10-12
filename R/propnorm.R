#' propnorm
#'
#' A function which tests normality for each variable and the data as multivariate
#'
#' @param df data frame
#' @param varfact list of variable names that are factors.
#' @param ... passes additional arguments to the function
#'
#' @return a plot and a named list
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom ggplot2 aes facet_wrap geom_histogram ggplot
#' @importFrom dplyr select_if mutate
#'
#' @examples \dontrun{propnorm(t46, facts)}
propnorm <- function(df, varfact, ...) {
  Both <- c()
  Test1 <- c()
  Test2 <- c()
  d1 <- c()
  d2 <- c()
  value <- c()

  # Define n and p
  n <- length(df[,1])
  p <- length(df[1,])

  # Convert to Factors
  ind <- which(colnames(df) %in% varfact)

  for (i in 1:length(ind)) {
    df[,ind[i]] <- as.factor(df[,ind[i]])
  }

  # Separate Numeric vs Factor
  dfNum <- df %>%
    select_if(is.integer)
  dfFac <- df %>%
    select_if(is.factor)

  # tester points
  t1 <- 1.396/sqrt(n)
  t2 <- 0.628/sqrt(n)

  # test
  f1 <- function(x) {
    m <- mean(x); s <- stats::sd(x)
    pi1 <- mean((x > m - s) & (x < m + s))
    pi1
  }

  f2 <- function(x) {
    m <- mean(x); s <- stats::sd(x)
    pi2 <- mean((x > m - 2*s) & (x < m + 2*s))
    pi2
  }

  v1 <- c()
  for (i in 1:5) {
    v1[i] <- f1(df[,i])
  }

  v2 <- c()
  for (j in 1:5) {
    v2[j] <- f2(df[,j])
  }

  pij <- data.frame(pi1 = v1, pi2 = v2)
  pijm <- as.matrix(pij)
  dfnew <- pij %>%
    mutate(d1 = abs(v1 - 0.683),
           d2 = abs(v2 - 0.954),
           Test1 = ifelse(d1 > t1, "NonNorm", "Norm"),
           Test2 = ifelse(d2 > t2, "NonNorm", "Norm"),
           Both = ifelse(Test1 == "NonNorm" & Test2 == "NonNorm", "Both",
                         ifelse(Test1 == "NonNorm" | Test2 == "NonNorm", "NonNorm", "Norm")))

  TestMat <- dfnew %>%
    dplyr::select(Test1, Test2, Both)
  TestMat <- as.matrix(TestMat)

  MultiVarConc <- ifelse(TestMat[,3] == "NonNorm", "MultiNonNorm", "MultiNorm")

  # Plot
  g <- ggplot(gather(dfNum), aes(value)) +
    geom_histogram(bins = 10, fill = 'darkgreen') +
    facet_wrap(~key)
  print(g)

  # list
  list(pij=pijm, TestMat=TestMat, MultiVarConc=MultiVarConc)
}
