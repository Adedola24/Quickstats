#' Confidence Interval
#'
#' Computes a confidence interval for a numeric vector.
#'
#' @param x A numeric vector.
#' @return A confidence interval range.
#' @importFrom stats qt
#' @importFrom stats na.omit
#' @export
confidence_interval <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  se <- sd(x) / sqrt(n)
  alpha <- 0.05
  t_value <- qt(1 - alpha / 2, df = n - 1)
  margin <- t_value * se
  return(c(lower = mean(x) - margin, upper = mean(x) + margin))
}
