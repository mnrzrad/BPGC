#' Calculate the Probability Density Function for the Bivariate Poisson-Gamma Distribution
#'
#' Computes the probability density function (PDF) for the bivariate Poisson-Gamma distribution given parameters and values of X and Y.
#'
#' @param x integer values (discrete variable)
#' @param y Numeric value (continuous variable)
#' @param params A numeric vector of parameters: \code{c(m10, m01, m11, m02, m12)}.
#' @return A numeric value or vector representing the PDF evaluated at the given \code{x} and \code{y}.
#' @details
#' This function calculates the PDF for the bivariate Poisson-Gamma distribution based on the provided parameters.
#' The PDF is given by:
#' \deqn{f(x, y; \mathbf{m}) = \frac{\exp(c + m10 \cdot x - m01 \cdot y - m11 \cdot x \cdot y + m02 \cdot \log(y) + m12 \cdot x \cdot \log(y))}{x! \cdot y}}
#' where \code{c} is a normalization constant calculated using \code{calC}.
#'
#' @examples
#' # Example usage:
#' params <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' x <- 5
#' y <- 2
#' dBPGC(x, y, params)
#'
#' @export
dBPGC <- function(x, y, params) {
  m10 <- params[1]
  m01 <- params[2]
  m11 <- params[3]
  m02 <- params[4]
  m12 <- params[5]

  c <- calC(params)

  exp(c + (m10 * x) - (m01 * y) - (m11 * x * y) + (m02 * log(y)) + (m12 * x * log(y))) / (factorial(x) * y)
}
