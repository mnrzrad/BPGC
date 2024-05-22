#' Calculate the Cumulative Distribution Function for the Bivariate Poisson-Gamma Distribution
#'
#' Computes the cumulative distribution function (CDF) for the bivariate Poisson-Gamma distribution given parameters and values of X and Y.
#'
#' @param x Numeric value or vector for the Poisson-distributed variable up to which to calculate the CDF.
#' @param y Numeric value or vector for the Gamma-distributed variable up to which to calculate the CDF.
#' @param params A numeric vector of parameters: \code{c(m10, m01, m11, m02, m12)}.
#' @return A numeric value representing the CDF evaluated at the given \code{x} and \code{y}.
#' @details
#' This function calculates the CDF for the bivariate Poisson-Gamma distribution based on the provided parameters by integrating the PDF.
#'
#' @examples
#' # Example usage:
#' params <- c(1,5,1,5,1)
#' x <- 2
#' y <- 0.3
#' pBPGC(x, y, params)
#'
#' @export
pBPGC <- function(X, Y, params) {
  m10 <- params[1]
  m01 <- params[2]
  m11 <- params[3]
  m02 <- params[4]
  m12 <- params[5]

  c <- calC(params)

  # Define the normalized function
  f_normalized <- function(x, y, m10, m01, m11, m02, m12, c) {
    exp(c + m10 * x - m01 * y - m11 * x * y + m02 * log(y) + m12 * x * log(y)) / (factorial(x) * y)
  }

  # Integrate the normalized function over y for each x
  integrand_normalized <- function(y, x, m10, m01, m11, m02, m12, c) {
    f_normalized(x, y, m10, m01, m11, m02, m12, c)
  }

  integrate_for_x_normalized <- function(x, m10, m01, m11, m02, m12, c) {
    result <- tryCatch({
      integrate(
        f = function(y) integrand_normalized(y, x, m10, m01, m11, m02, m12, c),
        lower = 0,
        upper = Y,
        subdivisions = 1000,
        rel.tol = 1e-8
      )$value
    }, error = function(e) {
      NA
    })
    return(result)
  }

  # Check normalization
  sum_result_normalized <- 0

  for (x in 0:X) {
    integral_value <- integrate_for_x_normalized(x, m10, m01, m11, m02, m12, c)
    if (is.finite(integral_value) && integral_value > 0) {
      sum_result_normalized <- sum_result_normalized + integral_value
    } else {
      break
    }
  }
  return(sum_result_normalized)
}
