#' Check Distribution Normalization
#'
#' Verifies if the provided parameters normalize the distribution correctly.
#'
#' @param params A numeric vector of parameters: \code{c(m10, m01, m11, m02, m12)}.
#' @return Prints the normalization constant and a message indicating whether the function is normalized correctly.
#' @details
#' This function calculates the normalization constant for a given set of parameters by integrating the probability density function.
#' It then checks if the sum of the integrals over the specified range is close to 1, indicating proper normalization.
#'
#' @examples
#' # Example usage:
#' params <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' checkDist(params)
#'
#' @export
checkDist <- function(params){
  m10 <- params[1]
  m01 <- params[2]
  m11 <- params[3]
  m02 <- params[4]
  m12 <- params[5]

  # Define the integrand function for normalization constant
  integrand_c <- function(y, x, m10, m01, m11, m02, m12) {
    exp(m10 * x - m01 * y - m11 * x * y + m02 * log(y) + m12 * x * log(y)) / (factorial(x) * y)
  }

  # Integrate the integrand for normalization constant
  integrate_for_x_c <- function(x, m10, m01, m11, m02, m12) {
    result <- tryCatch({
      integrate(
        f = function(y) integrand_c(y, x, m10, m01, m11, m02, m12),
        lower = 0,
        upper = Inf,
        subdivisions = 1000,
        rel.tol = 1e-8
      )$value
    }, error = function(e) {
      NA
    })
    return(result)
  }

  # Calculate the sum for normalization constant
  x_max <- 100
  sum_result_c <- 0

  for (x in 0:x_max) {
    integral_value <- integrate_for_x_c(x, m10, m01, m11, m02, m12)
    if (is.finite(integral_value) && integral_value > 0) {
      sum_result_c <- sum_result_c + integral_value
    } else {
      break
    }
  }

  c <- -log(sum_result_c)

  message(paste("Normalization constant c:", c))

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
        upper = Inf,
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

  for (x in 0:x_max) {
    integral_value <- integrate_for_x_normalized(x, m10, m01, m11, m02, m12, c)
    if (is.finite(integral_value) && integral_value > 0) {
      sum_result_normalized <- sum_result_normalized + integral_value
    } else {
      break
    }
  }

  # Print the result
  message("Sum of integrals:", sum_result_normalized)

  # Check if the sum is close to 1
  if (abs(sum_result_normalized - 1) < 1e-6) {
    message("The function is normalized correctly.")
  } else {
    message("The function is NOT normalized correctly.")
  }
}
