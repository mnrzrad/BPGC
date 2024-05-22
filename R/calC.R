#' Calculate the normalizing constant c
#'
#' This function calculates the normalizing constant c for given parameters.
#'
#' Calculate the normalizing constant c for the given parameters, based on the following distribution:
#' \deqn{f(x,y | \bm{\theta}) = \frac{1}{x! \, y} \exp\left(c + m_{10}x - m_{01}y - m_{11}xy + m_{02}\log y + m_{12}x\log y\right)}
#' where \eqn{x = 0, 1, \ldots } and \eqn{y > 0 }.
#'
#' @param params A numeric vector of parameters (m10, m01, m11, m02, m12)
#'
#' @return The normalizing constant c.
#'
#' @examples
#'
#' # params = m = (m10, m01, m11, m02, m12)
#' params <- c(1, 1, 0, 1, 0)
#'
#' c_value <- calC(params)
#' cat("Normalization constant c:", c_value)
#'
#' @importFrom stats integrate
#' @export
calC <- function(params) {
  m10 <- params[1]
  m01 <- params[2]
  m11 <- params[3]
  m02 <- params[4]
  m12 <- params[5]

  # Define the integrand function for normalization constant
  integrand_c <- function(y, x, m10, m01, m11, m02, m12) {
    tryCatch({
      exp(m10 * x - m01 * y - m11 * x * y + m02 * log(y) + m12 * x * log(y)) / (factorial(x) * y)
    }, warning = function(w) {
      0
    }, error = function(e) {
      0
    })
  }

  # Integrate the integrand for normalization constant
  integrate_for_x_c <- function(x, m10, m01, m11, m02, m12) {
    result <- tryCatch({
      stats::integrate(
        f = function(y) integrand_c(y, x, m10, m01, m11, m02, m12),
        lower = 0,
        upper = Inf,
        rel.tol = 1e-10,
        abs.tol = 1e-10
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

  if (is.finite(sum_result_c) && sum_result_c > 0) {
    c <- -log(sum_result_c)
  } else {
    c <- Inf
  }
  return(c)
}
