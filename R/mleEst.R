#' Maximum Likelihood Estimation (MLE) Function
#'
#' Estimates the parameters that maximize the likelihood of the data.
#'
#' @param X A numeric vector of observations for the predictor variable.
#' @param Y A numeric vector of observations for the response variable.
#' @param params_init Initial values for the parameters.
#' @param max_iterations Maximum number of iterations for optimization.
#' @param rel_tol Relative tolerance for convergence.
#' @return A list with the following components:
#' \describe{
#'   \item{params}{Optimized parameters.}
#'   \item{negative_log_likelihood}{Value of the negative log-likelihood at the optimum.}
#'   \item{convergence}{Convergence code: 0 for successful convergence, non-zero otherwise.}
#'   \item{message}{Description of the optimization result.}
#' }
#' @details
#' This function performs Maximum Likelihood Estimation (MLE) using constrained optimization
#' to find the parameters that maximize the likelihood of the data given the model. It uses
#' the \code{constrOptim} function for optimization, which requires setting constraints on
#' the parameters.
#'
#' The negative log-likelihood function used for optimization is internally defined within
#' this function. It calculates the negative log-likelihood based on given parameters and
#' observed data.
#'
#' @importFrom stats constrOptim
#' @examples
#' # Example usage:
#' params <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' sim_data <- rBPGC(params, points = 100, seed = 42)
#' X <- sim_data$x
#' Y <- sim_data$y
#' mleEst(X, Y)
#'
#' @export
mleEst <- function(X, Y, params_init = rep(0.5,5), max_iterations = 1e3, rel_tol = 1e-8) {

  negative_log_likelihood <- function(params, X, Y) {
    m10 <- params[1]
    m01 <- params[2]
    m11 <- params[3]
    m02 <- params[4]
    m12 <- params[5]

    # Calculate the normalizing constant c
    c <- calC(params)

    if (is.infinite(c)) {
      return(Inf)
    }

    # Calculate the negative log-likelihood
    result <- sum(log(factorial(X) * Y)) - length(X) * c - m10 * sum(X) + m11 * sum(X * Y) + m01 * sum(Y) - m02 * sum(log(Y)) - m12 * sum(X * log(Y))

    return(result)
  }

  # Constraints matrix
  ui <- rbind(
    c(0, 1, 0, 0, 0),   # m01 > 0
    c(0, 0, 0, 1, 0),   # m02 > 0
    c(1, 0, 0, 0, 0),   # m10 > 0
    c(0, 0, 1, 0, 0),   # m11 >= 0
    c(0, 0, 0, 0, 1)    # m12 >= 0
  )

  # Constraints limits
  ci <- c(0, 0, 0, 0, 0)

  # Control parameters for optimization
  control <- list(maxit = max_iterations, reltol = rel_tol)

  # Perform constrained optimization
  opt_result <- tryCatch({
    stats::constrOptim(theta = params_init,
                f = negative_log_likelihood,
                grad = NULL,
                ui = ui,
                ci = ci,
                control = control,
                X = X,
                Y = Y)
  }, error = function(e) {
    list(par = NA, value = Inf, convergence = 1, message = as.character(e))
  })

  # Return optimized parameters and other information
  return(list(
    params = opt_result$par,
    negative_log_likelihood = opt_result$value,
    convergence = opt_result$convergence,
    message = opt_result$message
  ))
}
