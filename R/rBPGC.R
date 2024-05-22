#' Simulate Data Using Gibbs Sampling
#'
#' Generates a dataset by simulating values of X and Y using Gibbs sampling.
#'
#' @param params A numeric vector of parameters: \code{c(m10, m01, m11, m02, m12)}.
#' @param points The number of data points to generate. Default is 500.
#' @param seed An optional integer seed for reproducibility. Default is \code{NULL}.
#' @return A data frame with simulated values of X and Y.
#' @details
#' This function generates a dataset using Gibbs sampling based on the provided parameters.
#' It starts with initial values and iteratively samples values of X and Y from their respective
#' conditional distributions until the specified number of points is generated.
#'
#' @importFrom stats rgamma rpois
#' @examples
#' # Example usage:
#' params <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' sim_data <- rBPGC(params, points = 100, seed = 42)
#' head(sim_data)
#'
#' @export
rBPGC <- function(params, points = 500, seed = NULL) {
  m10 <- params[1]
  m01 <- params[2]
  m11 <- params[3]
  m02 <- params[4]
  m12 <- params[5]

  points <- points + 50

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Initialize data storage
  x <- numeric(points)
  y <- numeric(points)

  # Initialize starting values
  y <- 0.5

  x[1] <- floor(exp(m10 - m11 * y + m12 * log(y)))  # Starting value for x

  y[1] <- (m02 + m12 * x[1]) * (m01 + m11 * x[1])  # Starting value for y

  # Perform Gibbs sampling
  for (i in 2:points) {
    # Sample X given current Y
    lambda <- exp(m10 - m11 * y[i-1] + m12 * log(y[i-1]))
    x[i] <- rpois(1, lambda)

    # Sample Y given current X
    shape <- m02 + m12 * x[i]
    rate <- m01 + m11 * x[i]

    y[i] <- rgamma(1, shape, rate)
  }

  # Return the data frame
  data_df <- as.data.frame(cbind(x, y))

  return(data_df[51:points, ])
}

