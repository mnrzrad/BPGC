#' Empirical Plot with Bivariate Poisson-Gamma Fit
#'
#' Generates a 3D empirical plot of the data along with the estimated bivariate Poisson-Gamma distribution.
#'
#' @param X Numeric vector representing the Poisson-distributed variable.
#' @param Y Numeric vector representing the Gamma-distributed variable.
#' @details
#' This function creates a 3D empirical plot using the given data points for the Poisson and Gamma-distributed variables.
#' It also fits a bivariate Poisson-Gamma distribution to the data and superimposes the estimated density on the empirical plot.
#' @importFrom plot3D persp3D
#' @examples
#' # Example usage:
#' params <- c(5,5,5,5,5)
#' sim_data <- rBPGC(params, points = 1e5, seed = 42)
#' X <- sim_data$x
#' Y <- sim_data$y
#' #X <- rpois(1000,exp(1))
#' #Y <- rgamma(1000, 5, 5)
#' ePLOT(X, Y, params)
#'
#' @export
ePLOT <- function(X, Y, params) {
  # Remove NA values
  valid_indices <- complete.cases(X, Y)
  X <- X[valid_indices]
  Y <- Y[valid_indices]

  uniqueX <- sort(unique(X))

  # Number of bins for the histograms
  num_bins <- 25

  # Bin Y into intervals
  y_bins <- cut(Y, breaks = num_bins)


  hist_probs <- table(X, y_bins)/sum(table(X, y_bins))
  hist_probs <- as.matrix(hist_probs)
  #
  # hist_counts <- table(X, y_bins)


  # Create x and y meshgrid for ribbon3D plot
  x <- as.vector(uniqueX)
  y <- seq(min(Y), max(Y), length.out = num_bins)

  # Fit the bivariate Poisson-Gamma distribution to the data
  # mle_result <- mleEst(X, Y, params_init = rep(0.5, 5))
  # m <- mle_result$params

  zz <- matrix(0, nrow = length(x), ncol = length(y))
  # Compute the values for the function
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      zz[i, j] <- dBPGC(x[i], y[j], params)
    }
  }

  zz <- zz / sum(zz) * sum(hist_probs)

  # max_hist_probs <- max(hist_probs)
  # max_zz <- max(zz)
  # combined_max_z <- max(max_hist_probs, max_zz)
  #
  # xL <- range(c(min(x), max(x), min(hist_probs), max(hist_probs)))
  # yL <- range(c(min(y), max(y), min(hist_probs), max(hist_probs)))
  #
  #
  # zL <- c(0, combined_max_z)

  col_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))
  col <- col_pal(num_bins)

  plot3D::hist3D(x = x, y = y, z = hist_probs,
                 phi = 10, theta = 120,
                 col = 'grey', NAcol = "white", border = 'black',
                 xlab = "X", ylab = "Y", zlab = "Probability",
                 zlim = c(0, max(hist_probs, na.rm = TRUE, zz)),
                 add = FALSE, plot = TRUE, ticktype = 'detailed', along = 'y',
                 space = c(0.5, 0), curtain = TRUE)


  for (i in 1:length(x)) {
    plot3D::lines3D(x = rep(x[i], length(y)), y = y, z = zz[i,], add = TRUE, col = 'red', lwd = 2)
  }

  title("Empirical Plot with Bivariate Poisson-Gamma Fit")
}



