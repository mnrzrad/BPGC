#' Empirical Plot with Bivariate Poisson-Gamma Fit
#'
#' Generates a 3D empirical plot of the data along with the estimated bivariate Poisson-Gamma distribution.
#'
#' @param X Numeric vector representing the Poisson-distributed variable.
#' @param Y Numeric vector representing the Gamma-distributed variable.
#' @details
#' This function creates a 3D empirical plot using the given data points for the Poisson and Gamma-distributed variables.
#' It also fits a bivariate Poisson-Gamma distribution to the data and superimposes the estimated density on the empirical plot.
#'
#' @examples
#' if (!requireNamespace("plot3D", quietly = TRUE)) {
#' install.packages("plot3D")
#' }
#' library(plot3D)
#' # Example usage:
#' params <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' sim_data <- rBPGC(params, points = 100, seed = 42)
#' X <- sim_data$x
#' Y <- sim_data$y
#' ePLOT(X, Y)
#'
#' @export
ePLOT <- function(X, Y) {
  # Remove NA values
  valid_indices <- complete.cases(X, Y)
  X <- X[valid_indices]
  Y <- Y[valid_indices]

  unique_X <- sort(unique(X))

  # Number of bins for the histograms
  num_bins <- 10

  # Create a list to store histogram data
  hist_data_list <- vector("list", length = length(unique_X))

  # Compute histogram counts for each unique X value
  for (i in 1:length(unique_X)) {
    data_subset <- Y[X == unique_X[i]]
    hist_data <- hist(data_subset, breaks = num_bins, plot = FALSE)
    hist_data_list[[i]] <- hist_data$counts
  }

  # Convert list to a matrix
  hist_counts <- sapply(hist_data_list, function(x) {
    if (length(x) < num_bins) {
      c(x, rep(0, num_bins - length(x)))
    } else {
      x[1:num_bins]
    }
  })

  hist_counts <- t(hist_counts)

  # Create x and y meshgrid for ribbon3D plot
  x <- as.vector(unique_X)
  y <- seq(min(Y), max(Y), length.out = num_bins)

  # Fit the bivariate Poisson-Gamma distribution to the data
  mle_result <- mle(X, Y, params_init = rep(2, 5))
  m <- mle_result$params

  # Create an empty matrix to hold the z values
  z1 <- matrix(0, nrow = length(y), ncol = length(x))

  # Compute the values for the function
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z1[j, i] <- dBPGC(x[i], y[j], m)
    }
  }

  zl <- c(0, max(z1))

  col_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))
  col <- col_pal(num_bins)

  # Transpose the z matrix to match x and y lengths
  z1 <- t(z1)

  # Create the 3D plot using hist3D
  plot3D::hist3D(x = x, y = y, z = hist_counts,
                 phi = 20, theta = 120,
                 col = col, NAcol = "white", border = 'black',
                 xlab = "X", ylab = "Y", zlab = "Frequency",
                 add = FALSE, plot = TRUE, ticktype = 'detailed', along = 'y',
                 space = c(0.5, 0), curtain = TRUE)

  # Plot the fitted bivariate Poisson-Gamma distribution
  plot3D::persp3D(x, y, z = z1, theta = 120, zlim = zl,
                  box = TRUE,
                  shade = 0,
                  col = "transparent", border = NA,
                  xlab = 'X', ylab = 'Y', zlab = "f(x,y)", ticktype = 'detailed',
                  add = TRUE)

  # Add the curves for each fixed x without connecting lines between them
  for (i in 1:length(x)) {
    plot3D::lines3D(x = rep(x[i], length(y)), y = y, z = z1[, i], add = TRUE, col = 'red', lwd = 3)
  }

  title("Empirical Plot with Bivariate Poisson-Gamma Fit")
}
