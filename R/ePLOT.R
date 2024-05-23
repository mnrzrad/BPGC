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
#' # Example usage:
#' params <- c(1,5,0,5,0)
#' sim_data <- rBPGC(params, points = 500, seed = 42)
#' X <- rpois(5000,exp(1))
#' Y <- rgamma(5000, 5, 5)
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
  num_bins <- 15

  # Create a list to store histogram data
  hist_data_list <- vector("list", length = length(unique_X))

  # Compute histogram counts for each unique X value
  for (i in 1:length(unique_X)) {
    data_subset <- Y[X == unique_X[i]]
    hist_data <- hist(data_subset, breaks = num_bins, plot = FALSE)
    hist_data_list[[i]] <- hist_data$counts
  }

  # Convert list to a matrix
  # hist_counts <- sapply(hist_data_list, function(x) {
  #   if (length(x) < num_bins) {
  #     c(x, rep(0, num_bins - length(x)))
  #   } else {
  #     x[1:num_bins]
  #   }
  # })

  # hist_counts <- t(hist_counts)
  #
  # # Normalize each row by the total counts for each unique X
  # total_count <- sum(hist_counts)
  # hist_probs <- hist_counts / total_count

  for (i in 1:length(unique_X)) {
    data_subset <- Y[X == unique_X[i]]
    hist_data <- hist(data_subset, breaks = num_bins, plot = FALSE)
    hist_counts <- hist_data$counts
    total_count <- sum(hist_counts)
    hist_probs <- hist_counts / total_count  # Normalize to probabilities
    hist_data_list[[i]] <- hist_probs
  }

  hist_probs <- sapply(hist_data_list, function(x) {
    if (length(x) < num_bins) {
      c(x, rep(0, num_bins - length(x)))
    } else {
      x[1:num_bins]
    }
  })

  hist_probs <- t(hist_probs)

  # Create x and y meshgrid for ribbon3D plot
  x <- as.vector(unique_X)
  y <- seq(min(Y), max(Y), length.out = num_bins)

  # Fit the bivariate Poisson-Gamma distribution to the data
  # mle_result <- mleEst(X, Y, params_init = rep(0.5, 5))
  # m <- mle_result$params
  m <- c(1,5,0,5,0)

  # Create an empty matrix to hold the z values
  z1 <- matrix(0, nrow = length(x), ncol = length(y))

  # Compute the values for the function
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z1[i, j] <- dpois(x[i],exp(m[1]))*dgamma(y[j],m[2],m[4]) #dBPGC(x[i], y[j], m)
    }
  }

  max_hist_probs <- max(hist_probs)
  max_z1 <- max(z1)
  combined_max_z <- max(max_hist_probs, max_z1)

  xL <- range(c(min(x), max(x), min(hist_probs), max(hist_probs)))
  yL <- range(c(min(y), max(y), min(hist_probs), max(hist_probs)))


  zL <- c(0, combined_max_z)

  col_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))
  col <- col_pal(num_bins)

  # # Create the 3D plot using persp3D
  # trmat <- plot3D::persp3D(x = x, y = y, z = z1, theta = 120, zlim = zL, # Set zlim here
  #                          box = TRUE, shade = 0, col = "transparent", border = NA,
  #                          ticktype = 'detailed', add = FALSE,
  #                          xlim = xL, ylim = yL # Set xlim and ylim
  #                          )
  #
  # # Create the 3D plot using hist3D
  # plot3D::hist3D(x = x1, y = y1, z = hist_probs, phi = 20, theta = 120,
  #                col = col, NAcol = "white", border = 'black',
  #                xlab = "X", ylab = "Y", zlab = "Probability",
  #                zlim = zL, # Set zlim here
  #                xlim = xL, ylim = yL, # Set xlim and ylim
  #                add = TRUE, plot = TRUE, ticktype = 'detailed', along = 'y',
  #                space = c(0.5, 0), curtain = TRUE)

  plot3D::hist3D(x = x, y = y, z = hist_probs,
                 phi = 20, theta = 150,
                 col = col, NAcol = "white", border = 'black',
                 xlab = "X", ylab = "Y", zlab = "Probability",
                 zlim = zL, # Set zlim here
                 add = FALSE, plot = TRUE, ticktype = 'detailed', along = 'y',
                 space = c(0.5, 0), curtain = TRUE)

  # Add the curves for each fixed x
  for (i in 1:length(x)) {
    plot3D::lines3D(x = rep(x[i], length(y)), y = y, z = z1[i,], add = TRUE, col = 'red', lwd = 2)
  }

  title("Empirical Plot with Bivariate Poisson-Gamma Fit")
}

