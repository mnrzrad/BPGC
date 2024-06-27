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
#' params <- c(5,5,1,5,1)
#' sim_data <- rBPGC(params, points = 1e4, seed = 42)
#' X <- sim_data$x
#' Y <- sim_data$y
#' X <- rpois(1000,exp(1))
#' Y <- rgamma(1000, 5, 5)
#' ePLOT(X, Y, params, title = "m=(5,5,1,5,1)")
#'
#' @export
ePLOT <- function(X, Y, params, title = NULL) {
  # Remove NA values
  valid_indices <- complete.cases(X, Y)
  X <- X[valid_indices]
  Y <- Y[valid_indices]

  uniqueX <- sort(unique(X))

  # Number of bins for the histograms
  num_bins <- 20

  # Bin Y into intervals
  y_bins <- cut(Y, breaks = num_bins)

  interval_mean <- function(interval) {
    # Remove the parentheses/brackets and split the string
    cleaned_interval <- gsub("[\\(\\)]", "", interval)
    cleaned_interval <- gsub("\\]", "", cleaned_interval)
    bounds <- strsplit(cleaned_interval, ",")[[1]]
    # Trim any extra spaces
    bounds <- trimws(bounds)
    # Convert to numeric and calculate the mean, handling potential NA values
    num_bounds <- as.numeric(bounds)
    if (any(is.na(num_bounds))) {
      warning(paste("Could not convert bounds to numeric for interval:", interval))
      return(NA)
    }
    mean(num_bounds)
  }
  intervals <- levels(y_bins)
  interval_means <- sapply(intervals, interval_mean)
  levels(y_bins) <- interval_means
  cont_table <- table(X, y_bins); print(chisq.test(cont_table))

  hist_probs <- table(X, y_bins)/sum(table(X, y_bins))
  hist_probs <- as.matrix(hist_probs)
  #
  # hist_counts <- table(X, y_bins)


  # Create x and y meshgrid for ribbon3D plot
  x <- as.vector(uniqueX)
  y <- seq(min(Y), max(Y), length.out = num_bins)

  # Fit the bivariate Poisson-Gamma distribution to the data
  mle_result <- mleEst(X, Y, params_init = rep(0.5, 5))$params
  # m <- mle_result$params

  zz <- matrix(0, nrow = length(x), ncol = length(y))
  zz1 <- matrix(0, nrow = length(x), ncol = length(y))
  # Compute the values for the function
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      zz[i, j] <- dBPGC(x[i], y[j], params)
      zz1[i,j] <- dBPGC(x[i], y[j],  mle_result)
    }
  }

  zz <- zz / sum(zz) * sum(hist_probs)
  zz1 <- zz1 / sum(zz1) * sum(hist_probs)

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
                 xlab = "X", ylab = "Y", zlab = " ",
                 zlim = c(0, max(hist_probs, na.rm = TRUE, zz)),
                 add = FALSE, plot = TRUE, ticktype = 'detailed', along = 'y',
                 space = c(0.5, 0), curtain = TRUE)


  for (i in 1:length(x)) {
    plot3D::lines3D(x = rep(x[i], length(y)), y = y, z = zz[i,], add = TRUE, col = 'red', lwd = 2)
  }

  # Add the lines for zz1
  for (i in 1:length(x)) {
    plot3D::lines3D(x = rep(x[i], length(y)), y = y, z = zz1[i,], add = TRUE, col = 'blue', lwd = 2)
  }

  # plot3D::legend3D(x = "topright", bty = "n", legend = c("Histogram", "true", "fitted"),
  #                  col = c("grey", "red", "blue"), lty = c(1, 1, 1), lwd = c(2, 2, 2))
  title(title)
}

ePLOT1 <- function(X, Y, params, num_bins =10, title = NULL) {
  # Remove NA values
  valid_indices <- complete.cases(X, Y)
  X <- X[valid_indices]
  Y <- Y[valid_indices]

  uniqueX <- sort(unique(X))

  # Number of bins for the histograms
  num_bins <- num_bins

  # Bin Y into intervals
  y_bins <- cut(Y, breaks = num_bins)

  hist_probs <- table(X, y_bins)/sum(table(X, y_bins))
  hist_probs <- as.matrix(hist_probs)
  #
  # Create x and y meshgrid for ribbon3D plot
  x <- as.vector(uniqueX)
  y <- seq(min(Y), max(Y), length.out = num_bins)

  # Fit the bivariate Poisson-Gamma distribution to the data
  # mle_result <- mleEst(X, Y, params_init = rep(0.5, 5))$params
  # m <- mle_result$params

  zz <- matrix(0, nrow = length(x), ncol = length(y))
  # Compute the values for the function
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      zz[i, j] <- dBPGC(x[i], y[j], params)
    }
  }

  zz <- zz / sum(zz) * sum(hist_probs)

  col_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))
  col <- col_pal(num_bins)

  plot3D::hist3D(x = x, y = y, z = hist_probs,
                 phi = 10, theta = 120,
                 col = 'grey', NAcol = "white", border = 'black',
                 xlab = "X", ylab = "Y", zlab = " ",
                 zlim = c(0, max(hist_probs, na.rm = TRUE, zz)),
                 add = FALSE, plot = TRUE, ticktype = 'detailed', along = 'y',
                 space = c(0.5, 0), curtain = TRUE)


  for (i in 1:length(x)) {
    plot3D::lines3D(x = rep(x[i], length(y)), y = y, z = zz[i,], add = TRUE, col = 'blue', lwd = 2)
  }

  title(title)
}


#
#
# Example contingency table (replace with your actual data)
O <- table(X, y_bins)
# rownames(O) <- 0:6
# colnames(O) <- interval_means

# Calculate expected frequencies assuming independence
E <- outer(rowSums(O), colSums(O)) / sum(O)

# Calculate sqrt(O) and sqrt(E)
sqrt_O <- sqrt(O)
sqrt_E <- sqrt(E)

# Calculate squared differences
diff_squared <- (sqrt_O - sqrt_E)^2

# Calculate Freeman-Tukey statistic T^2
T2 <- 4 * sum(diff_squared)

# Degrees of freedom
r <- nrow(O)
c <- ncol(O)
df <- (r - 1) * (c - 1)

# Print Freeman-Tukey statistic and degrees of freedom
cat("Freeman-Tukey statistic T^2:", T2, "\n")
cat("Degrees of freedom:", df, "\n")

# Compare T^2 to chi-square distribution
p_value <- pchisq(T2, df, lower.tail = FALSE)
cat("Chi-square p-value:", p_value, "\n")
