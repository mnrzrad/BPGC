#' Plot the Bivariate Poisson-Gamma Distribution and Marginal Densities
#'
#' Generates a 3D plot of the bivariate Poisson-Gamma distribution along with its marginal densities.
#'
#' @param X Numeric vector representing the range of values for the Poisson-distributed variable.
#' @param Y Numeric vector representing the range of values for the Gamma-distributed variable.
#' @param params A numeric vector of parameters: \code{c(m10, m01, m11, m02, m12)}.
#' @details
#' This function creates a 3D surface plot of the bivariate Poisson-Gamma distribution using the provided parameters.
#' It also includes the marginal densities for X and Y.
#' @importFrom plot3D persp3D
#' @examples
#' # Example usage:
#' X <- seq(0, 5, by = 1)
#' Y <- seq(1e-6, 3, length.out = 100)
#' params <- c(1,1,1,1,1)
#' PLOT(X, Y, params)
#'
#' @export
PLOT <- function(X, Y, params) {
  m10 <- params[1]
  m01 <- params[2]
  m11 <- params[3]
  m02 <- params[4]
  m12 <- params[5]

  # Define the main function f
  f <- function(x, y, m) {
    c <- calC(m)
    exp(c + m[1]*x - m[2]*y - m[3]*x*y + m[4]*log(y) + m[5]*x*log(y)) / (factorial(x) * y)
  }

  # Marginal density function for x
  h <- function(x, m) {
    c <- calC(m)
    gamma_val <- gamma(m[4] + m[5] * x)
    exp_val <- exp(c + m[1] * x)
    factorial_val <- factorial(x)
    denominator_val <- (m[2] + m[3] * x)^(m[4] + m[5] * x)
    return(gamma_val * exp_val / (factorial_val * denominator_val))
  }

  # Marginal density function for y
  g <- function(y, m) {
    c <- calC(m)
    exp_val <- exp(exp(m[1] - m[3] * y) * y^m[5] + c - m[2] * y)
    return(y^(m[4] - 1) * exp_val)
  }

  # Set the range for x and y
  upper_x <- max(X)
  upper_y <- max(Y)

  # Create an empty matrix to hold the z values
  z <- matrix(0, nrow = length(Y), ncol = length(X))

  # Compute the values for the function
  for (i in 1:length(X)) {
    for (j in 1:length(Y)) {
      z[j, i] <- f(X[i], Y[j], params)
    }
  }

  zl <- c(0, 5*max(z))

  # Transpose the z matrix to match X and Y lengths
  z <- t(z)

  trmat <- plot3D::persp3D(X, Y, z, theta = 120, zlim = zl,
                   box = TRUE,
                   shade = 0,
                   col = "white", border = NA,
                   xlab = 'X', ylab = 'Y', zlab = 'f(X,Y)',
                   ticktype = 'detailed',
                   xlab.font = 2, ylab.font = 2, zlab.font = 2, # Font size
                   xlab.cex = 1.5, ylab.cex = 1.5, zlab.cex = 1.5, # Font size
                   cex.lab = 1.5, cex.main = 2 # Font size for axis labels and title
  )

  # Add the curves for each fixed x without connecting lines between them
  for (i in 1:length(x)) {
    lines(trans3d(rep(x[i], length(y)), y, z[i, ], pmat = trmat), col = 'red')
  }

  # Add marginal density for x
  points(trans3d(x, rep(0, length(x)), h(x, m), pmat = trmat), lwd = 2, col = 'blue')

  # Add grid lines for y
  for (i in seq(1e-6, upper_y, length.out = 5)) {
    lines(trans3d(rep(0, 2), c(i, i), zl, pmat = trmat), col = 'grey')
  }
  for (i in seq(0, zl[2], length = 7)) {
    lines(trans3d(rep(0, 2), c(0, upper_y), c(i, i), pmat = trmat), col = 'grey')
  }

  # Add grid lines for x
  for (i in seq(0, upper_x, by = 1)) {
    lines(trans3d(c(i, i), rep(0, 2), zl, pmat = trmat), col = 'grey')
  }

  # Add grid lines for the y wall (x = 0)
  for (i in seq(0, upper_x, by = 1)) {
    lines(trans3d(c(i, i), c(0, 0), zl, pmat = trmat), col = 'grey')
  }
  for (i in seq(0, zl[2], length = 7)) {
    lines(trans3d(c(0, upper_x), c(0, 0), c(i, i), pmat = trmat), col = 'grey')
  }

  # Add grid lines for the x wall (y = 0)
  for (i in seq(0, upper_y, length.out = 5)) {
    lines(trans3d(rep(0, 2), c(i, i), zl, pmat = trmat), col = 'grey')
  }
  for (i in seq(0, zl[2], length = 7)) {
    lines(trans3d(c(0, upper_x), rep(0, 2), c(i, i), pmat = trmat), col = 'grey')
  }

  # Add marginal density for y
  lines(trans3d(rep(0, length(y)), y, g(y, m), pmat = trmat), lwd = 2, col = 'blue')

  # Annotate the plot
  x_text_pos <- max(x) * 0.85
  y_text_pos <- max(y) * 0.85
  text(trans3d(x_text_pos, 0, 1, pmat = trmat), expression(f[X](x)), col = "blue", cex = 1.5, pos = 3)
  text(trans3d(0, y_text_pos, 1, pmat = trmat), expression(f[Y](y)), col = "blue", cex = 1.5, pos = 3)

  # Add title
  # mtext("m = (1,1,1,1,1)", side = 3, line = 1, cex = 2)

}


