#' Fasano-Franceschini Test for goodness-of-fit of BPGC distribution
#'
#' This function performs the Fasano-Franceschini test in order to test goodness-of-fit of BPGC distrubution.
#'
#' @param x integer values (discrete variable)
#' @param y Numeric value (continuous variable)
#'
#' @importFrom fasano.franceschini.test fasano.franceschini.test
#' @return
#' The result of the Fasano-Franceschini test, which includes a test statistic and a p-value indicating the likelihood of the null hypothesis.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' params <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' sim_data <- rBPGC(params, points = 100, seed = 42)
#' X <- sim_data$x
#' Y <- sim_data$y
#' FFtest(X, Y)
#' }
#'
#' @export
#'
FFtest <- function(X, Y){
  valid_indices <- complete.cases(X, Y)
  X <- X[valid_indices]
  Y <- Y[valid_indices]

  positions_to_remove <- which(Y == 0)

  if (length(positions_to_remove) == 0) {
    X_filtered <- X
    Y_filtered <- Y
  } else {
    X_filtered <- X[-positions_to_remove]
    Y_filtered <- Y[-positions_to_remove]
  }

  estimated_params <-mlEst(X_filtered,Y_filtered)$params
  set.seed(42)
  theoretical_data <- rBPGC(estimated_params, points = 10000)
  X_theoretical <- theoretical_data$x
  Y_theoretical <- theoretical_data$y

  S1 <- cbind(X_filtered,Y_filtered)
  S2 <- cbind(X_theoretical, Y_theoretical)

  fasano.franceschini.test::fasano.franceschini.test(S1, S2)
}
