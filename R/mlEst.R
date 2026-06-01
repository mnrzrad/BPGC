#' Maximum Likelihood Estimation (MLE) Function
#'
#' Estimates the parameters of the BPGC distribution by maximum likelihood.
#'
#' @param X A numeric vector of observations for the discrete variable.
#' @param Y A numeric vector of observations for the continuous variable.
#' @param params_init Initial values for the parameters.
#' @param max_iterations Maximum number of iterations for optimization.
#' @param rel_tol Relative tolerance for convergence.
#' @param inference Logical. If TRUE, standard errors, Wald statistics and p-values
#'   are computed from a numerical Hessian of the negative log-likelihood.
#' @return A list with the optimized parameters, negative log-likelihood,
#' convergence information, and optionally inferential summaries.
#'
#' @importFrom stats constrOptim pnorm
#' @importFrom numDeriv hessian
#'
#' @examples
#' params <- c(1, 1, 0.1, 1, 0.1)
#' sim_data <- rBPGC(params, points = 1000, seed = 42)
#' X <- sim_data$x
#' Y <- sim_data$y
#' mlEst(X, Y, inference = TRUE)
#'
#' @export
mlEst <- function(X,
                  Y,
                  params_init = rep(0.5, 5),
                  max_iterations = 1e3,
                  rel_tol = 1e-8,
                  inference = FALSE) {

  negative_log_likelihood <- function(params, X, Y) {

    m10 <- params[1]
    m01 <- params[2]
    m11 <- params[3]
    m02 <- params[4]
    m12 <- params[5]

    if (m10 <= 0 || m01 <= 0 || m02 <= 0 || m11 < 0 || m12 < 0) {
      return(1e100)
    }

    c <- calC(params)

    if (!is.finite(c)) {
      return(1e100)
    }

    result <- sum(lfactorial(X) + log(Y)) -
      length(X) * c -
      m10 * sum(X) +
      m11 * sum(X * Y) +
      m01 * sum(Y) -
      m02 * sum(log(Y)) -
      m12 * sum(X * log(Y))

    return(result)
  }

  ui <- rbind(
    c(0, 1, 0, 0, 0),   # m01 > 0
    c(0, 0, 0, 1, 0),   # m02 > 0
    c(1, 0, 0, 0, 0),   # m10 > 0
    c(0, 0, 1, 0, 0),   # m11 >= 0
    c(0, 0, 0, 0, 1)    # m12 >= 0
  )

  ci <- c(0, 0, 0, 0, 0)

  control <- list(
    maxit = max_iterations,
    reltol = rel_tol
  )

  opt_result <- tryCatch({
    stats::constrOptim(
      theta = params_init,
      f = negative_log_likelihood,
      grad = NULL,
      ui = ui,
      ci = ci,
      control = control,
      X = X,
      Y = Y
    )
  }, error = function(e) {
    list(
      par = rep(NA_real_, 5),
      value = Inf,
      convergence = 1,
      message = as.character(e)
    )
  })

  output <- list(
    params = opt_result$par,
    negative_log_likelihood = opt_result$value,
    convergence = opt_result$convergence,
    message = opt_result$message
  )

  if (isTRUE(inference) &&
      all(is.finite(opt_result$par)) &&
      opt_result$convergence == 0) {

    theta_hat <- opt_result$par
    param_names <- c("m10", "m01", "m11", "m02", "m12")
    names(theta_hat) <- param_names

    nll_eta <- function(eta, X, Y) {
      params <- exp(eta)
      negative_log_likelihood(params, X, Y)
    }

    eta_hat <- log(theta_hat)

    inference_result <- tryCatch({

      H_eta <- numDeriv::hessian(
        func = nll_eta,
        x = eta_hat,
        X = X,
        Y = Y
      )

      V_eta <- solve(H_eta)

      J <- diag(theta_hat)

      V_theta <- J %*% V_eta %*% J

      se <- sqrt(diag(V_theta))
      z_value <- theta_hat / se
      p_value <- 2 * stats::pnorm(-abs(z_value))

      summary_table <- data.frame(
        Parameter = param_names,
        Estimate = as.numeric(theta_hat),
        Std_Error = as.numeric(se),
        Wald_statistic = as.numeric(z_value),
        p_value = as.numeric(p_value),
        row.names = NULL
      )

      list(
        vcov = V_theta,
        standard_errors = se,
        z_values = z_value,
        p_values = p_value,
        summary = summary_table,
        inference_method = "Numerical Hessian of the negative log-likelihood on the log-parameter scale"
      )

    }, error = function(e) {
      list(
        vcov = NULL,
        standard_errors = rep(NA_real_, 5),
        z_values = rep(NA_real_, 5),
        p_values = rep(NA_real_, 5),
        summary = NULL,
        inference_method = "Inference failed",
        inference_message = as.character(e)
      )
    })

    output <- c(output, inference_result)
  }

  class(output) <- "mlEstBPGC"

  return(output)
}
