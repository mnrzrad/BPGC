#' Print method for BPGC maximum likelihood estimates
#'
#' @param x An object of class \code{mlEstBPGC}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.mlEstBPGC <- function(x, ...) {

  cat("Maximum likelihood estimation for the BPGC model\n")
  cat("------------------------------------------------\n\n")

  cat("Parameter estimates:\n")
  print(x$params)

  cat("\nNegative log-likelihood:", x$negative_log_likelihood, "\n")
  cat("Convergence code:", x$convergence, "\n")

  if (!is.null(x$summary)) {
    cat("\nInferential summary:\n")
    print(x$summary, row.names = FALSE)
  }

  invisible(x)
}
