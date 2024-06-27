# Define a function to compute expected frequencies
# compute_expected <- function(x, y_lower, params, total_count) {
#   expected_values <- sapply(y_lower, function(y) {
#     sum(sapply(x, function(x_val) {
#       dBPGC(x_val, y, params)
#     }))
#   })
#   return(expected_values * total_count)
# }
#
# # Sample data
# data <- data.frame(X, Y)  # Replace X and Y with your actual data
#
# # Compute observed frequencies
# data$Y_interval <- cut(data$Y, breaks = 20)
# observed <- as.data.frame(table(data$X, data$Y_interval))
# names(observed) <- c("X", "Y_interval", "Freq")
#
# # Extract lower bounds of the intervals
# observed$Y_lower <- as.numeric(sub("\\((.+),.*", "\\1", observed$Y_interval))
#
# # Parameters for dBPGC function
# params <- m  # Replace with your actual parameters
#
# # Compute expected frequencies
# total_count <- sum(observed$Freq)
# expected <- data.frame(X = observed$X, Y_lower = observed$Y_lower,
#                        Expected = compute_expected(observed$X, observed$Y_lower, params, total_count))
#
# # Perform chi-squared test
# chisq_test <- chisq.test(observed$Freq, p = expected$Expected, rescale.p = TRUE)
# print(chisq_test)

# https://journal.r-project.org/articles/RJ-2023-067/RJ-2023-067.pdf

# params <- c(1,5,1,5,1)
# sim_data <- rBPGC(params, points = 1e4, seed = 42)
# X <- sim_data$x
# Y <- sim_data$y
#
# estimated_params <-mleEst(X,Y)$params
#
# set.seed(42)
# theoretical_data <- rBPGC(estimated_params, points = 1e4)
# X_theoretical <- theoretical_data$x
# Y_theoretical <- theoretical_data$y
#
# S3 <- cbind(X, Y)
# S4 <- cbind(X_theoretical, Y_theoretical)
#
# fasano.franceschini.test(S3, S4, seed = 2, verbose = FALSE)
# #
library(fasano.franceschini.test)
library(readr)
data <- read_csv("data.csv")
index <- sample(1:nrow(data), 10000)
data <- data[index,]
X <- data$X
Y <- data$Y

estimated_params <-mleEst(X_filtered,Y_filtered)$params

set.seed(42)
theoretical_data <- rBPGC(estimated_params, points = nrow(data))
X_theoretical <- theoretical_data$x
Y_theoretical <- theoretical_data$y

S3 <- cbind(X_filtered,Y_filtered)
S4 <- cbind(X_theoretical, Y_theoretical)

fasano.franceschini.test(S3, S4, verbose = FALSE)

ePLOT1(X_filtered,Y_filtered, estimated_params)
