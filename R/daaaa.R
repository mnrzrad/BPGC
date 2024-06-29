# data <- read_csv("C:/Users/minan/Downloads/en_climate_summaries_All_08-2023.csv")
# X <- data$Pd
# Y <- data$P
# valid_indices <- complete.cases(X, Y); X <- X[valid_indices]; Y <- Y[valid_indices]
# positions_to_remove <- which(Y == 0) #which(X == 0 | Y == 0);
# X_filtered <- X[-positions_to_remove];
# Y_filtered <- Y[-positions_to_remove]
# estimated_params <-mleEst(X_filtered,Y_filtered)$params
# set.seed(42)
# theoretical_data <- rBPGC(estimated_params, points = length(X))
# X_theoretical <- theoretical_data$x
# Y_theoretical <- theoretical_data$y
# S1 <- cbind(X_filtered,Y_filtered)
# S2 <- cbind(X_theoretical, round(Y_theoretical,1))
# fasano.franceschini.test(S1, S2)
# ePLOT1(X_filtered,Y_filtered, estimated_params, num_bins = 30)
#
# en_climate_summaries_ON_08_2023 <- read_csv("C:/Users/minan/Downloads/en_climate_summaries_ON_08-2023.csv")
#
#
# # # Set seed for reproducibility
# # set.seed(123)
# #
# # number_of_kittens_born <- sample(0:8, 200, replace = TRUE)
# #
# # # Generate corresponding average birth weight of kittens (Y)
# # # Assuming weights follow a normal distribution with mean 0.1 kg and standard deviation 0.02 kg
# # average_birth_weight_kg <- rnorm(200, mean = 0.8, sd = 0.02)
# # average_birth_weight_kg <- pmax(average_birth_weight_kg, 0.01)  # Ensure weights are positive
# # average_birth_weight_kg <- number_of_kittens_born * average_birth_weight_kg
# # # Create the dataframe
# # cat_data <- data.frame(number_of_kittens_born, average_birth_weight_kg)
# #
# # # Display the first few rows of the data
# #
# # X <- cat_data$number_of_kittens_born
# # Y <- cat_data$average_birth_weight_kg
# # valid_indices <- complete.cases(X, Y); X <- X[valid_indices]; Y <- Y[valid_indices]
# # positions_to_remove <- which(Y == 0) #which(X == 0 | Y == 0);
# # X_filtered <- X[-positions_to_remove];
# # Y_filtered <- Y[-positions_to_remove]
# # estimated_params <-mleEst(X_filtered,Y_filtered)$params
# # set.seed(42)
# # theoretical_data <- rBPGC(estimated_params, points = length(X))
# # X_theoretical <- theoretical_data$x
# # Y_theoretical <- theoretical_data$y
# # S1 <- cbind(X_filtered,Y_filtered)
# # S2 <- cbind(X_theoretical, Y_theoretical)
# # fasano.franceschini.test(S1, S2)
# # ePLOT1(X_filtered,Y_filtered, estimated_params, num_bins = 20)
#
# # Set seed for reproducibility
# set.seed(123)
#
# # Generate hypothetical data
# n_patients <- 1000
#
# # X: Number of admissions (Poisson distributed)
# number_of_admissions <- rpois(n_patients, lambda = 2)  # Mean of 2 admissions per year
#
# # Y: Length of stay (Gamma distributed)
# length_of_stay <- rgamma(n_patients, shape = 2, rate = 0.5)  # Shape = 2, rate = 0.5
#
# # Create dataframe
# hospital_data <- data.frame(number_of_admissions, length_of_stay)
#
# # Display the first few rows of the data
# print(head(hospital_data))
#
# X <- hospital_data$number_of_admissions
# Y <- hospital_data$length_of_stay
# valid_indices <- complete.cases(X, Y); X <- X[valid_indices]; Y <- Y[valid_indices]
# positions_to_remove <- which(Y == 0) #which(X == 0 | Y == 0);
# X_filtered <- X#[-positions_to_remove];
# Y_filtered <- Y#[-positions_to_remove]
# estimated_params <-mleEst(X_filtered,Y_filtered)$params
# set.seed(42)
# theoretical_data <- rBPGC(estimated_params, points = length(X))
# X_theoretical <- theoretical_data$x
# Y_theoretical <- theoretical_data$y
# S1 <- cbind(X_filtered,Y_filtered)
# S2 <- cbind(X_theoretical, Y_theoretical)
# fasano.franceschini.test(S1, S2)
# ePLOT1(X_filtered,Y_filtered, estimated_params, num_bins = 20)
#
# n_hospitals <- 100
#
# # X: Number of admissions per year (Poisson distributed)
# number_of_admissions <- rpois(n_hospitals, lambda = 5)
# shape <- 2
# scale <- 1000
# total_charges <- rgamma(n_hospitals, shape = shape, scale = scale)
# hospital_data <- data.frame(Number_of_Admissions = number_of_admissions,
#                             Total_Charges = total_charges)
# X <- hospital_data$Number_of_Admissions
# Y <- hospital_data$Total_Charges
# valid_indices <- complete.cases(X, Y); X <- X[valid_indices]; Y <- Y[valid_indices]
# positions_to_remove <- which(Y == 0) #which(X == 0 | Y == 0);
# X_filtered <- X#[-positions_to_remove];
# Y_filtered <- Y#[-positions_to_remove]
# estimated_params <-mleEst(X_filtered,Y_filtered)$params
# set.seed(42)
# theoretical_data <- rBPGC(estimated_params, points = length(X))
# X_theoretical <- theoretical_data$x
# Y_theoretical <- theoretical_data$y
# S1 <- cbind(X_filtered,Y_filtered)
# S2 <- cbind(X_theoretical, Y_theoretical)
# fasano.franceschini.test(S1, S2)
# ePLOT1(X_filtered,Y_filtered, estimated_params, num_bins = 20)
#
#
# # Set seed for reproducibility
# set.seed(123)
#
# # Generate hypothetical data
# n_days <- 1000
#
# # X: Number of patients admitted per day (Poisson distributed)
# number_of_admissions <- rpois(n_days, lambda = 10)  # Mean of 5 admissions per day
#
# # Y: Total cost of treatment per day (Gamma distributed)
# total_cost_of_treatment <- rgamma(n_days, shape = 3, rate = 0.2)  # Shape = 3, rate = 0.2
#
# # Create dataframe
# hospital_data <- data.frame(day = 1:n_days, number_of_admissions, total_cost_of_treatment)
#
# X <- hospital_data$number_of_admissions
# Y <- hospital_data$total_cost_of_treatment
# valid_indices <- complete.cases(X, Y); X <- X[valid_indices]; Y <- Y[valid_indices]
# positions_to_remove <- which(Y == 0) #which(X == 0 | Y == 0);
# X_filtered <- X#[-positions_to_remove];
# Y_filtered <- Y#[-positions_to_remove]
# estimated_params <-mleEst(X_filtered,Y_filtered)$params
#
#
# ePLOT1(X_filtered,Y_filtered, estimated_params, num_bins = 20)
#
# ePLOT1(X,Y, estimated_params, num_bins = 20)
# # Load necessary libraries
# library(dplyr)
# library(readr)
#
# # Load the dataset
# df <- read_csv("C:/Users/minan/Downloads/archive(3)/healthcare_dataset.csv")
#
# # Convert 'Date of Admission' to Date type
# df$`Date of Admission` <- as.Date(df$`Date of Admission`, format="%Y-%m-%d")
#
# # Group by 'Date of Admission' and summarize
# result <- df %>%
#   group_by(`Date of Admission`) %>%
#   summarise(
#     Admissions = n(),
#     Total_Billing_Amount = sum(`Billing Amount`, na.rm = TRUE)
#   )
#
# # Display the result
# print(result)
#
#
# X <- result$Admissions
# Y <- result$Total_Billing_Amount
# valid_indices <- complete.cases(X, Y); X <- X[valid_indices]; Y <- Y[valid_indices]
# positions_to_remove <- which(Y == 0) #which(X == 0 | Y == 0);
# X_filtered <- X#[-positions_to_remove];
# Y_filtered <- Y#[-positions_to_remove]
# estimated_params <-mleEst(X_filtered,Y_filtered)$params
# set.seed(42)
# theoretical_data <- rBPGC(estimated_params, points = 10000)
# X_theoretical <- theoretical_data$x
# Y_theoretical <- theoretical_data$y
# S1 <- cbind(X_filtered,Y_filtered)
# S2 <- cbind(X_theoretical, Y_theoretical)
# fasano.franceschini.test::fasano.franceschini.test(S1, S2)
#
# ePLOT1(X, Y, estimated_params, num_bins = 20)
#
#
# #
# #
# # Example contingency table (replace with your actual data)
# O <- table(X, y_bins)
# # rownames(O) <- 0:6
# # colnames(O) <- interval_means
#
# # Calculate expected frequencies assuming independence
# E <- outer(rowSums(O), colSums(O)) / sum(O)
#
# # Calculate sqrt(O) and sqrt(E)
# sqrt_O <- sqrt(O)
# sqrt_E <- sqrt(E)
#
# # Calculate squared differences
# diff_squared <- (sqrt_O - sqrt_E)^2
#
# # Calculate Freeman-Tukey statistic T^2
# T2 <- 4 * sum(diff_squared)
#
# # Degrees of freedom
# r <- nrow(O)
# c <- ncol(O)
# df <- (r - 1) * (c - 1)
#
# # Print Freeman-Tukey statistic and degrees of freedom
# cat("Freeman-Tukey statistic T^2:", T2, "\n")
# cat("Degrees of freedom:", df, "\n")
#
# # Compare T^2 to chi-square distribution
# p_value <- pchisq(T2, df, lower.tail = FALSE)
# cat("Chi-square p-value:", p_value, "\n")
#
